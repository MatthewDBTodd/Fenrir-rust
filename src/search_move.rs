use crate::transposition_table::{TranspositionTable, ResultFlag, CachedSearchResult};
use crate::{board::Board, attack_table::AttackTable, chess_move::Move,};
use crate::shared_perft::*;
use crate::eval::{eval_position, DRAW, CHECKMATE};
use crate::engine::LegalMoves;
use crate::chess_move::MoveType;
use std::cmp;
use std::sync::Condvar;
use std::sync::{Arc, Mutex, atomic::{AtomicBool, AtomicU64, Ordering}};

pub struct SearchStats {
    pub negamax_nodes: AtomicU64,
    pub quiescence_nodes: AtomicU64,
    pub tt_total_hits: AtomicU64,
    pub tt_exact_hits: AtomicU64,
    pub tt_inserts: AtomicU64,
}

pub enum Counter {
    NegamaxNodes,
    QuiescenceNodes,
    TTTotalHits,
    TTExactHits,
    TTInserts,
}

impl SearchStats {
    pub fn new() -> Self {
        Self {
            negamax_nodes: AtomicU64::new(0),
            quiescence_nodes: AtomicU64::new(0),
            tt_total_hits: AtomicU64::new(0),
            tt_exact_hits: AtomicU64::new(0),
            tt_inserts: AtomicU64::new(0),
        }
    }

    pub fn inc(&self, counter: Counter) {
        match counter {
            Counter::NegamaxNodes => { 
                self.negamax_nodes.fetch_add(1, Ordering::Relaxed); 
            },
            Counter::QuiescenceNodes => { 
                self.quiescence_nodes.fetch_add(1, Ordering::Relaxed); 
            },
            Counter::TTTotalHits => {
                self.tt_total_hits.fetch_add(1, Ordering::Relaxed);
            },
            Counter::TTExactHits => {
                self.tt_exact_hits.fetch_add(1, Ordering::Relaxed);
            },
            Counter::TTInserts => {
                self.tt_inserts.fetch_add(1, Ordering::Relaxed);
            },
        }
    }
}

pub fn stable_shift_left(move_list: &mut [Move; 256], pv_move: &Move) {
    let idx = move_list.iter().position(|m| m == pv_move).unwrap();
    let temp = move_list[idx];
    for i in (1..=idx).rev() {
        move_list[i] = move_list[i-1];
    }
    move_list[0] = temp;
}


// alpha-beta search. Returns best move with its eval
pub fn search_position(
    mut legal_moves: LegalMoves,
    stop_flag: Arc<AtomicBool>, 
    mut board: Board, 
    attack_table: Arc<AttackTable>, 
    starting_depth: u32,
    max_depth: u32,
    tt: Arc<TranspositionTable>,
    cv: Arc<(Mutex<()>, Condvar)>,
    quiet: bool,
    stats: Arc<SearchStats>,
) -> (Option<Move>, i32, u32) /* (move, eval, depth-reached) */ {

    let mut best_move: Move = Move::default();
    let mut best_eval: i32 = i32::MIN + 1;

    if legal_moves.num == 0 {
        panic!("This should not have happened");
    }
    if !quiet {
        println!("checking {} moves...", legal_moves.num);
    }

    let mut current_depth = starting_depth;
    'outer: while current_depth <= max_depth {
        if stop_flag.load(Ordering::Relaxed) {
            break;
        }
        let mut current_best_move: Move = Move::default();
        let mut current_best_eval: i32 = i32::MIN + 1;
        let mut current_best_index = 0;

        let mut alpha = i32::MIN + 1;
        let mut beta = -current_best_eval;

        if let Some(rv) = tt.get(board.board_hash) {
            if rv.depth_searched as u32 >= current_depth {
                if rv.flag == ResultFlag::Exact {
                    // println!("found tt match");
                    best_eval = rv.eval;
                    best_move = rv.best_move.unwrap_or_default();
                    current_depth += 1;
                    continue;
                } else if rv.flag == ResultFlag::LowerBound {
                    alpha = cmp::max(alpha, rv.eval);
                } else if rv.flag == ResultFlag::UpperBound {
                    beta = cmp::min(beta, rv.eval);
                }
            }
            if rv.best_move.is_some() {
                stable_shift_left(&mut legal_moves.move_list, &rv.best_move.unwrap());
            }
        }

        for i in 0..legal_moves.num {
            if stop_flag.load(Ordering::Relaxed) {
                break 'outer;
            }
            board.make_move(legal_moves.move_list[i]);
            // let mut tt = tt.lock().unwrap();
            let e = negamax(
                &mut board, 
                &attack_table, 
                current_depth-1, 
                alpha,
                beta,
                // i32::MIN + 1, 
                // -current_best_eval, 
                &tt,
                &stop_flag,
                &stats,
            );
            board.undo_move();
            if e.is_none() {
                break 'outer;
            }
            let e = -e.unwrap();
            // if !quiet {
            //     println!("{:?} -> {e}", legal_moves.move_list[i]);
            // }
            if e > current_best_eval {
                current_best_move = legal_moves.move_list[i];
                current_best_eval = e;
                current_best_index = i;
            }
            if current_best_eval == CHECKMATE {
                let _guard = cv.0.lock().unwrap();
                cv.1.notify_one();
                return (Some(current_best_move), current_best_eval, current_depth-1);
            }
        }
        best_move = current_best_move;
        best_eval = current_best_eval;
        if !quiet {
            println!("Depth {current_depth}: {} with eval {best_eval}", move_string(&best_move));
        }
        for i in (1..=current_best_index).rev() {
            legal_moves.move_list.swap(i-1, i);
        }

        current_depth += 1;
    }
    (Some(best_move), best_eval, current_depth-1)
}

fn negamax(
    board: &mut Board, 
    attack_table: &AttackTable, 
    depth: u32, 
    mut alpha: i32,
    mut beta: i32, 
    tt: &Arc<TranspositionTable>,
    stop_flag: &AtomicBool,
    stats: &Arc<SearchStats>,
) -> Option<i32> 
{
    stats.inc(Counter::NegamaxNodes);
    // can check for threefold before checking for checkmate as you can't repeat
    // a checkmate position
    if board.is_threefold_repetition() {
        return Some(DRAW);
    }
    
    // might have to move this after checking for checkmate/stalemate just
    // incase the 50th move is decisive?
    // we check for the fifty move rule after checking for checkmate/stalemate just incase
    // the fiftieth move is a checkmate
    if board.half_move_num >= 50 {
        return Some(DRAW);
    }

    let alpha_orig = alpha;
    let search_result = tt.get(board.board_hash);
    if let Some(search_result) = &search_result {
        stats.inc(Counter::TTTotalHits);
        if search_result.depth_searched as u32 >= depth {
            if search_result.flag == ResultFlag::Exact {
                stats.inc(Counter::TTExactHits);
                return Some(search_result.eval);
            } else if search_result.flag == ResultFlag::LowerBound {
                alpha = cmp::max(alpha, search_result.eval);
            } else if search_result.flag == ResultFlag::UpperBound {
                beta = cmp::min(beta, search_result.eval);
            }
            if alpha >= beta {
                return Some(search_result.eval);
            }
        }
    }

    if depth == 0 {
        // return Some(eval_position(board, attack_table));
        return quiescence(board, attack_table, alpha, beta, tt, stop_flag, stats);
    }

    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);
    // game is over, either checkmate or stalemate
    if num_moves == 0 {
        return Some(get_end_condition(board, attack_table));
    }

    if let Some(search_result) = search_result {
        let pv_move = search_result.best_move;
        if pv_move.is_some() {
            stable_shift_left(&mut move_list, &pv_move.unwrap());
        }
    }

    let mut value = i32::MIN + 1;
    let mut best_idx: Option<usize> = None;
    for i in 0..num_moves {
        if stop_flag.load(Ordering::Relaxed) {
            return None;
        }
        board.make_move(move_list[i]);
        let rv = negamax(board, attack_table, depth-1, -beta, -alpha, tt, stop_flag, stats);
        board.undo_move();
        if rv.is_none() {
            return None;
        }
        value = cmp::max(value, -rv.unwrap());
        if value == CHECKMATE {
            return Some(CHECKMATE);
        }
        if value > alpha {
            alpha = value;
            best_idx = Some(i);
        }
        if alpha >= beta {
            best_idx = Some(i);
            break;
        }
    }

    let new_entry = CachedSearchResult {
        depth_searched: depth as u8,
        eval: value,
        best_move: if best_idx.is_some() {
            Some(move_list[best_idx.unwrap()])
        } else {
            None
        },
        flag: if value <= alpha_orig {
            ResultFlag::UpperBound
        } else if value >= beta {
            ResultFlag::LowerBound
        } else {
            ResultFlag::Exact
        },
    };
    tt.insert(board.board_hash, new_entry);
    stats.inc(Counter::TTInserts);

    Some(value)
}

fn quiescence(
    board: &mut Board, 
    attack_table: &AttackTable, 
    // current_depth: u32, 
    mut alpha: i32,
    beta: i32, 
    tt: &Arc<TranspositionTable>,
    stop_flag: &AtomicBool,
    stats: &Arc<SearchStats>,
) -> Option<i32>
{
    stats.inc(Counter::QuiescenceNodes);
    // can check for threefold before checking for checkmate as you can't repeat
    // a checkmate position
    if board.is_threefold_repetition() {
        return Some(DRAW);
    }

    if board.half_move_num >= 50 {
        return Some(DRAW);
    }

        /* 
    {
        if let Some(search_result) = tt.get(board.board_hash) {
            if search_result.depth_searched as u32 >= depth {
                if search_result.flag == ResultFlag::Exact {
                    return Some(search_result.eval);
                } else if search_result.flag == ResultFlag::LowerBound {
                    alpha = cmp::max(alpha, search_result.eval);
                } else if search_result.flag == ResultFlag::UpperBound {
                    beta = cmp::min(beta, search_result.eval);
                }
                if alpha >= beta {
                    return Some(search_result.eval);
                }
            }
        }
    }
    */

    let standing_pat = eval_position(&board, attack_table);
    if standing_pat >= beta {
        return Some(beta);
    } else if alpha < standing_pat {
        alpha = standing_pat;
    }


    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);

    // game is over, either checkmate or stalemate
    if num_moves == 0 {
        return Some(get_end_condition(board, attack_table));
    }

    let ignored_move_types = [
        MoveType::Quiet,
        MoveType::DoublePawnPush,
        MoveType::CastleKingSide,
        MoveType::CastleQueenSide,
    ];
    for i in 0..num_moves {
        if stop_flag.load(Ordering::Relaxed) {
            return None;
        }
        if ignored_move_types.contains(&move_list[i].move_type) {
            break;
        }
        board.make_move(move_list[i]);
        let eval = quiescence(board, attack_table, -beta, -alpha, tt, stop_flag, stats);
        if eval.is_none() {
            return None;
        }
        let eval = -eval.unwrap();
        board.undo_move();
        
        if eval >= beta {
            return Some(beta);
        }
        if eval > alpha {
            alpha = eval;
        }
    }
    Some(alpha)
}

// check for checkmates/stalemates
fn get_end_condition(board: &Board, att_table: &AttackTable) -> i32 {
    if att_table.king_in_check(board) {
        return -CHECKMATE;
    } else {
        return DRAW;
    }
}

// fn branching_factor(nodes: u64, depth: u32) -> f64 {
//     (nodes as f64).powf(1.0 / depth as f64)
// }
