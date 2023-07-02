use crate::transposition_table::{TranspositionTable, EntryMatch, ResultFlag};
use crate::{board::Board, attack_table::AttackTable, chess_move::Move, Colour};
use crate::shared_perft::*;
use crate::eval::{eval_position, DRAW, CHECKMATE};
use crate::engine::LegalMoves;
use std::cmp;
use std::sync::Condvar;
use std::sync::{Arc, Mutex, atomic::{AtomicBool, Ordering}};

static mut NODES_VISITED: u64 = 0;

// alpha-beta search. Returns best move with its eval
pub fn search_position(
    mut legal_moves: LegalMoves,
    stop_flag: Arc<AtomicBool>, 
    mut board: Board, 
    attack_table: Arc<AttackTable>, 
    max_depth: u32,
    tt: Arc<Mutex<TranspositionTable>>,
    cv: Arc<(Mutex<()>, Condvar)>,
) -> (Option<Move>, i32, u32) /* (move, eval, depth-reached) */ {

    let mut best_move: Move = Move::default();
    let mut best_eval: i32 = i32::MIN + 1;
    // let mut best_index = usize::MAX;
    // let mut move_list = [Move::default(); 256];
    // let mut board = board.lock().unwrap();
    // let num_moves = attack_table.generate_legal_moves(&board, board.turn_colour, &mut move_list);

    // if num_moves == 0 {
    //     return (None, get_end_condition(&board, &attack_table), 0);
    // }
    if legal_moves.num == 0 {
        return (None, 0, 0);
    }
    println!("checking {} moves...", legal_moves.num);

    // let mut prev_num_nodes: u64 = 1;

    // let mut alpha: i32 = i32::MIN + 1;
    // let mut beta: i32 = i32::MAX;
    let mut current_depth = 1;
    'outer: while current_depth <= max_depth {
        if stop_flag.load(Ordering::Relaxed) {
            break;
        }
        let mut current_best_move: Move = Move::default();
        let mut current_best_eval: i32 = i32::MIN + 1;
        let mut current_best_index = 0;

        for i in 0..legal_moves.num {
            if stop_flag.load(Ordering::Relaxed) {
                break 'outer;
            }
            board.make_move(legal_moves.move_list[i]);
            let mut tt = tt.lock().unwrap();
            let e = negamax(
                &mut board, 
                &attack_table, 
                current_depth-1, 
                i32::MIN + 1, 
                -current_best_eval, 
                &mut tt,
                &stop_flag,
            );
            board.undo_move();
            if e.is_none() {
                break 'outer;
            }
            let e = -e.unwrap();
            println!("{:?} -> {e}", legal_moves.move_list[i]);
            if e > current_best_eval {
                current_best_move = legal_moves.move_list[i];
                current_best_eval = e;
                current_best_index = i;
            }
            if current_best_eval == CHECKMATE {
                let _guard = cv.0.lock().unwrap();
                cv.1.notify_one();
                break;
            }
        }
        best_move = current_best_move;
        best_eval = current_best_eval;
        // best_index = current_best_index;
        println!("Depth {current_depth}: {} with eval {best_eval}", move_string(&best_move));
        for i in (1..=current_best_index).rev() {
            legal_moves.move_list.swap(i-1, i);
        }

        unsafe {
            let bf = branching_factor(NODES_VISITED, current_depth);
            println!("branching factor = {}", bf);
            // prev_num_nodes = nodes_visited;
            NODES_VISITED = 0;
        }
        current_depth += 1;
    }
    (Some(best_move), best_eval, current_depth-1)
}

fn negamax(board: &mut Board, attack_table: &AttackTable, depth: u32, mut alpha: i32,
           mut beta: i32, tt: &mut TranspositionTable, stop_flag: &AtomicBool) -> Option<i32> {

    unsafe {
        NODES_VISITED += 1;
    }

    let alpha_orig = alpha;
    {
        let (entry, found) = tt.get(board.board_hash);
        if found != EntryMatch::NoMatch {
            let entry = match found {
                EntryMatch::DepthPreferredMatch => &mut entry.depth_preferred,
                EntryMatch::AlwaysReplaceMatch => &mut entry.always_replace,
                _ => panic!("This should not happen"),
            };
            if entry.depth_searched as u32 >= depth {
                if entry.flag == ResultFlag::Exact {
                    // println!("exact match found!");
                    return Some(entry.eval);
                } else if entry.flag == ResultFlag::LowerBound {
                    // println!("lower bound found");
                    alpha = cmp::max(alpha, entry.eval);
                } else if entry.flag == ResultFlag::UpperBound {
                    // println!("upper bound found");
                    beta = cmp::min(beta, entry.eval);
                }

                if alpha >= beta {
                    return Some(entry.eval);
                }
            }
        }
    }

    // can check for threefold before checking for checkmate as you can't repeat
    // a checkmate position
    if board.is_threefold_repetition() {
        return Some(DRAW);
    }
    if depth == 0 {
        return Some(eval_position(board, attack_table));
    }

    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);
    // game is over, either checkmate or stalemate
    if num_moves == 0 {
        return Some(-get_end_condition(board, attack_table));
    }
    // we check for the fifty move rule after checking for checkmate/stalemate just incase
    // the fiftieth move is a checkmate
    if board.half_move_num >= 50 {
        return Some(DRAW);
    }

    let mut value = i32::MIN + 1;
    let mut best_idx: Option<usize> = None;
    for i in 0..num_moves {
        if stop_flag.load(Ordering::Relaxed) {
            return None;
        }
        board.make_move(move_list[i]);
        let rv = negamax(board, attack_table, depth-1, -beta, -alpha, tt, stop_flag);
        board.undo_move();
        if rv.is_none() {
            return None;
        }
        value = cmp::max(value, -rv.unwrap());
        if value == CHECKMATE {
            return Some(CHECKMATE);
        }
        // alpha = cmp::max(alpha, value);
        if value > alpha {
            alpha = value;
            best_idx = Some(i);
        }
        if alpha >= beta {
            best_idx = Some(i);
            break;
        }
    }

    let (entry, _) = tt.get(board.board_hash);
    let node2replace = if depth > entry.depth_preferred.depth_searched as u32 {
        &mut entry.depth_preferred
    } else {
        &mut entry.always_replace
    };
    node2replace.hash = board.board_hash;
    node2replace.depth_searched = depth as u8;
    node2replace.eval = value;
    node2replace.flag = if value <= alpha_orig {
        ResultFlag::UpperBound
    } else if value >= beta {
        ResultFlag::LowerBound
    } else {
        ResultFlag::Exact
    };
    node2replace.best_move = if best_idx.is_some() {
        Some(move_list[best_idx.unwrap()])
    } else {
        None
    };

    Some(value)
}

// check for checkmates/stalemates
fn get_end_condition(board: &Board, att_table: &AttackTable) -> i32 {
    if att_table.king_in_check(board) {
        match board.turn_colour {
            Colour::White => return -CHECKMATE,
            Colour::Black => return CHECKMATE,
        }
    } else {
        return DRAW;
    }
}

fn branching_factor(nodes: u64, depth: u32) -> f64 {
    (nodes as f64).powf(1.0 / depth as f64)
}
