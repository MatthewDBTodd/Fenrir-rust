use crate::transposition_table::{TranspositionTable, EntryMatch, ResultFlag};
use crate::{board::Board, attack_table::AttackTable, chess_move::Move, Colour, Piece};
use crate::{shared_perft::*, king};
use std::cmp;

pub const CHECKMATE: i32 = 100_000;
pub const DRAW: i32 = 0;

static mut nodes_visited: u64 = 0;

// alpha-beta search. Returns best move with its eval
pub fn search_position(
    board: &mut Board, attack_table: &AttackTable, depth: u32, tt: &mut TranspositionTable,
) -> (Move, i32) {

    let mut best_move: Move = Move::default();
    let mut best_eval: i32 = i32::MIN + 1;
    // let mut best_index = usize::MAX;
    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);
    println!("checking {} moves...", num_moves);

    // let mut prev_num_nodes: u64 = 1;

    let mut alpha: i32 = i32::MIN + 1;
    let mut beta: i32 = i32::MAX;

    for current_depth in (1..=depth) {
        let mut current_best_move: Move = Move::default();
        let mut current_best_eval: i32 = i32::MIN + 1;
        let mut current_best_index = 0;

        for i in 0..num_moves {
            board.make_move(move_list[i]);
            let e = -negamax(board, attack_table, current_depth-1, i32::MIN + 1, -current_best_eval, tt);
            println!("{:?} -> {e}", move_list[i]);
            board.undo_move();
            if e > current_best_eval {
                current_best_move = move_list[i];
                current_best_eval = e;
                current_best_index = i;
            }
        }
        best_move = current_best_move;
        best_eval = current_best_eval;
        // best_index = current_best_index;
        println!("Depth {current_depth}: {} with eval {best_eval}", move_string(&best_move));
        for i in (1..=current_best_index).rev() {
            move_list.swap(i-1, i);
        }

        unsafe {
            let bf = branching_factor(nodes_visited, current_depth);
            println!("branching factor = {}", bf);
            // prev_num_nodes = nodes_visited;
            nodes_visited = 0;
        }
    }
    (best_move, best_eval)
}

fn negamax(board: &mut Board, attack_table: &AttackTable, depth: u32, mut alpha: i32,
           mut beta: i32, tt: &mut TranspositionTable) -> i32 {

    unsafe {
        nodes_visited += 1;
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
                    return entry.eval;
                } else if entry.flag == ResultFlag::LowerBound {
                    // println!("lower bound found");
                    alpha = cmp::max(alpha, entry.eval);
                } else if entry.flag == ResultFlag::UpperBound {
                    // println!("upper bound found");
                    beta = cmp::min(beta, entry.eval);
                }

                if alpha >= beta {
                    return entry.eval;
                }
            }
        }
    }
    if depth == 0 {
        return eval_position(board, attack_table);
    }
    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);
    if num_moves == 0 {
        if attack_table.king_in_check(board) {
            match board.turn_colour {
                Colour::White => return -CHECKMATE,
                Colour::Black => return CHECKMATE,
            }
        } else {
            return DRAW;
        }
    }
    let mut value = i32::MIN + 1;
    let mut best_idx: Option<usize> = None;
    for i in 0..num_moves {
        board.make_move(move_list[i]);
        value = cmp::max(value, -negamax(board, attack_table, depth-1, -beta, -alpha, tt));
        // alpha = cmp::max(alpha, value);
        if value > alpha {
            alpha = value;
            best_idx = Some(i);
        }
        board.undo_move();
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

    value
}

fn eval_position(board: &Board, attack_table: &AttackTable) -> i32 {
    // let mut move_list = [Move::default(); 256];
    // let white_legal_moves = attack_table.generate_legal_moves(board, Colour::White, &mut move_list);
    // let black_legal_moves = attack_table.generate_legal_moves(board, Colour::Black, &mut move_list);
    // println!("----------------------------------------------------");
    // println!("BOARD EVAL");
    // println!("{}", board);
    let white_rough_moves = attack_table.get_num_legal_moves(board, Colour::White);
    let black_rough_moves = attack_table.get_num_legal_moves(board, Colour::Black);
    // println!("white legal/rough = {} {}", white_legal_moves, white_rough_moves);
    // println!("black legal/rough = {} {}", black_legal_moves, black_rough_moves);
    if board.turn_colour == Colour::White && white_rough_moves == 0 {
        // hack for now, there may actually be a legal en_passant, have to check for that
        let mut move_list = [Move::default(); 256];
        let legal_moves = attack_table.generate_legal_moves(board, Colour::White, &mut move_list);
        if legal_moves == 0 && attack_table.king_in_check(board) {
            return -CHECKMATE;
        } else if legal_moves == 0 {
            return DRAW;
        }
    } else if board.turn_colour == Colour::Black && black_rough_moves == 0 {
        // hack for now, there may actually be a legal en_passant, have to check for that
        let mut move_list = [Move::default(); 256];
        let legal_moves = attack_table.generate_legal_moves(board, Colour::Black, &mut move_list);
        if legal_moves == 0 && attack_table.king_in_check(board) {
            return CHECKMATE;
        } else if legal_moves == 0 {
            return DRAW;
        }
    }
    let mut eval: i32 = 0;
    for (piece_type, weight) in [
        (Piece::Queen, 900),
        (Piece::Rook, 500),
        (Piece::Bishop, 330),
        (Piece::Knight, 320),
        (Piece::Pawn, 100),
    ] {
        let (white_count, black_count) = board.bitboard.num_pieces(piece_type);
        let val = weight * (white_count as i32 - black_count as i32);
        // println!("{:?} {} * ({} - {}) = {}", piece_type, weight, white_count, black_count, val);
        eval += val;
        // eval += weight * (white_count as i32 - black_count as i32);
    }
    // println!("\nmobility:");
    let val = 10 * (white_rough_moves as i32 - black_rough_moves as i32);
    // println!("10 * ({} - {}) = {}", white_rough_moves, black_rough_moves, val);
    eval += val;
    // eval += 10 * (white_rough_moves as i32 - black_rough_moves as i32);

    let (w_doubled, w_isolated) = pawn_eval(board, Colour::White);
    let (b_doubled, b_isolated) = pawn_eval(board, Colour::Black);
    // let pawns = 5 * ((w_doubled - b_doubled) + (w_isolated - b_isolated)) as i32;
    let pawns = 50 * ((w_doubled - b_doubled) + (w_isolated - b_isolated)) as i32;
    // println!("\npawns:");
    // println!("5 * (({} - {}) + ({} - {})) = {}", w_doubled, b_doubled, w_isolated, b_isolated, pawns);
    eval += pawns;
    eval = match board.turn_colour {
        Colour::White => 1 * eval,
        Colour::Black => -1 * eval,
    };
    // println!("\nFinal eval = {}", eval);
    eval
}

// returns doubled and isolated pawns
fn pawn_eval(board: &Board, colour: Colour) -> (u32, u32) {
    let pawns = board.bitboard.get_colour_piece_mask(Piece::Pawn, colour);
    let mut doubled_pawns: u32 = 0;
    let mut isolated_pawns: u32 = 0;
    let mut pawns_tmp = pawns;
    while pawns_tmp != 0 {
        let pawn = pawns_tmp & pawns_tmp.wrapping_neg();
        pawns_tmp ^= pawn;
        let idx = pawn.trailing_zeros() as usize;
        let pawns_on_file = pawn & FILES[idx];
        pawns_tmp = pawns_tmp & !pawns_on_file;
        let num_pawns_on_file = pawns_on_file.count_ones();
        doubled_pawns += num_pawns_on_file - 1;

        if pawns & ISOLATED[idx] == 0 {
            isolated_pawns += num_pawns_on_file;
        }
    }
    (doubled_pawns, isolated_pawns)
}

#[rustfmt::skip]
const FILES: [u64; 64] = [
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
];

const ISOLATED: [u64; 64] = [
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
];

/* 
fn alpha_beta_max(
    board: &mut Board, attack_table: &AttackTable, depth: u32, mut alpha: i32, 
    mut beta: i32) -> (Move, i32) {

    if depth == 0 {
        return (Move::default(), eval_position(board, attack_table));
    }
    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);
    if num_moves == 0 {
        return (Move::default(), -CHECKMATE);
    }
    unsafe {
        move_counter += num_moves as u64;
    }
    let mut best_eval: i32 = i32::MIN;
    let mut best_move = Move::default();
    
    // try best move so far first
    let candidate_move = candidate_move.
    if candidate_move.is_some() {
        board.make_move(candidate_move.unwrap());
        let (_, move_eval) = alpha_beta_min(board, attack_table, depth-1, alpha, beta, None);
        board.undo_move();
    }
    for i in 0..num_moves {
        // println!("alpha beta MAX: depth {depth}. Checking {:?}", move_list[i]);
        board.make_move(move_list[i]);
        let (_, move_eval) = alpha_beta_min(board, attack_table, depth-1, alpha, beta);
        board.undo_move();
        if move_eval == CHECKMATE {
            return (move_list[i], CHECKMATE);
        }
        if move_eval > best_eval {
            best_eval = move_eval;
            best_move = move_list[i];
        }
        if move_eval >= beta {
            // println!("beta pruning");
            unsafe {
                prune_counter += 1;
            }
            break;
        }
        alpha = alpha.max(move_eval);
    }
    (best_move, best_eval)
}

fn alpha_beta_min(
    board: &mut Board, attack_table: &AttackTable, depth: u32, mut alpha: i32, 
    mut beta: i32) -> (Move, i32) {

    if depth == 0 {
        return (Move::default(), eval_position(board, attack_table));
    }
    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);
    if num_moves == 0 {
        return (Move::default(), CHECKMATE);
    }
    unsafe {
        move_counter += num_moves as u64;
    }
    let mut best_eval: i32 = i32::MAX;
    let mut best_move = Move::default();
    for i in 0..num_moves {
        // println!("alpha beta MIN: depth {depth}. Checking {:?}", move_list[i]);
        board.make_move(move_list[i]);
        let (_, move_eval) = alpha_beta_max(board, attack_table, depth-1, alpha, beta);
        board.undo_move();
        if move_eval == -CHECKMATE {
            return (move_list[i], -CHECKMATE);
        }
        if move_eval < best_eval {
            best_eval = move_eval;
            best_move = move_list[i];
        }
        if move_eval <= alpha {
            // println!("alpha pruning");
            unsafe {
                prune_counter += 1;
            }
            break;
        }
        beta = beta.min(move_eval);
    }
    (best_move, best_eval)
}
*/

fn branching_factor(nodes: u64, depth: u32) -> f64 {
    (nodes as f64).powf(1.0 / depth as f64)
}
