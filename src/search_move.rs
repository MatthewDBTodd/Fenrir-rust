use crate::{board::Board, attack_table::AttackTable, chess_move::Move, Colour, Piece};
use crate::shared_perft::*;

pub const CHECKMATE: i32 = 100_000;
const WHITE_MULTIPLIER: f64 = 1.0;
const BLACK_MULTIPLIER: f64 = -1.0;

static mut move_counter: u64 = 0;
static mut prune_counter: u64 = 0;
static mut moves_explored: u64 = 0;

// alpha-beta search. Returns best move with its eval
pub fn search_position(
    board: &mut Board, attack_table: &AttackTable, depth: u32
) -> (Move, i32, u64, u64) {

    let mut best_move: Move = Move::default();
    let mut best_eval: i32 = i32::MIN;
    let mut best_index = 0;
    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);
    unsafe {
        move_counter += num_moves as u64;
    }
    for current_depth in (1..=depth) {
        for i in 0..num_moves {
            board.make_move(move_list[i]);
            let e = negamax(board, attack_table, current_depth-1, i32::MIN, i32::MAX);
            board.undo_move();
            if e > best_eval {
                best_move = move_list[i];
                best_eval = e;
                best_index = i;
            }
        }
        println!("Depth {current_depth}: {} with eval {best_eval}", move_string(&best_move));
        for i in (1..=best_index).rev() {
            move_list.swap(i-1, i);
        }
    }
    unsafe {
        let mc = move_counter;
        let pc = prune_counter;

        prune_counter = 0;
        (best_move, best_eval, mc, pc)
    }
}

fn negamax(board: &mut Board, attack_table: &AttackTable, depth: u32, mut alpha: i32,
           beta: i32) -> i32 {

    if depth == 0 {
        return eval_position(board, attack_table);
    }
    let mut move_list = [Move::default(); 256];
    let num_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);
    unsafe {
        move_counter += num_moves as u64;
    }
    for i in 0..num_moves {
        board.make_move(move_list[i]);
        let eval = -negamax(board, attack_table, depth-1, -beta, -alpha,);
        board.undo_move();
        if eval >= beta {
            unsafe {
                prune_counter += 1;
            }
            return beta;
        }
        if eval > alpha {
            alpha = eval;
        }
    }
    alpha
}

fn eval_position(board: &Board, attack_table: &AttackTable) -> i32 {
    let mut move_list = [Move::default(); 256];
    let white_legal_moves = attack_table.generate_legal_moves(board, Colour::White, &mut move_list);
    if white_legal_moves == 0 {
        return -CHECKMATE;
    }
    let black_legal_moves = attack_table.generate_legal_moves(board, Colour::Black, &mut move_list);
    if black_legal_moves == 0 {
        return CHECKMATE;
    }
    let mut eval: i32 = 0;
    for (piece_type, weight) in [
        (Piece::Queen, 900),
        (Piece::Rook, 500),
        (Piece::Bishop, 300),
        (Piece::Knight, 300),
        (Piece::Pawn, 100),
    ] {
        let (white_count, black_count) = board.bitboard.num_pieces(piece_type);
        eval += weight * (white_count as i32 - black_count as i32);
    }
    eval += 10 * (white_legal_moves as i32 - black_legal_moves as i32);

    let (w_doubled, w_isolated) = pawn_eval(board, Colour::White);
    let (b_doubled, b_isolated) = pawn_eval(board, Colour::Black);
    let pawns = 5 * ((w_doubled - b_doubled) + (w_isolated - b_isolated)) as i32;
    eval += pawns;
    eval
}

// returns doubled and isolated pawns
fn pawn_eval(board: &Board, colour: Colour) -> (u32, u32) {
    let mut pawns = board.bitboard.get_colour_piece_mask(Piece::Pawn, colour);
    let mut doubled_pawns: u32 = 0;
    let mut isolated_pawns: u32 = 0;
    while pawns != 0 {
        let pawn = pawns & pawns.wrapping_neg();
        pawns ^= 0;
        let idx = pawn.trailing_zeros() as usize;
        let pawns_on_file = pawn & FILES[idx];
        pawns = pawns & !pawns_on_file;
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
