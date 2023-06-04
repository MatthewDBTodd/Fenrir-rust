use crate::{board::Board, attack_table::AttackTable, chess_move::Move, Colour, Piece};

pub const CHECKMATE: f64 = 100_000f64;
const WHITE_MULTIPLIER: f64 = 1.0;
const BLACK_MULTIPLIER: f64 = -1.0;

static mut move_counter: u64 = 0;
static mut prune_counter: u64 = 0;

// alpha-beta search. Returns best move with its eval
pub fn search_position(board: &mut Board, attack_table: &AttackTable, depth: u32) -> (Move, f64, u64, u64) {
    let (m, e) = match board.turn_colour {
        Colour::White => alpha_beta_max(board, attack_table, depth, f64::MIN, f64::MAX),
        Colour::Black => alpha_beta_min(board, attack_table, depth, f64::MIN, f64::MAX),
    };
    unsafe {
        let mc = move_counter;
        let pc = prune_counter;

        prune_counter = 0;
        (m, e, mc, pc)
    }
}

fn alpha_beta_max(board: &mut Board, attack_table: &AttackTable, depth: u32, mut alpha: f64, mut beta: f64) -> (Move, f64) {
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
    let mut best_eval: f64 = f64::MIN;
    let mut best_move = Move::default();
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

fn alpha_beta_min(board: &mut Board, attack_table: &AttackTable, depth: u32, mut alpha: f64, mut beta: f64) -> (Move, f64) {
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
    let mut best_eval: f64 = f64::MAX;
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

fn eval_position(board: &Board, attack_table: &AttackTable) -> f64 {
    let mut move_list = [Move::default(); 256];
    let white_legal_moves = attack_table.generate_legal_moves(board, Colour::White, &mut move_list);
    if white_legal_moves == 0 {
        return -CHECKMATE;
    }
    let black_legal_moves = attack_table.generate_legal_moves(board, Colour::Black, &mut move_list);
    if black_legal_moves == 0 {
        return CHECKMATE;
    }
    let mut eval: f64 = 0.0;
    for (piece_type, weight) in [
        (Piece::Queen, 9f64),
        (Piece::Rook, 5f64),
        (Piece::Bishop, 3f64),
        (Piece::Knight, 3f64),
        (Piece::Pawn, 1f64),
    ] {
        let (white_count, black_count) = board.bitboard.num_pieces(piece_type);
        eval += (weight * (white_count as f64 - black_count as f64));
    }
    eval += 0.1 * (white_legal_moves as f64 - black_legal_moves as f64);
    eval
}
