use crate::{board::Board, attack_table::AttackTable, chess_move::Move};


fn eval_position(board: &Board, attack_table: &AttackTable) -> f64 {
    let mut move_list = [Move::default(); 256];
    let white_legal_moves = attack_table.generate_legal_moves(board, &mut move_list);
    let black_legal_moves = attack_table.generate_legal_moves(board, )
}