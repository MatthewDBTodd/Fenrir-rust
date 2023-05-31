use crate::attack_table::AttackTable;  
use crate::board::Board;        
use crate::chess_move::Move;         

pub fn perft_debug(board: &mut Board, attack_table: &AttackTable, depth: u32, verbose: bool) -> usize {
    assert!(depth > 0, "depth = {depth}");
    let mut move_list = [Move::default(); 256];

    let legal_moves = attack_table.generate_legal_moves(board, &mut move_list);

    if depth == 1 {
        for i in 0..legal_moves {
            println!("{} 1", move_string(&move_list[i]),);
            if verbose {
                println!("{:?}", move_list[i]);
            }
        }
        return legal_moves;
    }

    let mut total_nodes: usize = 0;
    for i in 0..legal_moves {
        board.make_move(move_list[i]);
        let nodes = perft(board, attack_table, depth-1, verbose);
        total_nodes += nodes;
        println!("{} {}", move_string(&move_list[i]), nodes);
        board.undo_move();
    }
    total_nodes
}

pub fn perft(board: &mut Board, attack_table: &AttackTable, depth: u32, verbose: bool) -> usize {

    let mut move_list = [Move::default(); 256];

    let legal_moves = attack_table.generate_legal_moves(board, &mut move_list);

    if depth == 1 {
        if verbose {
            for i in 0..legal_moves {
                println!("{:?}", move_list[i]);
            }
        }
        return legal_moves;
    }

    let mut nodes: usize = 0;
    for i in 0..legal_moves {
        board.make_move(move_list[i]); 
        nodes += perft(board, attack_table, depth-1, verbose);
        board.undo_move();
    }
    nodes
}

fn move_string(chess_move: &Move) -> String {
    let squares: [&str; 64] = [
        "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
        "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
        "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
        "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
        "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
        "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
        "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
        "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8",
    ];
    format!("{}{}", squares[chess_move.source_sq as usize], squares[chess_move.dest_sq as usize])
}
