use num_traits::FromPrimitive;
use crate::attack_table::AttackTable;  
use crate::board::Board;        
use crate::chess_move::{Move, MoveType};
use crate::{Square, Piece};

pub fn perft_debug(board: &mut Board, attack_table: &AttackTable, depth: u32, verbose: bool) -> usize {
    assert!(depth > 0, "depth = {depth}");
    let mut move_list = [Move::default(); 256];

    let legal_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);

    if depth == 1 {
        for i in 0..legal_moves {
            println!("{} 1", move_string(&move_list[i]),);
            if verbose {
                println!("{:?}", move_list[i]);
            }
        }
        if verbose {
            println!("{legal_moves} total moves");
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

    let legal_moves = attack_table.generate_legal_moves(board, board.turn_colour, &mut move_list);

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

pub fn move_string(chess_move: &Move) -> String {
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
    let promotion_string = match chess_move.move_type {
        MoveType::MovePromotion(p) | MoveType::CapturePromotion(_, p) => {
            match p {
                Piece::Queen => "q",
                Piece::Rook => "r",
                Piece::Bishop => "b",
                Piece::Knight => "n",
                _ => panic!("Invalid promotion piece"),
            }
        },
        _ => "",
    };
    format!("{}{}{}", squares[chess_move.source_sq as usize], squares[chess_move.dest_sq as usize],
        promotion_string)
}

fn string_to_square(square: &str) -> Result<Square, &'static str> {
    assert!(square.len() == 2);
    let file = square.chars().nth(0).unwrap();
    if file < 'a' || file > 'h' {
        return Err("Invalid file");
    }
    let file = file as u32 - 'a' as u32;
    let rank = square.chars().nth(1).unwrap().to_digit(10).ok_or("Non digit rank").unwrap();
    if !(1..=8).contains(&rank) {
        return Err("Invalid rank");
    }
    let square_idx = ((rank-1) * 8) + file;

    FromPrimitive::from_u32(square_idx).ok_or("Couldn't convert to square")

}

fn char_to_piece(piece: char) -> Result<Piece, &'static str> {
    match piece {
        'q' => Ok(Piece::Queen),
        'r' => Ok(Piece::Rook),
        'b' => Ok(Piece::Bishop),
        'n' => Ok(Piece::Knight),
        _ => Err("Invalid promotion piece"),
    }
}

pub fn string_to_move(chess_move: &str, board: &Board) -> Result<Move, &'static str> {
    if chess_move.len() != 4 && chess_move.len() != 5 {
        return Err("Invalid length");
    }
    let source_sq = string_to_square(&chess_move[..2])?;
    let dest_sq = string_to_square(&chess_move[2..4])?;
    let piece = board.bitboard.get_piece_type_for_square(source_sq)
        .ok_or("No piece on source square")?;
    let captured_piece = board.bitboard.get_piece_type_for_square(dest_sq);
    let move_type: MoveType = if chess_move.len() == 5 {
        assert!(piece == Piece::Pawn);
        let promotion_piece = char_to_piece(chess_move.chars().nth(4).unwrap())?;
        if captured_piece.is_some() {
            MoveType::CapturePromotion(captured_piece.unwrap(), promotion_piece)
        } else {
            MoveType::MovePromotion(promotion_piece)
        }
    } else if captured_piece.is_some() {
        if board.bitboard.square_empty(dest_sq) {
            assert!(captured_piece.unwrap() == Piece::Pawn);
            MoveType::EnPassant
        } else {
            MoveType::Capture(captured_piece.unwrap())
        }
    } else if piece == Piece::Pawn {
        let source_mask: u64 = 1 << source_sq as u32;
        let dest_mask: u64 = 1 << dest_sq as u32;
        if (source_mask & 0xFF00 != 0 && dest_mask & 0xFF000000 != 0) ||
            (source_mask & 0xFF000000000000 != 0 && dest_mask & 0xFF00000000 != 0)  {

            MoveType::DoublePawnPush
        } else {
            MoveType::Quiet
        }
    } else if piece == Piece::King {
        if (source_sq == Square::E1 && dest_sq == Square::G1) {
            MoveType::CastleKingSide
        } else if (source_sq == Square::E1 && dest_sq == Square::C1) {
            MoveType::CastleQueenSide
        } else if (source_sq == Square::E8 && dest_sq == Square::G8) {
            MoveType::CastleKingSide
        } else if (source_sq == Square::E8 && dest_sq == Square::C8) {
            MoveType::CastleQueenSide
        } else {
            MoveType::Quiet
        }
    } else {
        MoveType::Quiet
    };
    Ok(Move {
        source_sq,
        dest_sq,
        piece,
        move_type,
    })
}
