use crate::Piece;
use crate::chess_move::{Move, MoveType};

// TODO: import pgn

pub enum MoveState {
    Check,
    Checkmate,
}

pub fn generate_move_notation(chess_move: &Move, legal_moves: Vec<Move>,
                          move_state: Option<MoveState>) -> String 
{
    let mut rv = String::new();
    if chess_move.move_type == MoveType::CastleKingSide {
        rv.push_str(&"O-O");
    } else if chess_move.move_type == MoveType::CastleQueenSide {
        rv.push_str(&"O-O-O");
    } else {
        rv.push_str(&piece_character(chess_move.piece));

        let ambiguous_moves: Vec<Move> = legal_moves
            .iter()
            .filter(|&m| m.dest_sq == chess_move.dest_sq && m.piece == chess_move.piece)
            .cloned()
            .collect();

        // ambiguous moves should have at least one entry: the actual move made itself
        if ambiguous_moves.is_empty() {
            panic!("Given move isn't in filtered move list");
        } else if chess_move.piece == Piece::Pawn {
            match chess_move.move_type {
                MoveType::Capture(_) | MoveType::EnPassant(_) | MoveType::CapturePromotion(_, _) => {
                    let file = chess_move.source_sq as usize % 8;
                    rv.push_str(&file_string(file));
                },
                _ => {},
            }
        } else if ambiguous_moves.len() > 1 {
            let file = chess_move.source_sq as usize % 8;
            let rank = chess_move.source_sq as usize / 8;

            let sources_on_same_file: Vec<Move> = ambiguous_moves
                .iter()
                .filter(|&m| m.source_sq as usize % 8 == file)
                .cloned()
                .collect();

            if sources_on_same_file.is_empty() {
                panic!("file filter is empty");
            } else if sources_on_same_file.len() == 1 {
                rv.push_str(&file_string(file));
            } else {
                let sources_on_same_rank: Vec<Move> = ambiguous_moves
                    .iter()
                    .filter(|&m| m.source_sq as usize / 8 == rank)
                    .cloned()
                    .collect();
                
                if sources_on_same_rank.is_empty() {
                    panic!("rank filter is empty");
                } else if sources_on_same_rank.len() == 1 {
                    rv.push_str(&rank_string(rank));
                } else {
                    rv.push_str(&file_string(file));
                    rv.push_str(&rank_string(rank));
                }
            }
        }

        match chess_move.move_type {
            MoveType::Capture(_) | MoveType::EnPassant(_) | MoveType::CapturePromotion(_, _) => {
                rv.push('x');
            },
            _ => {},
        }

        let dest_file = chess_move.dest_sq as usize % 8;
        let dest_rank = chess_move.dest_sq as usize / 8;

        rv.push_str(&file_string(dest_file));
        rv.push_str(&rank_string(dest_rank));

        match chess_move.move_type {
            MoveType::MovePromotion(piece) | MoveType::CapturePromotion(_, piece) => {
                rv.push('=');
                rv.push_str(&piece_character(piece));
            },
            _ => {},
        }
    }

    if move_state.is_some() {
        match move_state.unwrap() {
            MoveState::Check => rv.push('+'),
            MoveState::Checkmate => rv.push('#'),
        }
    }
    rv
}

pub fn file_string(file: usize) -> String {
    match file {
        0 => "a".to_string(),
        1 => "b".to_string(),
        2 => "c".to_string(),
        3 => "d".to_string(),
        4 => "e".to_string(),
        5 => "f".to_string(),
        6 => "g".to_string(),
        7 => "h".to_string(),
        _ => panic!("Invalid file"),
    }
}

pub fn rank_string(rank: usize) -> String {
    match rank {
        0 => "1".to_string(),
        1 => "2".to_string(),
        2 => "3".to_string(),
        3 => "4".to_string(),
        4 => "5".to_string(),
        5 => "6".to_string(),
        6 => "7".to_string(),
        7 => "8".to_string(),
        _ => panic!("Invalid rank"),
    }
}

fn piece_character(piece: Piece) -> String {
    match piece {
        Piece::Pawn => String::new(),
        Piece::Knight => String::from("N"),
        Piece::Bishop => String::from("B"),
        Piece::Rook => String::from("R"),
        Piece::Queen => String::from("Q"),
        Piece::King => String::from("K"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Square;

    // For all these tests we don't pass in all the actual legal moves like we 
    // would normally, just ambiguous moves so we can just hardcode them without
    // having to generate all the legal moves
    #[test]
    fn test_quiet_moves() {
        // rnbqkbnr/ppp2ppp/3p4/8/3pP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 0 4

        let chess_move = Move {
            source_sq: Square::B2,
            dest_sq: Square::B4,
            piece: Piece::Pawn,
            move_type: MoveType::DoublePawnPush,
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "b4";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::B1,
            dest_sq: Square::C3,
            piece: Piece::Knight,
            move_type: MoveType::Quiet,
        };
        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::C2,
                dest_sq: Square::C3,
                piece: Piece::Pawn,
                move_type: MoveType::Quiet,
            }
        ];
        let expected = "Nc3";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::C1,
            dest_sq: Square::F4,
            piece: Piece::Bishop,
            move_type: MoveType::Quiet,
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "Bf4";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::H1,
            dest_sq: Square::G1,
            piece: Piece::Rook,
            move_type: MoveType::Quiet,
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::F3,
                dest_sq: Square::G1,
                piece: Piece::Knight,
                move_type: MoveType::Quiet,
            }
        ];
        let expected = "Rg1";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::D1,
            dest_sq: Square::D3,
            piece: Piece::Queen,
            move_type: MoveType::Quiet,
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::F1,
                dest_sq: Square::D3,
                piece: Piece::Bishop,
                move_type: MoveType::Quiet,
            }
        ];
        let expected = "Qd3";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::E1,
            dest_sq: Square::E2,
            piece: Piece::King,
            move_type: MoveType::Quiet,
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::F1,
                dest_sq: Square::E2,
                piece: Piece::Bishop,
                move_type: MoveType::Quiet,
            },
            Move {
                source_sq: Square::D1,
                dest_sq: Square::E2,
                piece: Piece::Queen,
                move_type: MoveType::Quiet,
            }
        ];
        let expected = "Ke2";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_captures() {
        // 1B2r3/8/Pb1p2q1/4P3/1n5P/4N2k/2B5/7K b - - 0 1

        let chess_move = Move {
            source_sq: Square::D6,
            dest_sq: Square::E5,
            piece: Piece::Pawn,
            move_type: MoveType::Capture(Piece::Pawn),
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::E8,
                dest_sq: Square::E5,
                piece: Piece::Rook,
                move_type: MoveType::Capture(Piece::Pawn),
            }
        ];
        let expected = "dxe5";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::B4,
            dest_sq: Square::A6,
            piece: Piece::Knight,
            move_type: MoveType::Capture(Piece::Pawn),
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "Nxa6";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::B6,
            dest_sq: Square::E3,
            piece: Piece::Bishop,
            move_type: MoveType::Capture(Piece::Knight),
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "Bxe3";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::E8,
            dest_sq: Square::E5,
            piece: Piece::Rook,
            move_type: MoveType::Capture(Piece::Pawn),
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "Rxe5";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::G6,
            dest_sq: Square::C2,
            piece: Piece::Queen,
            move_type: MoveType::Capture(Piece::Bishop),
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::B4,
                dest_sq: Square::C2,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Bishop),
            }
        ];
        let expected = "Qxc2";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::H3,
            dest_sq: Square::H4,
            piece: Piece::King,
            move_type: MoveType::Capture(Piece::Pawn),
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "Kxh4";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_castling() {
        // r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4

        let chess_move = Move {
            source_sq: Square::E1,
            dest_sq: Square::G1,
            piece: Piece::King,
            move_type: MoveType::CastleKingSide,
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::H1,
                dest_sq: Square::G1,
                piece: Piece::Rook,
                move_type: MoveType::Quiet,
            },
            Move {
                source_sq: Square::F3,
                dest_sq: Square::G1,
                piece: Piece::Knight,
                move_type: MoveType::Quiet,
            },
        ];
        let expected = "O-O";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        // r3kb1r/pppbqppp/2np1n2/1B2p3/4P3/2NP1N1P/PPP2PP1/R1BQ1RK1 b kq - 0 7

        let chess_move = Move {
            source_sq: Square::E8,
            dest_sq: Square::C8,
            piece: Piece::King,
            move_type: MoveType::CastleQueenSide,
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::D7,
                dest_sq: Square::C8,
                piece: Piece::Bishop,
                move_type: MoveType::Quiet,
            }
        ];
        let expected = "O-O-O";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_promotions() {
        // 3r3r/4PPP1/8/4k3/8/8/8/K7 w - - 0 1

        let chess_move = Move {
            source_sq: Square::F7,
            dest_sq: Square::F8,
            piece: Piece::Pawn,
            move_type: MoveType::MovePromotion(Piece::Queen),
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "f8=Q";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::E7,
            dest_sq: Square::E8,
            piece: Piece::Pawn,
            move_type: MoveType::MovePromotion(Piece::Rook),
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "e8=R+";
        let actual = generate_move_notation(&chess_move, moves, Some(MoveState::Check));
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::G7,
            dest_sq: Square::H8,
            piece: Piece::Pawn,
            move_type: MoveType::CapturePromotion(Piece::Rook, Piece::Bishop),
        };
        let expected = "gxh8=B+";

        let moves = vec![
            chess_move.clone(),
        ];
        let actual = generate_move_notation(&chess_move, moves, Some(MoveState::Check));
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::E7,
            dest_sq: Square::D8,
            piece: Piece::Pawn,
            move_type: MoveType::CapturePromotion(Piece::Rook, Piece::Knight),
        };
        let expected = "exd8=N";

        let moves = vec![
            chess_move.clone(),
        ];
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        // 1n4k1/P1PR4/8/8/8/8/8/2K5 w - - 0 1

        let chess_move = Move {
            source_sq: Square::A7,
            dest_sq: Square::B8,
            piece: Piece::Pawn,
            move_type: MoveType::CapturePromotion(Piece::Knight, Piece::Queen),
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::C7,
                dest_sq: Square::B8,
                piece: Piece::Pawn,
                move_type: MoveType::CapturePromotion(Piece::Knight, Piece::Queen),
            }
        ];
        let expected = "axb8=Q#";
        let actual = generate_move_notation(&chess_move, moves, Some(MoveState::Checkmate));
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::C7,
            dest_sq: Square::C8,
            piece: Piece::Pawn,
            move_type: MoveType::MovePromotion(Piece::Rook),
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "c8=R#";
        let actual = generate_move_notation(&chess_move, moves, Some(MoveState::Checkmate));
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_en_passant() {
        // rnbqkbnr/ppp1pppp/8/8/3pP3/PP6/2PP1PPP/RNBQKBNR b KQkq e3 0 3

        let chess_move = Move {
            source_sq: Square::D4,
            dest_sq: Square::E3,
            piece: Piece::Pawn,
            move_type: MoveType::EnPassant(Square::E4),
        };

        let moves = vec![
            chess_move.clone(),
        ];
        let expected = "dxe3";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_ambiguous_moves() {
        // 3k4/8/8/4r3/2N5/5N2/8/3K4 w - - 0 1

        let chess_move = Move {
            source_sq: Square::F3,
            dest_sq: Square::D2,
            piece: Piece::Knight,
            move_type: MoveType::Quiet,
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::C4,
                dest_sq: Square::D2,
                piece: Piece::Knight,
                move_type: MoveType::Quiet,
            }
        ];
        let expected = "Nfd2";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::C4,
            dest_sq: Square::E5,
            piece: Piece::Knight,
            move_type: MoveType::Capture(Piece::Rook),
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::F3,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Rook),
            }
        ];
        let expected = "Ncxe5";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        // k7/5N2/8/4r3/8/5N2/8/3K4 w - - 0 1

        let chess_move = Move {
            source_sq: Square::F7,
            dest_sq: Square::G5,
            piece: Piece::Knight,
            move_type: MoveType::Quiet,
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::F3,
                dest_sq: Square::G5,
                piece: Piece::Knight,
                move_type: MoveType::Quiet,
            }
        ];
        let expected = "N7g5";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::F3,
            dest_sq: Square::E5,
            piece: Piece::Knight,
            move_type: MoveType::Capture(Piece::Rook),
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::F7,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Rook),
            }
        ];
        let expected = "N3xe5";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        // kr5r/8/8/7N/8/7r/8/3K4 b - - 0 1

        let chess_move = Move {
            source_sq: Square::B8,
            dest_sq: Square::D8,
            piece: Piece::Rook,
            move_type: MoveType::Quiet,
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::H8,
                dest_sq: Square::D8,
                piece: Piece::Rook,
                move_type: MoveType::Quiet,
            }
        ];

        let expected = "Rbd8+";
        let actual = generate_move_notation(&chess_move, moves, Some(MoveState::Check));
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::H3,
            dest_sq: Square::H5,
            piece: Piece::Rook,
            move_type: MoveType::Capture(Piece::Knight),
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::H8,
                dest_sq: Square::H5,
                piece: Piece::Rook,
                move_type: MoveType::Capture(Piece::Knight)
            }
        ];
        let expected = "R3xh5";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);

        // 4k3/3N1N2/2N3N1/4p3/2N3N1/3N1N2/8/3K4 w - - 0 1

        let chess_move = Move {
            source_sq: Square::F7,
            dest_sq: Square::E5,
            piece: Piece::Knight,
            move_type: MoveType::Capture(Piece::Pawn),
        };

        let moves = vec![
            chess_move.clone(),
            Move {
                source_sq: Square::G6,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Pawn),
            },
            Move {
                source_sq: Square::G4,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Pawn),
            },
            Move {
                source_sq: Square::F3,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Pawn),
            },
            Move {
                source_sq: Square::D3,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Pawn),
            },
            Move {
                source_sq: Square::C4,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Pawn),
            },
            Move {
                source_sq: Square::C6,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Pawn),
            },
            Move {
                source_sq: Square::D7,
                dest_sq: Square::E5,
                piece: Piece::Knight,
                move_type: MoveType::Capture(Piece::Pawn),
            },
        ];

        let expected = "Nf7xe5";
        let actual = generate_move_notation(&chess_move, moves, None);
        assert_eq!(expected, actual);
    }
}