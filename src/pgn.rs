use crate::Piece;
use crate::board::Board;
use crate::chess_move::{Move, SavedMove, MoveType};
use crate::Square;

fn generate_move_notation(board: &Board, chess_move: &Move) -> String {
    String::new()
}

fn piece_character(piece: Piece) -> Option<char> {
    match piece {
        Piece::Pawn => None,
        Piece::Knight => Some('N'),
        Piece::Bishop => Some('B'),
        Piece::Rook => Some('R'),
        Piece::Queen => Some('Q'),
        Piece::King => Some('K'),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board_hash::ZobristHasher;
    use std::sync::Arc;

    #[test]
    fn test_quiet_moves() {
        let hasher = Arc::new(ZobristHasher::new());
        let board = Board::new(Some("rnbqkbnr/ppp2ppp/3p4/8/3pP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 0 4"),
                               hasher.clone()).unwrap();
        let chess_move = Move {
            source_sq: Square::B2,
            dest_sq: Square::B4,
            piece: Piece::Pawn,
            move_type: MoveType::DoublePawnPush,
        };
        let expected = "b4";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::B1,
            dest_sq: Square::C3,
            piece: Piece::Knight,
            move_type: MoveType::Quiet,
        };
        let expected = "Nc3";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::C1,
            dest_sq: Square::F4,
            piece: Piece::Bishop,
            move_type: MoveType::Quiet,
        };
        let expected = "Bf4";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::H1,
            dest_sq: Square::G1,
            piece: Piece::Rook,
            move_type: MoveType::Quiet,
        };
        let expected = "Rg1";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::D1,
            dest_sq: Square::D3,
            piece: Piece::Queen,
            move_type: MoveType::Quiet,
        };
        let expected = "Qd3";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::E1,
            dest_sq: Square::E2,
            piece: Piece::King,
            move_type: MoveType::Quiet,
        };
        let expected = "Ke2";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_captures() {
        let hasher = Arc::new(ZobristHasher::new());
        let board = Board::new(Some("1B2r3/8/Pb1p2q1/4P3/1n5P/4N2k/2B5/7K b - - 0 1"),
                               hasher.clone()).unwrap();

        let chess_move = Move {
            source_sq: Square::D6,
            dest_sq: Square::E5,
            piece: Piece::Pawn,
            move_type: MoveType::Capture(Piece::Pawn),
        };
        let expected = "dxe5";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::B4,
            dest_sq: Square::A6,
            piece: Piece::Knight,
            move_type: MoveType::Capture(Piece::Pawn),
        };
        let expected = "Nxa6";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::B6,
            dest_sq: Square::E3,
            piece: Piece::Bishop,
            move_type: MoveType::Capture(Piece::Knight),
        };
        let expected = "Bxe3";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::E8,
            dest_sq: Square::E5,
            piece: Piece::Rook,
            move_type: MoveType::Capture(Piece::Pawn),
        };
        let expected = "Rxe5";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::G6,
            dest_sq: Square::C2,
            piece: Piece::Queen,
            move_type: MoveType::Capture(Piece::Bishop),
        };
        let expected = "Qxc2";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::H3,
            dest_sq: Square::H4,
            piece: Piece::King,
            move_type: MoveType::Capture(Piece::Pawn),
        };
        let expected = "Kxh4";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_castling() {
        let hasher = Arc::new(ZobristHasher::new());
        let board = Board::new(Some("r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4"),
                               hasher.clone()).unwrap();

        let chess_move = Move {
            source_sq: Square::E1,
            dest_sq: Square::G1,
            piece: Piece::King,
            move_type: MoveType::CastleKingSide,
        };
        let expected = "O-O";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let board = Board::new(Some("r3kb1r/pppbqppp/2np1n2/1B2p3/4P3/2NP1N1P/PPP2PP1/R1BQ1RK1 b kq - 0 7"),
                               hasher.clone()).unwrap();
        let chess_move = Move {
            source_sq: Square::E8,
            dest_sq: Square::C8,
            piece: Piece::King,
            move_type: MoveType::CastleQueenSide,
        };
        let expected = "O-O-O";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_promotions() {
        let hasher = Arc::new(ZobristHasher::new());
        let board = Board::new(Some("3r3r/4PPP1/8/4k3/8/8/8/K7 w - - 0 1"),
                               hasher.clone()).unwrap();

        let chess_move = Move {
            source_sq: Square::F7,
            dest_sq: Square::F8,
            piece: Piece::Pawn,
            move_type: MoveType::MovePromotion(Piece::Queen),
        };
        let expected = "f8=Q";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::E7,
            dest_sq: Square::E8,
            piece: Piece::Pawn,
            move_type: MoveType::MovePromotion(Piece::Rook),
        };
        let expected = "e8=R+";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::G7,
            dest_sq: Square::H8,
            piece: Piece::Pawn,
            move_type: MoveType::CapturePromotion(Piece::Rook, Piece::Bishop),
        };
        let expected = "gxh8=B+";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::E7,
            dest_sq: Square::D8,
            piece: Piece::Pawn,
            move_type: MoveType::CapturePromotion(Piece::Rook, Piece::Knight),
        };
        let expected = "exd8=N";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let board = Board::new(Some("n5k1/1P1R4/8/8/8/8/8/2K5 w - - 0 1"),
                               hasher.clone()).unwrap();
        
        let chess_move = Move {
            source_sq: Square::B7,
            dest_sq: Square::A8,
            piece: Piece::Pawn,
            move_type: MoveType::CapturePromotion(Piece::Knight, Piece::Queen),
        };
        let expected = "bxa8=Q#";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::B7,
            dest_sq: Square::B8,
            piece: Piece::Pawn,
            move_type: MoveType::MovePromotion(Piece::Rook),
        };
        let expected = "b8=R#";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_en_passant() {
        let hasher = Arc::new(ZobristHasher::new());
        let board = Board::new(Some("rnbqkbnr/ppp1pppp/8/8/3pP3/PP6/2PP1PPP/RNBQKBNR b KQkq e3 0 3"),
                               hasher.clone()).unwrap();

        let chess_move = Move {
            source_sq: Square::D4,
            dest_sq: Square::E3,
            piece: Piece::Pawn,
            move_type: MoveType::EnPassant(Square::E4),
        };
        let expected = "dxe3";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_ambiguous_moves() {
        let hasher = Arc::new(ZobristHasher::new());
        let board = Board::new(Some("3k4/8/8/4r3/2N5/5N2/8/3K4 w - - 0 1"),
                               hasher.clone()).unwrap();

        let chess_move = Move {
            source_sq: Square::F3,
            dest_sq: Square::D2,
            piece: Piece::Knight,
            move_type: MoveType::Quiet,
        };
        let expected = "Nfd2";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::C4,
            dest_sq: Square::E5,
            piece: Piece::Knight,
            move_type: MoveType::Capture(Piece::Rook),
        };
        let expected = "Ncxe5";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let hasher = Arc::new(ZobristHasher::new());
        let board = Board::new(Some("k7/5N2/8/4r3/8/5N2/8/3K4 w - - 0 1"),
                               hasher.clone()).unwrap();

        let chess_move = Move {
            source_sq: Square::F7,
            dest_sq: Square::G5,
            piece: Piece::Knight,
            move_type: MoveType::Quiet,
        };
        let expected = "N7g5";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::F3,
            dest_sq: Square::E5,
            piece: Piece::Knight,
            move_type: MoveType::Capture(Piece::Rook),
        };
        let expected = "N3xe5";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let hasher = Arc::new(ZobristHasher::new());
        let board = Board::new(Some("kr5r/8/8/7N/8/7r/8/3K4 b - - 0 1"),
                               hasher.clone()).unwrap();

        let chess_move = Move {
            source_sq: Square::B8,
            dest_sq: Square::D8,
            piece: Piece::Rook,
            move_type: MoveType::Quiet,
        };
        let expected = "Nbd8+";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);

        let chess_move = Move {
            source_sq: Square::H3,
            dest_sq: Square::H5,
            piece: Piece::Rook,
            move_type: MoveType::Capture(Piece::Knight),
        };
        let expected = "R3xh5";
        let actual = generate_move_notation(&board, &chess_move);
        assert_eq!(expected, actual);
    }
}