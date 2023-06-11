use crate::{board::Board, chess_move::Move, chess_move::MoveType};

// Zobrist hashing
pub struct ZobristHasher {
    // indexed by [piece][square]
    piece_square_rngs: Vec<Vec<u64>>,
    // w_kingside, w_queenside, b_kingside, b_queenside
    castle_rngs: Vec<u64>,
    black_rng: Vec<u64>,
    // 1 for each file A -> H
    en_passant_rngs: Vec<u64>,
}

impl ZobristHasher {
    pub fn new() -> Self {

    }

    pub fn hash_board(&self, board: &Board) -> u64 {

    }

    // called after making the move
    pub fn update_hash(&self, board: &Board, chess_move: Move) -> u64 {

    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Square, Piece};

    #[test]
    fn test_undo_quiet() {
        let mut board = Board::new(None).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::G1,
                dest_sq: Square::F3,
                piece: Piece::Knight,
                move_type: MoveType::Quiet,
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }

    #[test]
    fn test_undo_capture() {
        let mut board = Board::new(Some("r1bqkbnr/ppp2ppp/2np4/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4")).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::B5,
                dest_sq: Square::C6,
                piece: Piece::Bishop,
                move_type: MoveType::Capture(Piece::Knight),
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }

    #[test]
    fn test_undo_double_pawn_push() {
        let mut board = Board::new(None).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::E2,
                dest_sq: Square::E4,
                piece: Piece::Pawn,
                move_type: MoveType::DoublePawnPush,
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }

    #[test]
    fn test_undo_en_passant() {
        let mut board = Board::new(Some("rnbqkbnr/pp1ppppp/8/8/2pPP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 3")).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::C4,
                dest_sq: Square::D3,
                piece: Piece::Pawn,
                move_type: MoveType::EnPassant,
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }

    #[test]
    fn test_undo_castle_king_side() {
        let mut board = Board::new(Some("rnbqk2r/pppp1ppp/5n2/2b1p3/4P3/3P1N2/PPP1BPPP/RNBQK2R b KQkq - 2 4")).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::E8,
                dest_sq: Square::G8,
                piece: Piece::King,
                move_type: MoveType::CastleKingSide,
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }

    #[test]
    fn test_undo_castle_queen_side() {
        let mut board = Board::new(Some("r1bq1rk1/ppppbppp/2n2n2/4p3/4P3/1PN5/PBPPQPPP/R3KBNR w KQ - 5 6")).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::E1,
                dest_sq: Square::C1,
                piece: Piece::King,
                move_type: MoveType::CastleQueenSide,
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }

    #[test]
    fn test_undo_move_promotion() {
        let mut board = Board::new(Some("rnbqkbnr/ppp1pppp/8/8/2B5/2N5/PPPPKpPP/R1BQ2NR b kq - 1 5")).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::F2,
                dest_sq: Square::F1,
                piece: Piece::Pawn,
                move_type: MoveType::MovePromotion(Piece::Rook),
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }

    #[test]
    fn test_undo_capture_promotion() {
        let mut board = Board::new(Some("r1bqkb1r/pP2pppp/2n2n2/8/8/8/PPPP1PPP/RNBQKBNR w KQkq - 1 5")).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::B7,
                dest_sq: Square::A8,
                piece: Piece::Pawn,
                move_type: MoveType::CapturePromotion(Piece::Rook, Piece::Queen),
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }
}