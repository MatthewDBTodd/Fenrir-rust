use rand::Rng;
use crate::{board::Board, chess_move::{MoveType, SavedMove}, Piece, Colour, Square};

// Zobrist hashing
#[derive(Debug)]
pub struct ZobristHasher {
    // indexed by [piece][square]
    piece_square_rngs: Vec<Vec<u64>>,
    // w_kingside, w_queenside, b_kingside, b_queenside
    castle_rngs: Vec<u64>,
    black_rng: u64,
    // 1 for each file A -> H
    en_passant_rngs: Vec<u64>,
}

impl ZobristHasher {
    pub fn new() -> Self {
        let mut rng = rand::thread_rng();
        let mut piece_square_rngs: Vec<Vec<u64>> = Vec::with_capacity(6);
        for _ in [Piece::Pawn, Piece::King, Piece::Queen, Piece::Bishop, Piece::Knight, Piece::Rook] {
            let mut v: Vec<u64> = Vec::with_capacity(64);
            for _ in 0..64 {
                v.push(rng.gen::<u64>());
            }
            piece_square_rngs.push(v);
        }
        let mut castle_rngs: Vec<u64> = Vec::with_capacity(4);
        castle_rngs.push(rng.gen::<u64>());
        castle_rngs.push(rng.gen::<u64>());
        castle_rngs.push(rng.gen::<u64>());
        castle_rngs.push(rng.gen::<u64>());

        let black_rng = rng.gen::<u64>();

        let mut en_passant_rngs: Vec<u64> = Vec::with_capacity(8);
        for _ in 0..8 {
            en_passant_rngs.push(rng.gen::<u64>());
        }

        Self {
            piece_square_rngs,
            castle_rngs,
            black_rng,
            en_passant_rngs,
        }
    }

    // Called on a new board to hash the entire board from scratch
    pub fn hash_board(&self, board: &Board) -> u64 {
        let mut hash: u64 = 0;
        for piece in [Piece::Pawn, Piece::King, Piece::Queen, Piece::Bishop, Piece::Knight, Piece::Rook] {
            let piece_idx: usize = piece as usize;
            for colour in [Colour::White, Colour::Black] {
                let mut all_pieces = board.bitboard.get_colour_piece_mask(piece, colour);
                while all_pieces != 0 {
                    let one_piece = all_pieces & all_pieces.wrapping_neg();
                    all_pieces ^= one_piece;
                    let square_idx: usize = one_piece.trailing_zeros() as usize;

                    hash ^= self.piece_square_rngs[piece_idx][square_idx];
                }
            }
        }

        // castling
        let (wk, wq, bk, bq) = board.castling_rights.all_castling_rights();
        if wk {
            hash ^= self.castle_rngs[0];
        }
        if wq {
            hash ^= self.castle_rngs[1];
        }
        if bk {
            hash ^= self.castle_rngs[2];
        }
        if bq {
            hash ^= self.castle_rngs[3];
        }

        if board.turn_colour == Colour::Black {
            hash ^= self.black_rng;
        }

        if let Some(ep_sq) = board.en_passant {
            let idx = ep_sq as usize;
            let file_idx = idx % 8;
            hash ^= self.en_passant_rngs[file_idx];
        }
        hash
    }

    // called after making the move, updates the hash based on the pieces affected
    pub fn update_hash(&self, board: &Board, saved_move: &SavedMove) -> u64 {
        let mut hash = board.board_hash;

        let piece_idx = saved_move.move_.piece as usize;
        let source_sq_idx = saved_move.move_.source_sq as usize;
        let dest_sq_idx = saved_move.move_.dest_sq as usize;

        hash ^= self.piece_square_rngs[piece_idx][source_sq_idx];
        hash ^= self.piece_square_rngs[piece_idx][dest_sq_idx];

        match saved_move.move_.move_type {
            MoveType::Capture(p) => {
                let captured_piece_idx = p as usize;
                hash ^= self.piece_square_rngs[captured_piece_idx][dest_sq_idx];
            },
            MoveType::EnPassant(captured_pawn_square) => {
                let captured_piece_idx = captured_pawn_square as usize;
                hash ^= self.piece_square_rngs[Piece::Pawn as usize][captured_piece_idx];
            },
            MoveType::CastleKingSide => {
                let (rook_source_sq, rook_dest_sq) = match saved_move.move_.source_sq {
                    Square::E1 => (Square::H1, Square::F1),
                    Square::E8 => (Square::H8, Square::F8),
                    _ => panic!("Invalid source square for castling"),
                };

                let source_sq_idx = rook_source_sq as usize;
                let dest_sq_idx = rook_dest_sq as usize;
                hash ^= self.piece_square_rngs[Piece::Rook as usize][source_sq_idx];
                hash ^= self.piece_square_rngs[Piece::Rook as usize][dest_sq_idx];
            },
            MoveType::CastleQueenSide => {
                let (rook_source_sq, rook_dest_sq) = match saved_move.move_.source_sq {
                    Square::E1 => (Square::A1, Square::D1),
                    Square::E8 => (Square::A8, Square::D8),
                    _ => panic!("Invalid source square for castling"),
                };

                let source_sq_idx = rook_source_sq as usize;
                let dest_sq_idx = rook_dest_sq as usize;
                hash ^= self.piece_square_rngs[Piece::Rook as usize][source_sq_idx];
                hash ^= self.piece_square_rngs[Piece::Rook as usize][dest_sq_idx];
            },
            MoveType::MovePromotion(p) => {
                let promotion_piece_idx = p as usize;
                hash ^= self.piece_square_rngs[promotion_piece_idx][dest_sq_idx];
                // TODO: This is inefficient simply removing the xor we just did above
                hash ^= self.piece_square_rngs[piece_idx][dest_sq_idx];
            },
            MoveType::CapturePromotion(captured_p, promotion_p) => {
                let captured_piece_idx = captured_p as usize;
                hash ^= self.piece_square_rngs[captured_piece_idx][dest_sq_idx];
                let promotion_piece_idx = promotion_p as usize;
                hash ^= self.piece_square_rngs[promotion_piece_idx][dest_sq_idx];
                // TODO: This is inefficient simply removing the xor we just did above
                hash ^= self.piece_square_rngs[piece_idx][dest_sq_idx];
            },
            MoveType::ErrorMove => panic!("ErrorMove type in update_hash"),
            _ => (),
        }

        let (wk1, wq1, bk1, bq1) = saved_move.prev_castling_rights.all_castling_rights();
        let (wk2, wq2, bk2, bq2) = board.castling_rights.all_castling_rights();

        for (idx, (&a, &b)) in [wk1, wq1, bk1, bq1]
            .iter()
            .zip([wk2, wq2, bk2, bq2].iter())
            .enumerate()
        {
            if a != b {
                hash ^= self.castle_rngs[idx];
            }
        }

        if let Some(ep_sq) = saved_move.prev_en_passant {
            let idx = ep_sq as usize;
            let file_idx = idx % 8;
            hash ^= self.en_passant_rngs[file_idx];
        }

        if let Some(ep_sq) = board.en_passant {
            let idx = ep_sq as usize;
            let file_idx = idx % 8;
            hash ^= self.en_passant_rngs[file_idx];
        }

        hash ^= self.black_rng;
        hash
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chess_move::Move;
    use crate::{Square, Piece};
    use std::sync::Arc;

    #[test]
    fn test_undo_quiet() {
        let hasher = Arc::new(ZobristHasher::new());
        let mut board = Board::new(None, hasher.clone()).unwrap();
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
        let hasher = Arc::new(ZobristHasher::new());
        let mut board = Board::new(
            Some("r1bqkbnr/ppp2ppp/2np4/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4"),
            hasher.clone()
        ).unwrap();
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
        let hasher = Arc::new(ZobristHasher::new());
        let mut board = Board::new(None, hasher.clone()).unwrap();
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
        let hasher = Arc::new(ZobristHasher::new());
        let mut board = Board::new(
            Some("rnbqkbnr/pp1ppppp/8/8/2pPP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 3"),
            hasher.clone(),
        ).unwrap();
        let old_hash = board.board_hash;
        board.make_move(
            Move {
                source_sq: Square::C4,
                dest_sq: Square::D3,
                piece: Piece::Pawn,
                move_type: MoveType::EnPassant(Square::D4),
            }
        );
        board.undo_move();
        assert_eq!(old_hash, board.board_hash);
    }

    #[test]
    fn test_undo_castle_king_side() {
        let hasher = Arc::new(ZobristHasher::new());
        let mut board = Board::new(
            Some("rnbqk2r/pppp1ppp/5n2/2b1p3/4P3/3P1N2/PPP1BPPP/RNBQK2R b KQkq - 2 4"),
            hasher.clone(),
        ).unwrap();
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
        let hasher = Arc::new(ZobristHasher::new());
        let mut board = Board::new(
            Some("r1bq1rk1/ppppbppp/2n2n2/4p3/4P3/1PN5/PBPPQPPP/R3KBNR w KQ - 5 6"),
            hasher.clone()
        ).unwrap();
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
        let hasher = Arc::new(ZobristHasher::new());
        let mut board = Board::new(
            Some("rnbqkbnr/ppp1pppp/8/8/2B5/2N5/PPPPKpPP/R1BQ2NR b kq - 1 5"),
            hasher.clone(),
        ).unwrap();
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
        let hasher = Arc::new(ZobristHasher::new());
        let mut board = Board::new(
            Some("r1bqkb1r/pP2pppp/2n2n2/8/8/8/PPPP1PPP/RNBQKBNR w KQkq - 1 5"),
            hasher.clone(),
        ).unwrap();
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