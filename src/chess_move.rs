use crate::{Square, Piece};
use crate::board::CastlingRights;

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub struct Move {
    pub source_sq: Square,
    pub dest_sq: Square,
    pub piece: Piece,
    pub move_type: MoveType,
}

#[derive(Debug, Clone, Copy)]
pub struct SavedMove {
    pub move_: Move,
    // castling rights before the move was made
    pub prev_castling_rights: CastlingRights,
    // half move num before the move was made
    pub prev_half_move_num: u16,
}

#[derive(Debug, Clone, Copy)]
pub enum MoveType {
    Quiet,
    Capture(Piece),
    DoublePawnPush,
    EnPassant,
    CastleKingSide,
    CastleQueenSide,
    MovePromotion(Piece),
    // captured piece first, then promotion piece
    CapturePromotion(Piece, Piece),
    ErrorMove,
}

impl Default for Move {
    fn default() -> Self {
        Self {
            source_sq: Square::A1,
            dest_sq: Square::A1,
            piece: Piece::Pawn,
            move_type: MoveType::ErrorMove,
        }
    }    
}
