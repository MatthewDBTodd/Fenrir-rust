use crate::{Square, Piece};
use crate::board::CastlingRights;

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub struct Move {
    source_sq: Square,
    dest_sq: Square,
    piece: Piece,
    move_type: MoveType,
    // castling rights before the move was made
    prev_castling_rights: CastlingRights,
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
    CapturePromotion(Piece),
    ErrorMove,
}

impl Default for Move {
    fn default() -> Self {
        Self {
            source_sq: Square::A1,
            dest_sq: Square::A1,
            piece: Piece::Pawn,
            move_type: MoveType::ErrorMove,
            prev_castling_rights: CastlingRights::default(),
        }
    }    
}
