use crate::{Square, Piece};
use crate::board::CastlingRights;

#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(dead_code)]
pub struct Move {
    pub source_sq: Square, // 6 bits
    pub dest_sq: Square, // 6 bits
    pub piece: Piece, // 3 bits
    pub move_type: MoveType, // 3 bits
    // 6 bits for move_type flags
    // 2 bits for ResultFlag
    // 32 bits for eval
    // 6 bits for depth-searched

}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SavedMove {
    pub move_: Move,
    // castling rights before the move was made
    pub prev_castling_rights: CastlingRights,
    // half move num before the move was made
    pub prev_half_move_num: u16,
    pub prev_en_passant: Option<Square>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MoveType {
    Quiet,
    Capture(Piece), // done
    DoublePawnPush,
    // the square the captured pawn is
    EnPassant(Square), // done
    CastleKingSide, // done
    CastleQueenSide, // done
    MovePromotion(Piece),
    // captured piece first, then promotion piece
    CapturePromotion(Piece, Piece), // done
    ErrorMove,
    // WhiteCheckmatesBlack,
    // BlackCheckmatesWhite,
    // WhiteResigns,
    // BlackResigns,
    // StalemateDraw,
    // ThreeMoveRepetitionDraw,
    // FiftyMoveRuleDraw
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
