use crate::bitboard::*;
use crate::{Colour, Square, Piece};
use crate::chess_move::{Move, SavedMove, MoveType};

use std::fmt;

#[derive(Debug)]
#[allow(dead_code)]
pub struct Board {
    pub bitboard: BitBoard,
    // TODO: turn_colour can instead be calculated from half_move_num to save space?
    pub turn_colour: Colour,
    move_num: u16,
    half_move_num: u16,
    castling_rights: CastlingRights,
    en_passant: Option<Square>,
    move_history: Vec<SavedMove>,
    board_hash: u64,
}

const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

impl Board {
    pub fn new(fen: Option<&str>) -> Result<Board, &'static str> {
        let fen = fen.unwrap_or(STARTING_FEN);
        let parts: Vec<&str> = fen.split_ascii_whitespace().collect();
        if parts.len() != 6 {
            return Err("Error: fen string does not contain the required 6 parts");
        }
        let Ok(half_move_num) = parts[4].parse::<u16>() else {
            return Err("Error: invalid half move num in fen string");
        };

        let Ok(move_num) = parts[5].parse::<u16>() else {
            return Err("Error: invalid move num in fen string");
        };
        Ok(Board {
            bitboard: BitBoard::try_from(parts[0])?,
            turn_colour: Colour::try_from(parts[1])?,
            castling_rights: CastlingRights::try_from(parts[2])?,
            en_passant: Square::parse(parts[3])?,
            half_move_num,
            move_num,
            move_history: Vec::new(),
            board_hash: 0,
        })
    }
    
    pub fn make_move(&mut self, move_: Move) {
        let saved_castling = self.castling_rights;
        let saved_half_move_num = self.half_move_num;
        let saved_en_passant = self.en_passant;

        // TODO: capture can be optimised by only changing the piece bitboard, full occupied bitboard
        // doesn't need changing
        match move_.move_type {
            MoveType::Quiet => {
                self.make_quiet_move(self.turn_colour, move_.source_sq, move_.dest_sq, move_.piece);
                self.en_passant = None;
                self.half_move_num += 1;
            },
            MoveType::Capture(piece) => {
                self.bitboard.remove_piece(!&self.turn_colour, piece, move_.dest_sq);
                self.make_quiet_move(self.turn_colour, move_.source_sq, move_.dest_sq, move_.piece);
                self.en_passant = None;
                self.half_move_num = 0;
            },
            MoveType::DoublePawnPush => {
                self.make_quiet_move(self.turn_colour, move_.source_sq, move_.dest_sq, move_.piece);
                self.en_passant = match move_.dest_sq {
                    Square::A4 => Some(Square::A3),
                    Square::B4 => Some(Square::B3),
                    Square::C4 => Some(Square::C3),
                    Square::D4 => Some(Square::D3),
                    Square::E4 => Some(Square::E3),
                    Square::F4 => Some(Square::F3),
                    Square::G4 => Some(Square::G3),
                    Square::H4 => Some(Square::H3),
                    Square::A5 => Some(Square::A6),
                    Square::B5 => Some(Square::B6),
                    Square::C5 => Some(Square::C6),
                    Square::D5 => Some(Square::D6),
                    Square::E5 => Some(Square::E6),
                    Square::F5 => Some(Square::F6),
                    Square::G5 => Some(Square::G6),
                    Square::H5 => Some(Square::H6),
                    _ => panic!("invalid double pawn push square"),
                };
                self.half_move_num = 0;
            },
            MoveType::EnPassant => {
                debug_assert!(self.en_passant.is_some());
                self.bitboard.remove_piece(!&self.turn_colour, Piece::Pawn, self.en_passant.unwrap());
                self.make_quiet_move(self.turn_colour, move_.source_sq, move_.dest_sq, move_.piece);
                self.en_passant = None;
                self.half_move_num = 0;
            },
            MoveType::CastleKingSide => {
                debug_assert!(move_.piece == Piece::King);
                debug_assert!((self.turn_colour == Colour::White && move_.source_sq == Square::E1)
                            || self.turn_colour == Colour::Black && move_.source_sq == Square::E8);
                debug_assert!((self.turn_colour == Colour::White && move_.dest_sq == Square::G1)
                            || self.turn_colour == Colour::Black && move_.dest_sq == Square::G8);

                self.make_quiet_move(self.turn_colour, move_.source_sq, move_.dest_sq, move_.piece);
                let rook_source_sq = if self.turn_colour == Colour::White {
                    Square::H1
                } else {
                    Square::H8
                };
                let rook_dest_sq = if self.turn_colour == Colour::White {
                    Square::F1
                } else {
                    Square::F8
                };
                self.make_quiet_move(self.turn_colour, rook_source_sq, rook_dest_sq, Piece::Rook);
                let (kingside, queenside) = match self.turn_colour {
                    Colour::White => (CastlingSide::WhiteKingside, CastlingSide::WhiteQueenside),
                    Colour::Black => (CastlingSide::BlackKingside, CastlingSide::BlackQueenside),
                };
                self.castling_rights.disable(kingside);
                self.castling_rights.disable(queenside);
                self.en_passant = None;
                self.half_move_num += 1;
            },
            MoveType::CastleQueenSide => {
                debug_assert!(move_.piece == Piece::King);
                debug_assert!((self.turn_colour == Colour::White && move_.source_sq == Square::E1)
                            || self.turn_colour == Colour::Black && move_.source_sq == Square::E8);
                debug_assert!((self.turn_colour == Colour::White && move_.dest_sq == Square::C1)
                            || self.turn_colour == Colour::Black && move_.dest_sq == Square::C8);

                self.make_quiet_move(self.turn_colour, move_.source_sq, move_.dest_sq, move_.piece);
                let rook_source_sq = if self.turn_colour == Colour::White {
                    Square::A1
                } else {
                    Square::A8
                };
                let rook_dest_sq = if self.turn_colour == Colour::White {
                    Square::D1
                } else {
                    Square::D8
                };
                self.make_quiet_move(self.turn_colour, rook_source_sq, rook_dest_sq, Piece::Rook);
                let (kingside, queenside) = match self.turn_colour {
                    Colour::White => (CastlingSide::WhiteKingside, CastlingSide::WhiteQueenside),
                    Colour::Black => (CastlingSide::BlackKingside, CastlingSide::BlackQueenside),
                };
                self.castling_rights.disable(kingside);
                self.castling_rights.disable(queenside);
                self.en_passant = None;
                self.half_move_num += 1;
            },
            MoveType::MovePromotion(piece) => {
                self.bitboard.remove_piece(self.turn_colour, move_.piece, move_.source_sq);
                self.bitboard.place_piece(self.turn_colour, piece, move_.dest_sq);
                self.en_passant = None;
                self.half_move_num = 0;
            },
            MoveType::CapturePromotion(captured_piece, promotion_piece) => {
                self.bitboard.remove_piece(self.turn_colour, move_.piece, move_.source_sq);
                self.bitboard.remove_piece(!&self.turn_colour, captured_piece, move_.dest_sq);
                self.bitboard.place_piece(self.turn_colour, promotion_piece, move_.dest_sq);
                self.en_passant = None;
                self.half_move_num = 0;
            },
            _ => {
                panic!("Invalid move type");
            }
        }
        
        // If the rook moves from its source square it voids castling rights on
        // that side. Instead of doing this check we just do it for any piece
        // moving from that square
        // Same goes for moving TO the rook square, if it's a quiet move then
        // the rook isn't there anymore, hence voiding rights. If it's capturing
        // the rook then the castling rights are also voided
        if move_.source_sq == Square::A1 || move_.dest_sq == Square::A1 {
            self.castling_rights.disable(CastlingSide::WhiteQueenside);
        } else if move_.source_sq == Square::H1 || move_.dest_sq == Square::H1 {
            self.castling_rights.disable(CastlingSide::WhiteKingside);
        } else if move_.source_sq == Square::A8 || move_.dest_sq == Square::A8 {
            self.castling_rights.disable(CastlingSide::BlackQueenside);
        } else if move_.source_sq == Square::H8 || move_.dest_sq == Square::H8 {
            self.castling_rights.disable(CastlingSide::BlackKingside);
        }
        
        self.turn_colour = !&self.turn_colour;
        self.move_num = if self.turn_colour == Colour::Black {
            self.move_num + 1
        } else {
            self.move_num
        };
        self.move_history.push(SavedMove{
            move_,
            prev_castling_rights: saved_castling,
            prev_half_move_num: saved_half_move_num, 
            prev_en_passant: saved_en_passant,
        });
    }
    
    pub fn undo_move(&mut self) {
        let Some(saved_move) = self.move_history.pop() else {
            return;
        };
        match saved_move.move_.move_type {
            MoveType::Quiet => {
                self.make_quiet_move(!&self.turn_colour, saved_move.move_.dest_sq, 
                    saved_move.move_.source_sq, saved_move.move_.piece);
            },
            MoveType::Capture(piece) => {
                self.make_quiet_move(!&self.turn_colour, saved_move.move_.dest_sq,
                    saved_move.move_.source_sq, saved_move.move_.piece);
                self.bitboard.place_piece(self.turn_colour, piece, saved_move.move_.dest_sq);
            },
            MoveType::DoublePawnPush => {
                self.make_quiet_move(!&self.turn_colour, saved_move.move_.dest_sq,
                    saved_move.move_.source_sq, saved_move.move_.piece);
            },
            MoveType::EnPassant => {
                self.make_quiet_move(!&self.turn_colour, saved_move.move_.dest_sq,
                    saved_move.move_.source_sq, saved_move.move_.piece);
                self.bitboard.place_piece(self.turn_colour, Piece::Pawn, saved_move.prev_en_passant.unwrap());
            },
            MoveType::CastleKingSide => {
                self.make_quiet_move(!&self.turn_colour, saved_move.move_.dest_sq,
                    saved_move.move_.source_sq, saved_move.move_.piece);
                let rook_source_sq = if self.turn_colour == Colour::White {
                    Square::F1
                } else {
                    Square::F8
                };
                let rook_dest_sq = if self.turn_colour == Colour::White {
                    Square::H1
                } else {
                    Square::H8
                };
                self.make_quiet_move(!&self.turn_colour, rook_source_sq, rook_dest_sq, Piece::Rook);
            },
            MoveType::CastleQueenSide => {
                self.make_quiet_move(!&self.turn_colour, saved_move.move_.dest_sq,
                    saved_move.move_.source_sq, saved_move.move_.piece);
                let rook_source_sq = if self.turn_colour == Colour::White {
                    Square::D1
                } else {
                    Square::D8
                };
                let rook_dest_sq = if self.turn_colour == Colour::White {
                    Square::A1
                } else {
                    Square::A8
                };
                self.make_quiet_move(!&self.turn_colour, rook_source_sq, rook_dest_sq, Piece::Rook);
            },
            MoveType::MovePromotion(piece) => {
                self.bitboard.remove_piece(!&self.turn_colour, piece, saved_move.move_.dest_sq);
                self.bitboard.place_piece(!&self.turn_colour, Piece::Pawn, saved_move.move_.source_sq);
            },
            MoveType::CapturePromotion(captured_piece, promoted_piece) => {
                self.bitboard.remove_piece(!&self.turn_colour, promoted_piece, saved_move.move_.dest_sq);
                self.bitboard.place_piece(self.turn_colour, captured_piece, saved_move.move_.dest_sq);
                self.bitboard.place_piece(!&self.turn_colour, Piece::Pawn, saved_move.move_.source_sq);
            },
            _ => panic!("Invalid move type"),
        }
        self.en_passant = saved_move.prev_en_passant;
        self.half_move_num = saved_move.prev_half_move_num;
        self.castling_rights = saved_move.prev_castling_rights;
        self.turn_colour = !&self.turn_colour;
        self.move_num = if self.turn_colour == Colour::White {
            self.move_num - 1
        } else {
            self.move_num
        };
    }
    
    fn make_quiet_move(&mut self, colour: Colour, source_sq: Square, dest_sq: Square, piece: Piece) {
        self.bitboard.remove_piece(colour, piece, source_sq);
        self.bitboard.place_piece(colour, piece, dest_sq);
    }
}

impl Default for Board {
    fn default() -> Self {
        Board {
            bitboard: BitBoard::default(),
            turn_colour: Colour::White,
            move_num: 0,
            half_move_num: 0,
            castling_rights: CastlingRights::default(),
            en_passant: None,
            move_history: Vec::new(),
            board_hash: 0,
        }
    }
}

pub enum CastlingSide {
    WhiteKingside,
    WhiteQueenside,
    BlackKingside,
    BlackQueenside,
}

// bit position for each castling side is 1 << (CastlingSide as u32)
// e.g. BlackKingside is 1 << 2 i.e. 0b00000100
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CastlingRights {
    mask: u8,
}

impl Default for CastlingRights {
    fn default() -> Self {
        CastlingRights {
            mask: 0b00001111,
        }
    }
}

impl CastlingRights {
    pub fn enable(&mut self, side: CastlingSide) {
        self.mask |= 1u8 << side as u32;
    }
    
    pub fn disable(&mut self, side: CastlingSide) {
        self.mask &= !(1u8 << side as u32);
    }
    
    pub fn can_castle(&self, colour: Colour) -> (bool, bool) {
        match colour {
            Colour::White => {
                (self.mask & 1u8 << CastlingSide::WhiteKingside as u32 != 0,
                 self.mask & 1u8 << CastlingSide::WhiteQueenside as u32 != 0)
            },
            Colour::Black => {
                (self.mask & 1u8 << CastlingSide::BlackKingside as u32 != 0,
                 self.mask & 1u8 << CastlingSide::BlackQueenside as u32 != 0)
            }
        }
    }
}

impl TryFrom<&str> for CastlingRights {
    type Error = &'static str;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if s.is_empty() || s.len() > 4 {
            return Err("Error: invalid castling rights string in fen");
        }
        let mut rv = CastlingRights {
            mask: 0u8,
        };
        if s == "-" {
            return Ok(rv);
        }
        for c in s.chars() {
            match c {
                'K' => rv.enable(CastlingSide::WhiteKingside),
                'k' => rv.enable(CastlingSide::BlackKingside),
                'Q' => rv.enable(CastlingSide::WhiteQueenside),
                'q' => rv.enable(CastlingSide::BlackQueenside),
                _ => return Err("Error: invalid castling rights string in fen"),
            }
        }
        Ok(rv)
    }
}

impl Square {
    pub fn parse(s: &str) -> Result<Option<Square>, &'static str> {
        if s.is_empty() || s.len() > 2 {
            return Err("Error: invalid length en_passant string");
        }
        if s == "-" {
            Ok(None)
        } else {
            match s {
                "a3" => Ok(Some(Square::A3)),
                "a6" => Ok(Some(Square::A6)),
                "b3" => Ok(Some(Square::B3)),
                "b6" => Ok(Some(Square::B6)),
                "c3" => Ok(Some(Square::C3)),
                "c6" => Ok(Some(Square::C6)),
                "d3" => Ok(Some(Square::D3)),
                "d6" => Ok(Some(Square::D6)),
                "e3" => Ok(Some(Square::E3)),
                "e6" => Ok(Some(Square::E6)),
                "f3" => Ok(Some(Square::F3)),
                "f6" => Ok(Some(Square::F6)),
                "g3" => Ok(Some(Square::G3)),
                "g6" => Ok(Some(Square::G6)),
                "h3" => Ok(Some(Square::H3)),
                "h6" => Ok(Some(Square::H6)),
                _ => Err("Error: invalid en_passant square"),
            }
        }
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.bitboard)?;
        writeln!(f, "{:?} to play", self.turn_colour)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Piece;

    #[test]
    fn test_fen_initial_position() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::new(Some(fen)).unwrap();

        assert_eq!(board.turn_colour, Colour::White);
        assert_eq!(board.move_num, 1);
        assert_eq!(board.half_move_num, 0);
        assert_eq!(
            board.castling_rights,
            CastlingRights {
                mask: 0b00001111,
            }
        );
        assert_eq!(board.en_passant, None);
        assert_eq!(board.move_history.len(), 0);
        assert_eq!(board.board_hash, 0);

        assert_eq!(board.bitboard.get_entire_mask(), 0xFF_FF_00_00_00_00_FF_FF);
        assert_eq!(
            board.bitboard.get_colour_mask(Colour::White),
            0x00_00_00_00_00_00_FF_FF
        );
        assert_eq!(
            board.bitboard.get_colour_mask(Colour::Black),
            0xFF_FF_00_00_00_00_00_00
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Pawn, Colour::White),
            0x00_00_00_00_00_00_FF_00
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Pawn, Colour::Black),
            0x00_FF_00_00_00_00_00_00
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::King, Colour::White),
            0x00_00_00_00_00_00_00_10
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::King, Colour::Black),
            0x10_00_00_00_00_00_00_00
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Queen, Colour::White),
            0x00_00_00_00_00_00_00_08
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Queen, Colour::Black),
            0x08_00_00_00_00_00_00_00
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Rook, Colour::White),
            0x00_00_00_00_00_00_00_81
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Rook, Colour::Black),
            0x81_00_00_00_00_00_00_00
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Knight, Colour::White),
            0x00_00_00_00_00_00_00_42
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Knight, Colour::Black),
            0x42_00_00_00_00_00_00_00
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Bishop, Colour::White),
            0x00_00_00_00_00_00_00_24
        );
        assert_eq!(
            board
                .bitboard
                .get_colour_piece_mask(Piece::Bishop, Colour::Black),
            0x24_00_00_00_00_00_00_00
        );
    }

    #[test]
    fn test_fen_custom_position() {
        let fen = "8/8/8/3k4/8/8/4K3/8 w - - 0 1";
        let board = Board::new(Some(fen)).unwrap();

        assert_eq!(board.turn_colour, Colour::White);
        assert_eq!(board.move_num, 1);
        assert_eq!(board.half_move_num, 0);
        assert_eq!(
            board.castling_rights,
            CastlingRights {
                mask: 0u8,
            }
        );
        assert_eq!(board.en_passant, None);
        assert_eq!(board.move_history.len(), 0);
        assert_eq!(board.board_hash, 0);
    }

    #[test]
    fn test_fen_en_passant() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq e3 0 1";
        let board = Board::new(Some(fen)).unwrap();

        assert_eq!(board.turn_colour, Colour::Black);
        assert_eq!(board.move_num, 1);
        assert_eq!(board.half_move_num, 0);
        assert_eq!(
            board.castling_rights,
            CastlingRights {
                mask: 0b00001111,
            }
        );
        assert_eq!(board.en_passant, Some(Square::E3));
        assert_eq!(board.move_history.len(), 0);
        assert_eq!(board.board_hash, 0);
    }

    #[test]
    fn test_fen_invalid() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";
        let result = Board::new(Some(fen));
        assert!(result.is_err());

        // too many ranks
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/8 w KQkq - 0 1";
        let result = Board::new(Some(fen));
        assert!(result.is_err());

        // invalid piece character
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNxQKBNR w KQkq - 0 1";
        let result = Board::new(Some(fen));
        assert!(result.is_err());

        // too many white pawns
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let result = Board::new(Some(fen));
        assert!(result.is_err());
    }

    #[test]
    fn test_fen_partial_castling_rights() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQk - 0 1";
        let board = Board::new(Some(fen)).unwrap();

        assert_eq!(board.turn_colour, Colour::White);
        assert_eq!(board.move_num, 1);
        assert_eq!(board.half_move_num, 0);
        assert_eq!(
            board.castling_rights,
            CastlingRights {
                mask: 0b00000111,
            }
        );
        assert_eq!(board.en_passant, None);
        assert_eq!(board.move_history.len(), 0);
        assert_eq!(board.board_hash, 0);
    }
}
