use crate::bitboard::*;
use crate::{Colour, Move, Square};

use std::fmt;

#[derive(Debug)]
#[allow(dead_code)]
pub struct Board {
    bitboard: BitBoard,
    turn_colour: Colour,
    move_num: u32,
    half_move_num: u32,
    castling_rights: CastlingRights,
    en_passant: Option<Square>,
    move_history: Vec<Move>,
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
        let Ok(half_move_num) = parts[4].parse::<u32>() else {
            return Err("Error: invalid half move num in fen string");
        };

        let Ok(move_num) = parts[5].parse::<u32>() else {
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
        
    }
    
    pub fn undo_move(&mut self) {
        
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

#[derive(Debug, PartialEq)]
struct CastlingRights {
    w_kingside: bool,
    w_queenside: bool,
    b_kingside: bool,
    b_queenside: bool,
}

impl Default for CastlingRights {
    fn default() -> Self {
        CastlingRights {
            w_kingside: true,
            w_queenside: true,
            b_kingside: true,
            b_queenside: true,
        }
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

impl TryFrom<&str> for CastlingRights {
    type Error = &'static str;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if s.is_empty() || s.len() > 4 {
            return Err("Error: invalid castling rights string in fen");
        }
        let mut rv = CastlingRights {
            w_kingside: false,
            w_queenside: false,
            b_kingside: false,
            b_queenside: false,
        };
        if s == "-" {
            return Ok(rv);
        }
        for c in s.chars() {
            match c {
                'K' => rv.w_kingside = true,
                'k' => rv.b_kingside = true,
                'Q' => rv.w_queenside = true,
                'q' => rv.b_queenside = true,
                _ => return Err("Error: invalid castling rights string in fen"),
            }
        }
        Ok(rv)
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
                w_kingside: true,
                w_queenside: true,
                b_kingside: true,
                b_queenside: true,
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
                w_kingside: false,
                w_queenside: false,
                b_kingside: false,
                b_queenside: false,
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
                w_kingside: true,
                w_queenside: true,
                b_kingside: true,
                b_queenside: true,
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
                w_kingside: true,
                w_queenside: true,
                b_kingside: true,
                b_queenside: false,
            }
        );
        assert_eq!(board.en_passant, None);
        assert_eq!(board.move_history.len(), 0);
        assert_eq!(board.board_hash, 0);
    }
}
