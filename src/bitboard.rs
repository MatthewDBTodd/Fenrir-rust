use crate::{Colour, Piece, Square};

use num_traits::FromPrimitive;
use std::convert::From;
use std::fmt;

struct PieceColour(Piece, Colour);

#[derive(Default)]
pub struct BitBoard {
    // indexed by colour, i.e. white = 0, black = 1
    pub colours: [u64; 2],
    // indexed by colour, then piece as per the order in the Piece enum
    pub pieces: [[u64; 6]; 2],
}

impl TryFrom<&str> for BitBoard {
    type Error = &'static str;

    fn try_from(pieces: &str) -> Result<Self, Self::Error> {
        let ranks: Vec<&str> = pieces.split('/').collect();
        if ranks.len() != 8 {
            return Err("Error: fen string does not contain 8 ranks of pieces");
        }
        let mut bb: BitBoard = BitBoard::default();
        let mut rank_idx = 64;
        for rank in ranks {
            rank_idx -= 8;
            let mut i = 0;
            for c in rank.chars() {
                i += if c.is_alphabetic() {
                    let PieceColour(piece, colour) = PieceColour::try_from(c)?;
                    let square: Square = FromPrimitive::from_usize(rank_idx + i).unwrap();
                    bb.place_piece(colour, piece, square);
                    1
                } else if ('1'..='8').contains(&c) {
                    c.to_digit(10).unwrap() as usize
                } else {
                    return Err("Error: fen string is invalid");
                };
                if i > 8 {
                    return Err("Error: too many pieces in rank");
                }
            }
        }
        Ok(bb)
    }
}

impl BitBoard {
    pub fn place_piece(&mut self, colour: Colour, piece: Piece, square: Square) {
        let square_mask = get_square_mask(square);
        debug_assert_eq!(self.get_entire_mask() & square_mask, 0);
        let piece_mask = self.get_piece_mask(colour, piece) | square_mask;
        self.set_piece_mask(colour, piece, piece_mask);
        let colour_mask = self.get_colour_mask(colour) | square_mask;
        self.set_colour_mask(colour, colour_mask);
    }

    pub fn remove_piece(&mut self, colour: Colour, piece: Piece, square: Square) {
        let square_mask = get_square_mask(square);
        let piece_mask = self.get_piece_mask(colour, piece);
        let colour_mask = self.get_colour_mask(colour);
        debug_assert_ne!(piece_mask & square_mask, 0);
        debug_assert_ne!(colour_mask & square_mask, 0);
        self.set_piece_mask(colour, piece, piece_mask ^ square_mask);
        self.set_colour_mask(colour, colour_mask ^ square_mask);
    }

    pub fn square_empty(&self, square: Square) -> bool {
        get_square_mask(square) & self.get_entire_mask() == 0
    }

    pub fn get_piece_mask(&self, colour: Colour, piece: Piece) -> u64 {
        self.pieces[colour as usize][piece as usize]
    }

    pub fn set_piece_mask(&mut self, colour: Colour, piece: Piece, mask: u64) {
        self.pieces[colour as usize][piece as usize] = mask;
    }

    pub fn get_colour_mask(&self, colour: Colour) -> u64 {
        self.colours[colour as usize]
    }

    pub fn set_colour_mask(&mut self, colour: Colour, mask: u64) {
        self.colours[colour as usize] = mask;
    }

    pub fn get_entire_mask(&self) -> u64 {
        self.get_colour_mask(Colour::White) | self.get_colour_mask(Colour::Black)
    }

    pub fn get_colour_piece_masks(&self, colour: Colour) -> [u64; 6] {
        self.pieces[colour as usize]
    }

    pub fn get_square_char(&self, square: Square) -> char {
        let square_mask = get_square_mask(square);
        let colour = if square_mask & self.get_colour_mask(Colour::White) > 0 {
            Colour::White
        } else if square_mask & self.get_colour_mask(Colour::Black) > 0 {
            Colour::Black
        } else {
            return ' ';
        };
        let piece_masks = self.get_colour_piece_masks(colour);
        let piece = 'outer: loop {
            for (i, mask) in piece_masks.iter().enumerate() {
                if square_mask & mask != 0 {
                    break 'outer Piece::from(i);
                }
            }
            panic!("No piece found on that square");
        };
        char::from(PieceColour(piece, colour))
    }
}

fn get_square_mask(square: Square) -> u64 {
    1u64 << (square as u32)
}

impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f)?;
        let mut rank_idx = 64;
        for rank in (1..9).rev() {
            rank_idx -= 8;
            write!(f, "   ")?;
            for _ in 0..8 {
                write!(f, "+---")?;
            }
            writeln!(f)?;
            write!(f, " {rank} ")?;
            for file in 0..8 {
                let c = self.get_square_char(Square::from_usize(rank_idx + file).unwrap());
                write!(f, "| {c} ")?;
            }
            writeln!(f, "|")?;
        }
        write!(f, "   ")?;
        for _ in 0..8 {
            write!(f, "+---")?;
        }
        writeln!(f)?;
        writeln!(f, "     a   b   c   d   e   f   g   h")?;
        writeln!(f)
    }
}

impl From<usize> for Piece {
    fn from(i: usize) -> Self {
        match i {
            0 => Piece::Pawn,
            1 => Piece::King,
            2 => Piece::Queen,
            3 => Piece::Bishop,
            4 => Piece::Knight,
            5 => Piece::Rook,
            _ => panic!("Index does not correspond to a piece"),
        }
    }
}

impl From<PieceColour> for char {
    fn from(piece_colour: PieceColour) -> Self {
        match piece_colour {
            PieceColour(Piece::Pawn, Colour::White) => 'P',
            PieceColour(Piece::Pawn, Colour::Black) => 'p',
            PieceColour(Piece::King, Colour::White) => 'K',
            PieceColour(Piece::King, Colour::Black) => 'k',
            PieceColour(Piece::Queen, Colour::White) => 'Q',
            PieceColour(Piece::Queen, Colour::Black) => 'q',
            PieceColour(Piece::Bishop, Colour::White) => 'B',
            PieceColour(Piece::Bishop, Colour::Black) => 'b',
            PieceColour(Piece::Knight, Colour::White) => 'N',
            PieceColour(Piece::Knight, Colour::Black) => 'n',
            PieceColour(Piece::Rook, Colour::White) => 'R',
            PieceColour(Piece::Rook, Colour::Black) => 'r',
        }
    }
}

impl TryFrom<char> for PieceColour {
    type Error = &'static str;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        let piece = match c.to_ascii_uppercase() {
            'P' => Piece::Pawn,
            'K' => Piece::King,
            'Q' => Piece::Queen,
            'B' => Piece::Bishop,
            'N' => Piece::Knight,
            'R' => Piece::Rook,
            _ => return Err("Error: fen string contains invalid characters"),
        };
        let colour = if c.is_uppercase() {
            Colour::White
        } else {
            Colour::Black
        };
        Ok(PieceColour(piece, colour))
    }
}

impl fmt::Debug for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{self}")
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_get_square_mask() {
        assert_eq!(get_square_mask(Square::A1), 0x1);
        assert_eq!(get_square_mask(Square::B1), 0x2);
        assert_eq!(get_square_mask(Square::A2), 0x100);
        assert_eq!(get_square_mask(Square::H8), 0x8000_0000_0000_0000);
    }

    #[test]
    fn test_place_piece() {
        let mut bitboard = BitBoard::default();

        bitboard.place_piece(Colour::White, Piece::King, Square::E1);
        bitboard.place_piece(Colour::White, Piece::Rook, Square::H1);
        bitboard.place_piece(Colour::Black, Piece::Pawn, Square::A7);
        bitboard.place_piece(Colour::Black, Piece::Queen, Square::H8);

        assert_eq!(
            bitboard.get_piece_mask(Colour::White, Piece::King),
            1u64 << (Square::E1 as u32)
        );
        assert_eq!(
            bitboard.get_piece_mask(Colour::White, Piece::Rook),
            1u64 << (Square::H1 as u32)
        );
        assert_eq!(
            bitboard.get_piece_mask(Colour::Black, Piece::Pawn),
            1u64 << (Square::A7 as u32)
        );
        assert_eq!(
            bitboard.get_piece_mask(Colour::Black, Piece::Queen),
            1u64 << (Square::H8 as u32)
        );
        assert_eq!(
            bitboard.get_colour_mask(Colour::White),
            (1u64 << (Square::E1 as u32)) | (1u64 << (Square::H1 as u32))
        );
        assert_eq!(
            bitboard.get_colour_mask(Colour::Black),
            (1u64 << (Square::A7 as u32)) | (1u64 << (Square::H8 as u32))
        );
    }

    #[test]
    fn test_remove_piece() {
        let mut bitboard = BitBoard::default();

        bitboard.place_piece(Colour::White, Piece::King, Square::E1);
        bitboard.place_piece(Colour::Black, Piece::Pawn, Square::A7);
        bitboard.place_piece(Colour::White, Piece::Rook, Square::H1);

        bitboard.remove_piece(Colour::White, Piece::King, Square::E1);
        bitboard.remove_piece(Colour::Black, Piece::Pawn, Square::A7);
        bitboard.remove_piece(Colour::White, Piece::Rook, Square::H1);

        assert_eq!(bitboard.get_piece_mask(Colour::White, Piece::King), 0);
        assert_eq!(bitboard.get_piece_mask(Colour::Black, Piece::Pawn), 0);
        assert_eq!(bitboard.get_piece_mask(Colour::White, Piece::Rook), 0);
        assert_eq!(bitboard.get_colour_mask(Colour::White), 0);
        assert_eq!(bitboard.get_colour_mask(Colour::Black), 0);
    }

    #[test]
    fn test_is_square_empty() {
        let mut bitboard = BitBoard::default();

        assert!(bitboard.square_empty(Square::E1));
        assert!(bitboard.square_empty(Square::A7));
        assert!(bitboard.square_empty(Square::H1));

        bitboard.place_piece(Colour::White, Piece::King, Square::E1);
        bitboard.place_piece(Colour::Black, Piece::Pawn, Square::A7);
        bitboard.place_piece(Colour::White, Piece::Rook, Square::H1);

        assert!(!bitboard.square_empty(Square::E1));
        assert!(!bitboard.square_empty(Square::A7));
        assert!(!bitboard.square_empty(Square::H1));

        bitboard.remove_piece(Colour::White, Piece::King, Square::E1);
        bitboard.remove_piece(Colour::Black, Piece::Pawn, Square::A7);
        bitboard.remove_piece(Colour::White, Piece::Rook, Square::H1);

        assert!(bitboard.square_empty(Square::E1));
        assert!(bitboard.square_empty(Square::A7));
        assert!(bitboard.square_empty(Square::H1));
    }
}
