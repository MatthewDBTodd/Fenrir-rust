use num_derive::FromPrimitive;

pub mod bitboard;
pub mod board;
pub mod king;
pub mod knight;
pub mod masks;
pub mod pawn;
pub mod sliding_piece;
pub mod attack_table;
pub mod chess_move;
pub mod shared_perft;
pub mod search_move;
pub mod board_hash;
pub mod engine;

// #[cfg(test)]
pub mod test_helpers;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Colour {
    White,
    Black,
}

impl std::ops::Not for &Colour {
    type Output = Colour;

    fn not(self) -> Self::Output {
        match self {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, FromPrimitive)]
pub enum Piece {
    Pawn,
    King,
    Queen,
    Bishop,
    Knight,
    Rook,
}

// Each squares index is its little-endian bit index
#[rustfmt::skip]
#[derive(Debug, Clone, Copy, PartialEq, FromPrimitive)]
pub enum Square {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
}

impl TryFrom<&str> for Colour {
    type Error = &'static str;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if s.len() != 1 {
            return Err("Error: turn colour has more than 1 character");
        }
        match s.chars().next().unwrap() {
            'w' => Ok(Colour::White),
            'b' => Ok(Colour::Black),
            _ => Err("Error: invalid colour in fen string"),
        }
    }
}
