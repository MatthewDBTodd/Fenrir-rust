use crate::masks::*;
use crate::{Colour, Square};

pub struct PawnAttackTable {
    w_moves: Vec<u64>,
    w_attacks: Vec<u64>,
    b_moves: Vec<u64>,
    b_attacks: Vec<u64>,
}

impl PawnAttackTable {
    pub fn init() -> Self {
        let mut w_moves = Vec::with_capacity(64);
        let mut w_attacks = Vec::with_capacity(64);
        let mut b_moves = Vec::with_capacity(64);
        let mut b_attacks = Vec::with_capacity(64);

        let mut mask: u64 = 1;
        for i in 0..64 {
            // double white push
            if (8..16).contains(&i) {
                w_moves.push(mask | north!(mask, 1) | north!(mask, 2));
            } else {
                w_moves.push(mask | north!(mask, 1));
            }

            // double black push
            if (48..56).contains(&i) {
                b_moves.push(mask | south!(mask, 1) | south!(mask, 2));
            } else {
                b_moves.push(mask | south!(mask, 1));
            }

            // white attacks
            w_attacks
                .push(mask | (north_east!(mask) & NOT_A_FILE) | (north_west!(mask) & NOT_H_FILE));

            // black attacks
            b_attacks
                .push(mask | (south_east!(mask) & NOT_A_FILE) | (south_west!(mask) & NOT_H_FILE));

            mask <<= 1;
        }

        PawnAttackTable {
            w_moves,
            w_attacks,
            b_moves,
            b_attacks,
        }
    }

    pub fn get_moves(&self, square: Square, colour: Colour) -> u64 {
        if colour == Colour::White {
            self.w_moves[square as usize]
        } else {
            self.b_moves[square as usize]
        }
    }

    pub fn get_attacks(&self, square: Square, colour: Colour) -> u64 {
        if colour == Colour::White {
            self.w_attacks[square as usize]
        } else {
            self.b_attacks[square as usize]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::*;

    #[test]
    fn test_init() {
        let pawn_attack_table = PawnAttackTable::init();

        test_bitboard_eq!(
            "White pawn moves from E2 (single and double push)",
            fen_to_hex("8/8/8/8/4P3/4P3/4P3/8 w - - 0 1"),
            pawn_attack_table.get_moves(Square::E2, Colour::White),
        );

        test_bitboard_eq!(
            "Black pawn moves from D7 (single and double push)",
            fen_to_hex("8/3P4/3P4/3P4/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_moves(Square::D7, Colour::Black),
        );

        test_bitboard_eq!(
            "Black pawn moves from E2",
            fen_to_hex("8/8/8/8/8/8/4P3/4P3 w - - 0 1"),
            pawn_attack_table.get_moves(Square::E2, Colour::Black),
        );

        test_bitboard_eq!(
            "White pawn moves from D7",
            fen_to_hex("3P4/3P4/8/8/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_moves(Square::D7, Colour::White),
        );

        test_bitboard_eq!(
            "White pawn attacks from D2",
            fen_to_hex("8/8/8/8/8/2P1P3/3P4/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::D2, Colour::White),
        );

        test_bitboard_eq!(
            "Black pawn attacks from D5",
            fen_to_hex("8/8/8/3P4/2P1P3/8/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::D5, Colour::Black),
        );

        test_bitboard_eq!(
            "White pawn attacks from A4 (edge)",
            fen_to_hex("8/8/8/1P6/P7/8/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::A4, Colour::White),
        );

        test_bitboard_eq!(
            "Black pawn attacks from A4 (edge)",
            fen_to_hex("8/8/8/8/P7/1P6/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::A4, Colour::Black),
        );

        test_bitboard_eq!(
            "White pawn attacks from H7 (edge)",
            fen_to_hex("6P1/7P/8/8/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::H7, Colour::White),
        );

        test_bitboard_eq!(
            "Black pawn attacks from H7 (edge)",
            fen_to_hex("8/7P/6P1/8/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::H7, Colour::Black),
        );
    }
}
