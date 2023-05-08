use crate::masks::*;
use crate::{Colour, Square};

// Indexed by colour
// Pawns are unique in as much as they have different attacks and movements
// and their attacks/movements depend on their colour.
pub struct PawnAttackTable {
    pub moves: [Vec<u64>; 2],
    pub attacks: [Vec<u64>; 2],
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
                w_moves.push(north!(mask, 1) | north!(mask, 2));
            } else {
                w_moves.push(north!(mask, 1));
            }

            // double black push
            if (48..56).contains(&i) {
                b_moves.push(south!(mask, 1) | south!(mask, 2));
            } else {
                b_moves.push(south!(mask, 1));
            }

            // white attacks
            w_attacks
                .push(north_east!(mask) | north_west!(mask));

            // black attacks
            b_attacks
                .push(south_east!(mask) | south_west!(mask));

            mask <<= 1;
        }

        PawnAttackTable {
            moves: [w_moves, b_moves],
            attacks: [w_attacks, b_attacks],
        }
    }

    #[cfg(test)]
    pub fn get_moves(&self, square: Square, colour: Colour) -> u64 {
        self.moves[colour as usize][square as usize]
    }

    #[cfg(test)]
    pub fn get_attacks(&self, square: Square, colour: Colour) -> u64 {
        self.attacks[colour as usize][square as usize]
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
            fen_to_hex("8/8/8/8/4N3/4N3/8/8 w - - 0 1"),
            pawn_attack_table.get_moves(Square::E2, Colour::White),
        );

        test_bitboard_eq!(
            "Black pawn moves from D7 (single and double push)",
            fen_to_hex("8/8/3N4/3N4/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_moves(Square::D7, Colour::Black),
        );

        test_bitboard_eq!(
            "Black pawn moves from E2",
            fen_to_hex("8/8/8/8/8/8/8/4N3 w - - 0 1"),
            pawn_attack_table.get_moves(Square::E2, Colour::Black),
        );

        test_bitboard_eq!(
            "White pawn moves from D7",
            fen_to_hex("3N4/8/8/8/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_moves(Square::D7, Colour::White),
        );

        test_bitboard_eq!(
            "White pawn attacks from D2",
            fen_to_hex("8/8/8/8/8/2N1N3/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::D2, Colour::White),
        );

        test_bitboard_eq!(
            "Black pawn attacks from D5",
            fen_to_hex("8/8/8/8/2N1N3/8/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::D5, Colour::Black),
        );

        test_bitboard_eq!(
            "White pawn attacks from A4 (edge)",
            fen_to_hex("8/8/8/1N6/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::A4, Colour::White),
        );

        test_bitboard_eq!(
            "Black pawn attacks from A4 (edge)",
            fen_to_hex("8/8/8/8/8/1N6/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::A4, Colour::Black),
        );

        test_bitboard_eq!(
            "White pawn attacks from H7 (edge)",
            fen_to_hex("6N1/8/8/8/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::H7, Colour::White),
        );

        test_bitboard_eq!(
            "Black pawn attacks from H7 (edge)",
            fen_to_hex("8/8/6N1/8/8/8/8/8 w - - 0 1"),
            pawn_attack_table.get_attacks(Square::H7, Colour::Black),
        );
    }
}
