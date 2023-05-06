use crate::masks::*;

pub fn init() -> Vec<u64> {
    let mut rv = Vec::with_capacity(64);
    let mut mask: u64 = 1;
    for _ in 0..64 {
        let n = north_north_west!(mask)
              | north_west_west!(mask)
              | south_south_west!(mask)
              | south_west_west!(mask)
              | north_north_east!(mask)
              | north_east_east!(mask)
              | south_south_east!(mask)
              | south_east_east!(mask);

        rv.push(n);
        mask <<= 1;
    }
    rv
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::*;
    use crate::Square;

    #[test]
    fn test_init() {
        let knight_attack_patterns = init();

        test_bitboard_eq!(
            "Knight attack pattern for E4",
            fen_to_hex("8/8/3N1N2/2N3N1/8/2N3N1/3N1N2/8 w - - 0 1"),
            knight_attack_patterns[Square::E4 as usize],
        );

        test_bitboard_eq!(
            "Knight attack pattern for A1 (corner)",
            fen_to_hex("8/8/8/8/8/1N6/2N5/8 w - - 0 1"),
            knight_attack_patterns[Square::A1 as usize],
        );

        test_bitboard_eq!(
            "Knight attack pattern for H1 (corner)",
            fen_to_hex("8/8/8/8/8/6N1/5N2/8 w - - 0 1"),
            knight_attack_patterns[Square::H1 as usize],
        );

        test_bitboard_eq!(
            "Knight attack pattern for A8 (corner)",
            fen_to_hex("8/2N5/1N6/8/8/8/8/8 w - - 0 1"),
            knight_attack_patterns[Square::A8 as usize],
        );

        test_bitboard_eq!(
            "Knight attack pattern for H8 (corner)",
            fen_to_hex("8/5N2/6N1/8/8/8/8/8 w - - 0 1"),
            knight_attack_patterns[Square::H8 as usize],
        );

        test_bitboard_eq!(
            "Knight attack pattern for A4 (edge)",
            fen_to_hex("8/8/1N6/2N5/8/2N5/1N6/8 w - - 0 1"),
            knight_attack_patterns[Square::A4 as usize],
        );

        test_bitboard_eq!(
            "Knight attack pattern for H4 (edge)",
            fen_to_hex("8/8/6N1/5N2/8/5N2/6N1/8 w - - 0 1"),
            knight_attack_patterns[Square::H4 as usize],
        );

        test_bitboard_eq!(
            "Knight attack pattern for E1 (edge)",
            fen_to_hex("8/8/8/8/8/3N1N2/2N3N1/8 w - - 0 1"),
            knight_attack_patterns[Square::E1 as usize],
        );

        test_bitboard_eq!(
            "Knight attack pattern for E8 (edge)",
            fen_to_hex("8/2N3N1/3N1N2/8/8/8/8/8 w - - 0 1"),
            knight_attack_patterns[Square::E8 as usize],
        );
    }
}
