use crate::masks::*;

pub fn init() -> Vec<u64> {
    let mut rv = Vec::with_capacity(64);
    let mut mask: u64 = 1;
    for _ in 0..64 {
        let n = mask | (east!(mask, 1) & NOT_A_FILE) | (west!(mask, 1) & NOT_H_FILE);
        let n = n | north!(n, 1) | south!(n, 1);

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
        let king_attack_patterns = init();

        test_bitboard_eq!(
            "King attack pattern for E4",
            fen_to_hex("8/8/8/3KKK2/3KKK2/3KKK2/8/8 w - - 0 1"),
            king_attack_patterns[Square::E4 as usize],
        );

        test_bitboard_eq!(
            "King attack pattern for A1 (corner)",
            fen_to_hex("8/8/8/8/8/8/KK6/KK6 w - - 0 1"),
            king_attack_patterns[Square::A1 as usize],
        );

        test_bitboard_eq!(
            "King attack pattern for H1 (corner)",
            fen_to_hex("8/8/8/8/8/8/6KK/6KK w - - 0 1"),
            king_attack_patterns[Square::H1 as usize],
        );

        test_bitboard_eq!(
            "King attack pattern for A8 (corner)",
            fen_to_hex("KK6/KK6/8/8/8/8/8/8 w - - 0 1"),
            king_attack_patterns[Square::A8 as usize],
        );

        test_bitboard_eq!(
            "King attack pattern for H8 (corner)",
            fen_to_hex("6KK/6KK/8/8/8/8/8/8 w - - 0 1"),
            king_attack_patterns[Square::H8 as usize],
        );

        test_bitboard_eq!(
            "King attack pattern for A4 (edge)",
            fen_to_hex("8/8/8/KK6/KK6/KK6/8/8 w - - 0 1"),
            king_attack_patterns[Square::A4 as usize],
        );

        test_bitboard_eq!(
            "King attack pattern for H4 (edge)",
            fen_to_hex("8/8/8/6KK/6KK/6KK/8/8 w - - 0 1"),
            king_attack_patterns[Square::H4 as usize],
        );

        test_bitboard_eq!(
            "King attack pattern for E1 (edge)",
            fen_to_hex("8/8/8/8/8/8/3KKK2/3KKK2 w - - 0 1"),
            king_attack_patterns[Square::E1 as usize],
        );

        test_bitboard_eq!(
            "King attack pattern for E8 (edge)",
            fen_to_hex("3KKK2/3KKK2/8/8/8/8/8/8 w - - 0 1"),
            king_attack_patterns[Square::E8 as usize],
        );
    }
}
