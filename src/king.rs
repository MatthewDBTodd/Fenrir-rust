use crate::masks;

/*
 * Shifting left goes up a file, i.e. A -> B file. But when in H file that wraps 
 * it back into the A file, so when shifting left we need to mask out the A file.
 * And visa-versa for shifting right.
 */
pub fn init() -> Vec<u64> {
    let mut rv = Vec::with_capacity(64);
    let mut mask: u64 = 1;
    for _ in 0..64 {
        // 0 0 0 0 1 0 0 0 => 0 0 0 1 1 1 0 0
        let n = mask | ((mask << 1) & masks::NOT_A_FILE) | ((mask >> 1) & masks::NOT_H_FILE);

        // 0 0 0 0 0 0 0 0    0 0 0 1 1 1 0 0
        // 0 0 0 1 1 1 0 0 => 0 0 0 1 1 1 0 0
        // 0 0 0 0 0 0 0 0 => 0 0 0 1 1 1 0 0
        let n = n | (n << 8) | (n >> 8);

        rv.push(n);
        mask <<= 1;
    }
    rv
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Square;
    use crate::test_helpers;
    
    #[test]
    fn test_init() {
        let king_attack_patterns = init();

        // Test E4
        let expected = 0x3838380000;
        let actual = king_attack_patterns[Square::E4 as usize];
        assert_eq!(
            actual,
            expected,
            "King attack pattern for E4 is incorrect. Expected: {} Actual: {}",
            test_helpers::hex_to_board(expected) ,
            test_helpers::hex_to_board(actual),
        );

        // Test A1 (corner)
        let expected = 0x303;
        let actual = king_attack_patterns[Square::A1 as usize];
        assert_eq!(
            actual,
            expected,
            "King attack pattern for A1 is incorrect. Expected: {} Actual: {}",
            test_helpers::hex_to_board(expected),
            test_helpers::hex_to_board(actual),
        );

        // Test H1 (corner)
        let expected = 0xc0c0;
        let actual = king_attack_patterns[Square::H1 as usize];
        assert_eq!(
            king_attack_patterns[Square::H1 as usize],
            expected,
            "King attack pattern for H1 is incorrect. Expected: {} Actual: {}",
            test_helpers::hex_to_board(expected), 
            test_helpers::hex_to_board(actual),
        );

        // Test A8 (corner)
        let expected = 0x303000000000000;
        let actual = king_attack_patterns[Square::A8 as usize];
        assert_eq!(
            king_attack_patterns[Square::A8 as usize],
            expected,
            "King attack pattern for A8 is incorrect. Expected: {} Actual: {}",
            test_helpers::hex_to_board(expected),
            test_helpers::hex_to_board(actual),
        );

        // Test H8 (corner)
        let expected = 0xc0c0000000000000;
        let actual = king_attack_patterns[Square::H8 as usize];
        assert_eq!(
            king_attack_patterns[Square::H8 as usize],
            expected,
            "King attack pattern for H8 is incorrect. Expected: {} Actual: {}",
            test_helpers::hex_to_board(expected),
            test_helpers::hex_to_board(actual), 
        );

        // Test A4 (edge)
        let expected = 0x303030000;
        let actual = king_attack_patterns[Square::A4 as usize];
        assert_eq!(
            king_attack_patterns[Square::A4 as usize],
            expected,
            "King attack pattern for A4 is incorrect. Expected: {} Actual: {}",
            test_helpers::hex_to_board(expected), 
            test_helpers::hex_to_board(actual), 
        );

        // Test H4 (edge)
        let expected = 0xc0c0c00000;
        let actual = king_attack_patterns[Square::H4 as usize];
        assert_eq!(
            king_attack_patterns[Square::H4 as usize],
            expected,
            "King attack pattern for H4 is incorrect. Expected {} Actual {}",
            test_helpers::hex_to_board(expected),
            test_helpers::hex_to_board(actual),
        );

        // Test E1 (edge)
        let expected = 0x3838;
        let actual = king_attack_patterns[Square::E1 as usize];
        assert_eq!(
            king_attack_patterns[Square::E1 as usize],
            expected,
            "King attack pattern for E1 is incorrect. Expected {} Actual: {}",
            test_helpers::hex_to_board(expected), 
            test_helpers::hex_to_board(actual), 
        );

        // Test E8 (edge)
        let expected = 0x3838000000000000;
        let actual = king_attack_patterns[Square::E8 as usize];
        assert_eq!(
            king_attack_patterns[Square::E8 as usize],
            expected,
            "King attack pattern for E8 is incorrect. Expected: {} Actual: {}",
            test_helpers::hex_to_board(expected), 
            test_helpers::hex_to_board(actual), 
        );
    }
}
