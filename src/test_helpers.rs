use std::fmt::Write;

#[cfg(test)]
pub fn fen_to_hex(fen: &str) -> u64 {
    let fen: Vec<&str> = fen.split_ascii_whitespace().collect();
    let fen = fen[0];
    let ranks: Vec<&str> = fen.split('/').collect();
    let mut result = 0;
    let mut shift = 63;
    for rank in ranks {
        // reverse the string
        let rank: String = rank.chars().rev().collect();
        for c in rank.chars() {
            if c.is_numeric() {
                let n = c.to_digit(10).unwrap();
                if n > shift {
                    break;
                }
                shift -= c.to_digit(10).unwrap();
            } else {
                result |= 1 << shift;
                if shift == 0 {
                    break;
                }
                shift -= 1;
            }
        }
    }
    result
}

#[cfg(test)]
pub fn hex_to_board(hex: u64) -> String {
    let mut rv = String::new();
    writeln!(&mut rv).unwrap();
    let mut rank_idx = 56;
    for _ in 0..8 {
        for file_idx in 0..8 {
            let n = (hex >> (rank_idx + file_idx)) & 1;
            write!(&mut rv, "{n} ").unwrap();
        }
        writeln!(&mut rv).unwrap();
        rank_idx -= 8;
    }
    rv
}

#[cfg(test)]
#[macro_export]
macro_rules! test_bitboard_eq {
    ($test_name:expr, $expected:expr, $actual:expr $(,)?) => {{
        let expected = $expected;
        let actual = $actual;
        assert_eq!(
            expected,
            actual,
            "Test '{}' failed:\nExpected:{}\nActual:{}",
            $test_name,
            hex_to_board(expected),
            hex_to_board(actual)
        );
    }};
}

#[cfg(test)]
pub(crate) use test_bitboard_eq;
