use chess::board::Board;
use chess::*;

fn main() {
    let mut b = Board::new(Some(
        "1r1k1b1r/p4pp1/1p1q1n1p/1Bp5/5P2/1Q4NP/PBPN2P1/1R2K2R b K - 0 1",
    ))
    .unwrap();
    print!("{b}");
}
