use crate::Square;
use crate::chess_move::{Move, MoveType};
use crate::Piece;
use std::sync::atomic::{AtomicU64, Ordering};
use std::convert::From;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[derive(Debug, Clone, PartialEq, FromPrimitive)]
pub enum ResultFlag {
    Empty,
    Exact,
    LowerBound,
    UpperBound,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CachedSearchResult {
    // pub hash: u64,
    pub depth_searched: u8,
    pub eval: i32,
    pub best_move: Option<Move>,
    pub flag: ResultFlag,
}

// bits 0-6 = 
impl From<u64> for CachedSearchResult {
    fn from(mut n: u64) -> Self {
        let depth_searched: u8 = (n & 0x3F) as u8;
        n >>= 6;
        let eval: i32 = (n & 0xFF_FF_FF_FF) as i32;
        n >>= 32; 
        // source_sq
        let source_sq = Square::from_u64(n & 0x3F).unwrap();
        n >>= 6;
        let dest_sq = Square::from_u64(n & 0x3F).unwrap();
        n >>= 6;
        let piece = Piece::from_u64(n & 0x7).unwrap();
        n >>= 3;
        let move_type_n = n & 0x7;
        n >>= 3;
        // move type data
        let mut move_type_data = n & 0x3F;
        n >>= 6;
        let move_type = match move_type_n {
            0 if move_type_data == 0x3F => MoveType::ErrorMove,
            0 => MoveType::Quiet,
            1 => {
                let captured_piece = Piece::from_u64(move_type_data).unwrap();
                MoveType::Capture(captured_piece)
            },
            2 => MoveType::DoublePawnPush,
            3 => {
                let ep_square = Square::from_u64(move_type_data).unwrap();
                MoveType::EnPassant(ep_square)
            },
            4 => MoveType::CastleKingSide,
            5 => MoveType::CastleQueenSide,
            6 => {
                let promotion_piece = Piece::from_u64(move_type_data).unwrap();
                MoveType::MovePromotion(promotion_piece)
            },
            7 => {
                let captured_piece = Piece::from_u64(move_type_data & 0x7).unwrap();
                move_type_data >>= 3;
                let promotion_piece = Piece::from_u64(move_type_data & 0x7).unwrap();
                MoveType::CapturePromotion(captured_piece, promotion_piece)
            }
            _ => panic!("Invalid move type bitmask"),
        };
        let flag = ResultFlag::from_u64(n & 0x3).unwrap();

        Self {
            eval,
            best_move: if move_type == MoveType::ErrorMove {
                None
            } else {
                Some(Move {
                    source_sq,
                    dest_sq,
                    piece,
                    move_type,
                })
            },
            depth_searched,
            flag,
        }
    }
}

impl From<CachedSearchResult> for u64 {
    fn from(entry: CachedSearchResult) -> Self {
        let mut rv: u64 = 0;
        rv |= entry.flag as u64;

        rv <<= 6;
        if entry.best_move.is_none() {
            rv |= 0x3F;
            rv <<= 3;
            rv |= 0;
            rv <<= 15;
        } else {
            let best_move = entry.best_move.unwrap();
            rv |= match best_move.move_type {
                MoveType::Quiet => 0,
                MoveType::Capture(piece) => piece as u64,
                MoveType::DoublePawnPush => 0,
                MoveType::EnPassant(square) => square as u64,
                MoveType::CastleKingSide => 0,
                MoveType::CastleQueenSide => 0,
                MoveType::MovePromotion(piece) => piece as u64,
                MoveType::CapturePromotion(captured_piece, promotion_piece) => {
                    let mut mask = promotion_piece as u64;
                    mask <<= 3;
                    mask |= captured_piece as u64;
                    mask
                },
                _ => panic!("Invalid move type in encoding function"),
            };
            rv <<= 3;
            rv |= match best_move.move_type {
                MoveType::Quiet => 0,
                MoveType::Capture(_) => 1,
                MoveType::DoublePawnPush => 2,
                MoveType::EnPassant(_) => 3,
                MoveType::CastleKingSide => 4,
                MoveType::CastleQueenSide => 5,
                MoveType::MovePromotion(_) => 6,
                MoveType::CapturePromotion(_, _) => 7,
                _ => panic!("Invalid move type in encoding function"),
            };
            rv <<= 3;
            rv |= best_move.piece as u64;
            rv <<= 6;
            rv |= best_move.dest_sq as u64;
            rv <<= 6;
            rv |= best_move.source_sq as u64;
        }
        rv <<= 32;
        rv |= (entry.eval as u64) & 0xFF_FF_FF_FF;
        rv <<= 6;
        rv |= (entry.depth_searched as u64) & 0x3F;
        rv
    }
}

// impl Default for CachedSearchResult {
//     fn default() -> Self {
//         Self {
//             hash: 0,
//             depth_searched: 0,
//             eval: 0,
//             best_move: Some(Move::default()),
//             flag: ResultFlag::Empty,
//         }
//     }
// }

// #[derive(Clone)]
pub struct EncodedSearchResult {
    pub hash: AtomicU64,
    pub data: AtomicU64,
}

impl Default for EncodedSearchResult {
    fn default() -> Self {
        Self {
            hash: AtomicU64::new(0),
            data: AtomicU64::new(0),
        }
    }
}

#[derive(Default)]
pub struct EncodedSearchResultPair {
    pub depth_preferred: EncodedSearchResult,
    pub always_replace: EncodedSearchResult,
}

// #[derive(Debug, Clone, Default)]
// pub struct CachedSearchResultPair {
//     // two-tier replacement strategy:
//     // If the new entry has a greater depth than the depth_preferred entry, it replaces that,
//     // otherwise it replaces the always_replace entry.
//     // See http://web.archive.org/web/20221112115144/http://mediocrechess.blogspot.com/2007/01/guide-transposition-tables.html
//     // and http://web.archive.org/web/20230326031052/https://www.chessprogramming.org/Transposition_Table
//     pub depth_preferred: CachedSearchResult,
//     pub always_replace: CachedSearchResult,
// }

// #[derive(Debug, PartialEq)]
// pub enum EntryMatch {
//     NoMatch,
//     DepthPreferredMatch,
//     AlwaysReplaceMatch,
// }

pub struct TranspositionTable {
    entries: Vec<EncodedSearchResultPair>,
    capacity: usize,
}

impl TranspositionTable {
    pub fn new(capacity: usize) -> Self {
        assert!(capacity.is_power_of_two(), "Capacity should be a power of 2");
        let v = (0..capacity).map(|_| EncodedSearchResultPair::default()).collect();
        Self {
            entries: v,
            capacity,
        }
    }

    pub fn get(&self, hash: u64) -> Option<CachedSearchResult> {
        let idx = hash as usize % self.capacity;
        let pair = &self.entries[idx];

        let entry_hash = pair.depth_preferred.hash.load(Ordering::Relaxed);
        let entry_data = pair.depth_preferred.data.load(Ordering::Relaxed);
        let actual_hash = entry_hash ^ entry_data;
        if actual_hash == hash {
            return Some(entry_data.into());
        }

        let entry_hash = pair.always_replace.hash.load(Ordering::Relaxed);
        let entry_data = pair.always_replace.hash.load(Ordering::Relaxed);
        let actual_hash = entry_hash ^ entry_data;
        if actual_hash == hash {
            return Some(entry_data.into());
        }
        None
        // let rv = &mut self.entries[hash as usize % self.capacity];
        // let match_type = if rv.depth_preferred.flag != ResultFlag::Empty && 
        //                     rv.depth_preferred.hash == hash
        // {
        //     EntryMatch::DepthPreferredMatch
        // } else if rv.always_replace.flag != ResultFlag::Empty && 
        //           rv.always_replace.hash == hash
        // {
        //     EntryMatch::AlwaysReplaceMatch
        // } else {
        //     EntryMatch::NoMatch
        // };
        // (rv, match_type)
    }

    pub fn insert(&mut self, hash: u64, search_result: CachedSearchResult) {

        let idx = hash as usize % self.capacity;

        let entry = &mut self.entries[idx].depth_preferred;
        let entry_data = entry.data.load(Ordering::Relaxed);
        let depth = (entry_data & 0x3F) as u8;
        
        if search_result.depth_searched > depth {
            let new_data: u64 = search_result.into();
            let new_hash = hash ^ new_data;
            entry.hash.store(new_hash, Ordering::Relaxed);
            entry.data.store(new_data, Ordering::Relaxed);
        } else {
            let new_data: u64 = search_result.into();
            let new_hash = hash ^ new_data;
            let entry = &mut self.entries[idx].always_replace;
            entry.hash.store(new_hash, Ordering::Relaxed);
            entry.data.store(new_data, Ordering::Relaxed);
        }
    }

    // pub fn reset(&mut self) {
    //     for pair in self.entries.iter_mut() {
    //         pair.depth_preferred.flag = ResultFlag::Empty;
    //         pair.always_replace.flag = ResultFlag::Empty;
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_move_encoding {
        ($search_result:expr $(,)?) => {{
            let encoded: u64 = $search_result.clone().into();

            let actual: CachedSearchResult = encoded.into();

            assert_eq!($search_result, actual);
        }};
    }

    #[test]
    fn test_quiet_move() {
        test_move_encoding!(CachedSearchResult {
            eval: -1234,
            best_move: Some(Move {
                source_sq: Square::B4,
                dest_sq: Square::H8,
                piece: Piece::Queen,
                move_type: MoveType::Quiet,
            }),
            depth_searched: 11,
            flag: ResultFlag::Exact,
        });
    }

    #[test]
    fn test_capture() {
        test_move_encoding!(CachedSearchResult {
            eval: 100000,
            best_move: Some(Move {
                source_sq: Square::A1,
                dest_sq: Square::F1,
                piece: Piece::Rook,
                move_type: MoveType::Capture(Piece::Rook),
            }),
            depth_searched: 8,
            flag: ResultFlag::LowerBound,
        });
    }

    #[test]
    fn test_double_pawn_push() {
        test_move_encoding!(CachedSearchResult {
            eval: 0,
            best_move: Some(Move {
                source_sq: Square::D7,
                dest_sq: Square::D5,
                piece: Piece::Pawn,
                move_type: MoveType::DoublePawnPush,
            }),
            depth_searched: 20,
            flag: ResultFlag::UpperBound,
        });
    }

    #[test]
    fn test_en_passant() {
        test_move_encoding!(CachedSearchResult {
            eval: 150,
            best_move: Some(Move {
                source_sq: Square::E5,
                dest_sq: Square::D6,
                piece: Piece::Pawn,
                move_type: MoveType::EnPassant(Square::D5),
            }),
            depth_searched: 5,
            flag: ResultFlag::Exact,
        });
    }

    #[test]
    fn test_castle_kingside() {
        test_move_encoding!(CachedSearchResult {
            eval: 990,
            best_move: Some(Move {
                source_sq: Square::E1,
                dest_sq: Square::G1,
                piece: Piece::King,
                move_type: MoveType::CastleKingSide,
            }),
            depth_searched: 10,
            flag: ResultFlag::Exact,
        });
    }

    #[test]
    fn test_castle_queenside() {
        test_move_encoding!(CachedSearchResult {
            eval: -405,
            best_move: Some(Move {
                source_sq: Square::E8,
                dest_sq: Square::C8,
                piece: Piece::King,
                move_type: MoveType::CastleQueenSide,
            }),
            depth_searched: 15,
            flag: ResultFlag::LowerBound,
        });
    }

    #[test]
    fn test_move_promotion() {
        test_move_encoding!(CachedSearchResult {
            eval: 2300,
            best_move: Some(Move {
                source_sq: Square::E2,
                dest_sq: Square::E1,
                piece: Piece::Pawn,
                move_type: MoveType::MovePromotion(Piece::Queen),
            }),
            depth_searched: 10,
            flag: ResultFlag::Exact,
        });
    }

    #[test]
    fn test_capture_promotion() {
        test_move_encoding!(CachedSearchResult {
            eval: -100000,
            best_move: Some(Move {
                source_sq: Square::C7,
                dest_sq: Square::B8,
                piece: Piece::Pawn,
                move_type: MoveType::CapturePromotion(Piece::Rook, Piece::Knight),
            }),
            depth_searched: 10,
            flag: ResultFlag::Exact,
        });
    }

    #[test]
    fn test_no_move() {
        test_move_encoding!(CachedSearchResult {
            eval: -100000,
            best_move: None,
            depth_searched: 10,
            flag: ResultFlag::LowerBound,
        });
    }
}