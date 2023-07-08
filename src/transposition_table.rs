use crate::chess_move::Move;

#[derive(Debug, Clone, PartialEq)]
pub enum ResultFlag {
    Exact,
    LowerBound,
    UpperBound,
    Empty,
}

#[derive(Debug, Clone)]
pub struct CachedSearchResult {
    pub hash: u64,
    pub depth_searched: u8,
    pub eval: i32,
    pub best_move: Option<Move>,
    pub flag: ResultFlag,
}

impl Default for CachedSearchResult {
    fn default() -> Self {
        Self {
            hash: 0,
            depth_searched: 0,
            eval: 0,
            best_move: Some(Move::default()),
            flag: ResultFlag::Empty,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct CachedSearchResultPair {
    // two-tier replacement strategy:
    // If the new entry has a greater depth than the depth_preferred entry, it replaces that,
    // otherwise it replaces the always_replace entry.
    // See http://web.archive.org/web/20221112115144/http://mediocrechess.blogspot.com/2007/01/guide-transposition-tables.html
    // and http://web.archive.org/web/20230326031052/https://www.chessprogramming.org/Transposition_Table
    pub depth_preferred: CachedSearchResult,
    pub always_replace: CachedSearchResult,
}

#[derive(Debug, PartialEq)]
pub enum EntryMatch {
    NoMatch,
    DepthPreferredMatch,
    AlwaysReplaceMatch,
}

pub struct TranspositionTable {
    entries: Vec<CachedSearchResultPair>,
    capacity: usize,
}

impl TranspositionTable {
    pub fn new(capacity: usize) -> Self {
        assert!(capacity.is_power_of_two(), "Capacity should be a power of 2");
        Self {
            entries: vec![CachedSearchResultPair::default(); capacity],
            capacity,
        }
    }

    pub fn get(&mut self, hash: u64) -> (&mut CachedSearchResultPair, EntryMatch) {
        let rv = &mut self.entries[hash as usize % self.capacity];
        let match_type = if rv.depth_preferred.flag != ResultFlag::Empty && 
                            rv.depth_preferred.hash == hash
        {
            EntryMatch::DepthPreferredMatch
        } else if rv.always_replace.flag != ResultFlag::Empty && 
                  rv.always_replace.hash == hash
        {
            EntryMatch::AlwaysReplaceMatch
        } else {
            EntryMatch::NoMatch
        };
        (rv, match_type)
    }

    // pub fn reset(&mut self) {
    //     for pair in self.entries.iter_mut() {
    //         pair.depth_preferred.flag = ResultFlag::Empty;
    //         pair.always_replace.flag = ResultFlag::Empty;
    //     }
    // }
}