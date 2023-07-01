use std::fmt;
use std::sync::{Arc, Mutex, atomic::{AtomicBool, Ordering}};
use std::thread;
use std::time::Duration;

use crate::attack_table::AttackTable;
use crate::board::Board;
use crate::chess_move::Move;
use crate::board_hash::ZobristHasher;
use crate::search_move::*;
use crate::shared_perft::*;
use crate::transposition_table::TranspositionTable;

pub struct Engine {
    board: Arc<Mutex<Board>>,
    attack_table: Arc<AttackTable>,
    hasher: Arc<ZobristHasher>,
    transposition_table: Arc<Mutex<TranspositionTable>>,
}

#[derive(PartialEq)]
pub enum SearchMethod {
    ToDepth(u32),
    ToTime(Duration),
}

impl Engine {
    pub fn new(fen: Option<&str>) -> Self {
        // print!("Generating attack tables... ");
        // std::io::stdout().flush().expect("Couldn't flush stdout");
        let attack_table = Arc::new(AttackTable::init());
        // println!("done");
        // print!("Generating Zobrist Hasher...");
        // std::io::stdout().flush().expect("Couldn't flush stdout");
        let hasher = Arc::new(ZobristHasher::new());
        // println!("done");
        let board = Arc::new(Mutex::new(Board::new(fen, hasher.clone()).unwrap()));

        let transposition_table = Arc::new(Mutex::new(TranspositionTable::new(1 << 20)));

        Self {
            board,
            attack_table,
            hasher,
            transposition_table,
        }
    }

    pub fn input_start_pos(&mut self, fen: &str) {
        let new_board = Arc::new(Mutex::new(Board::new(Some(fen), self.hasher.clone()).unwrap()));
        self.board = new_board;
    }

    pub fn starting_pos(&mut self) {
        let new_board = Arc::new(Mutex::new(Board::new(None, self.hasher.clone()).unwrap()));
        self.board = new_board;
    }

    pub fn make_move(&mut self, chess_move: Move) {
        let mut board = self.board.lock().unwrap();
        board.make_move(chess_move);
    }

    pub fn undo_move(&mut self) {
        let mut board = self.board.lock().unwrap();
        board.undo_move();
    }

    pub fn search_position(&mut self, search_method: SearchMethod) -> (Move, i32, u32) {
        let stop_searching = Arc::new(AtomicBool::new(false));
        let stop_flag = stop_searching.clone();
        let b = self.board.clone();
        let at = self.attack_table.clone();
        let tt = self.transposition_table.clone();
        let depth = match search_method {
            SearchMethod::ToDepth(n) => n,
            SearchMethod::ToTime(_) => 1000000000,
        };
        let search_thread = std::thread::spawn(move || search_position(
            stop_flag, 
            b,
            at,
            depth, 
            tt,
        ));
        if let SearchMethod::ToTime(time) = search_method {
            thread::sleep(time);
            stop_searching.store(true, Ordering::Relaxed);
        }
        search_thread.join().unwrap()
        // search_position(stop_flag, &mut self.board, &self.attack_table, depth, &mut self.transposition_table)
    }

    pub fn string_to_move(&self, chess_move: &str) -> Result<Move, &'static str> {
        let board = self.board.lock().unwrap();
        string_to_move(chess_move, &board)
    }

    pub fn perft_debug(&mut self, depth: u32, verbose: bool) -> usize {
        let mut board = self.board.lock().unwrap();
        perft_debug(&mut board, &self.attack_table, depth, verbose)
    }

    pub fn perft(&mut self, depth: u32, verbose: bool) -> usize {
        let mut board = self.board.lock().unwrap();
        perft(&mut board, &self.attack_table, depth, verbose)
    }
}

impl fmt::Display for Engine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let board = self.board.lock().unwrap();
        writeln!(f, "{}", board)
    }
}