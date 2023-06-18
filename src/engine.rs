use std::io::Write;
use std::fmt;
use std::rc::Rc;
use crate::attack_table::AttackTable;
use crate::board::Board;
use crate::chess_move::Move;
use crate::board_hash::ZobristHasher;
use crate::search_move::*;
use crate::shared_perft::*;

pub struct Engine {
    board: Board,
    attack_table: AttackTable,
    hasher: Rc<ZobristHasher>,
}

impl Engine {
    pub fn new(fen: Option<&str>) -> Self {
        // print!("Generating attack tables... ");
        // std::io::stdout().flush().expect("Couldn't flush stdout");
        let attack_table = AttackTable::init();
        // println!("done");
        // print!("Generating Zobrist Hasher...");
        // std::io::stdout().flush().expect("Couldn't flush stdout");
        let hasher = Rc::new(ZobristHasher::new());
        // println!("done");
        let board = Board::new(fen, hasher.clone()).unwrap();
        Self {
            board,
            attack_table,
            hasher,
        }
    }

    pub fn input_start_pos(&mut self, fen: &str) {
        let new_board = Board::new(Some(fen), self.hasher.clone()).unwrap();
        self.board = new_board;
    }

    pub fn starting_pos(&mut self) {
        let new_board = Board::new(None, self.hasher.clone()).unwrap();
        self.board = new_board;
    }

    pub fn make_move(&mut self, chess_move: Move) {
        self.board.make_move(chess_move);
    }

    pub fn undo_move(&mut self) {
        self.board.undo_move();
    }

    pub fn search_position(&mut self, depth: u32) -> (Move, i32) {
        search_position(&mut self.board, &self.attack_table, depth)
    }

    pub fn string_to_move(&self, chess_move: &str) -> Result<Move, &'static str> {
        string_to_move(chess_move, &self.board)
    }

    pub fn perft_debug(&mut self, depth: u32, verbose: bool) -> usize {
        perft_debug(&mut self.board, &self.attack_table, depth, verbose)
    }

    pub fn perft(&mut self, depth: u32, verbose: bool) -> usize {
        perft(&mut self.board, &self.attack_table, depth, verbose)
    }
}

impl fmt::Display for Engine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self.board)
    }
}