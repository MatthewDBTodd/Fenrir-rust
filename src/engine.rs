use std::fmt;
use std::sync::{Arc, Mutex, Condvar, atomic::{AtomicBool, Ordering}};
use std::time::Duration;

use crate::attack_table::AttackTable;
use crate::board::Board;
use crate::chess_move::Move;
use crate::board_hash::ZobristHasher;
use crate::search_move::*;
use crate::shared_perft::*;
use crate::transposition_table::TranspositionTable;
use crate::Colour;

pub struct Engine {
    board: Board,
    attack_table: Arc<AttackTable>,
    hasher: Arc<ZobristHasher>,
    transposition_table: Arc<Mutex<TranspositionTable>>,
    legal_moves: LegalMoves,
}

#[derive(PartialEq)]
pub enum SearchMethod {
    ToDepth(u32),
    ToTime(Duration),
}

#[derive(Debug, Clone)]
pub struct LegalMoves {
    pub move_list: [Move; 256],
    pub num: usize,
}

// TODO: Add specific draw conditions like stalemate, 50 move rule
#[derive(Debug, PartialEq)]
pub enum GameState {
    Ongoing,
    WhiteVictory,
    BlackVictory,
    Draw,
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
        let board = Board::new(fen, hasher.clone()).unwrap();

        let transposition_table = Arc::new(Mutex::new(TranspositionTable::new(1 << 20)));

        let mut move_list = [Move::default(); 256];
        let num = attack_table.generate_legal_moves(&board, board.turn_colour, &mut move_list);

        let legal_moves = LegalMoves {
            move_list,
            num,
        };

        Self {
            board,
            attack_table,
            hasher,
            transposition_table,
            legal_moves,
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
        self.generate_legal_moves();
    }

    pub fn undo_move(&mut self) {
        self.board.undo_move();
        self.generate_legal_moves();
    }

    fn generate_legal_moves(&mut self) {
        let n = self.attack_table.generate_legal_moves(
            &self.board, self.board.turn_colour, &mut self.legal_moves.move_list
        );
        self.legal_moves.num = n;
    }

    pub fn search_position(&mut self, search_method: SearchMethod,) -> (Option<Move>, i32, u32) {
        let legal_moves = self.legal_moves.clone();
        let stop_searching = Arc::new(AtomicBool::new(false));
        let stop_flag = stop_searching.clone();
        let pair = Arc::new((Mutex::new(()), Condvar::new()));
        let pair2 = pair.clone();
        let b = self.board.clone();
        let at = self.attack_table.clone();
        let tt = self.transposition_table.clone();
        let depth = match search_method {
            SearchMethod::ToDepth(n) => n,
            SearchMethod::ToTime(_) => 1000000000,
        };
        let search_thread = std::thread::spawn(move || search_position(
            legal_moves,
            stop_flag, 
            b,
            at,
            depth, 
            tt,
            pair2,
        ));
        if let SearchMethod::ToTime(time) = search_method {
            let guard = pair.0.lock().unwrap();
            let _rv = pair.1.wait_timeout(guard, time).unwrap();
            stop_searching.store(true, Ordering::Relaxed);
        }
        search_thread.join().unwrap()
        // search_position(stop_flag, &mut self.board, &self.attack_table, depth, &mut self.transposition_table)
    }

    pub fn get_game_state(&self) -> GameState {
        if self.legal_moves.num == 0 {
            if self.attack_table.king_in_check(&self.board) {
                match self.board.turn_colour {
                    Colour::White => return GameState::BlackVictory,
                    Colour::Black => return GameState::WhiteVictory,
                }
            } else {
                return GameState::Draw;
            }
        } else {
            return GameState::Ongoing;
        }
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