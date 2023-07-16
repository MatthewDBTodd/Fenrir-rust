use std::fmt;
use std::sync::{Arc, Mutex, Condvar, atomic::{AtomicBool, Ordering}};
use std::time::Duration;

use crate::attack_table::AttackTable;
use crate::board::Board;
use crate::chess_move::Move;
use crate::board_hash::ZobristHasher;
use crate::eval::eval_position;
use crate::search_move::*;
use crate::shared_perft::*;
use crate::transposition_table::TranspositionTable;
use crate::Colour;
use crate::pgn::*;

pub struct Engine {
    board: Board,
    attack_table: Arc<AttackTable>,
    hasher: Arc<ZobristHasher>,
    transposition_table: Arc<TranspositionTable>,
    legal_moves: LegalMoves,
    pgn_move_history: Vec<String>,
    from_perspective: Colour,
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
    WhiteCheckmatesBlack,
    BlackCheckmatesWhite,
    Stalemate,
    ThreefoldRepetition,
    FiftyMoveRule,
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

        let transposition_table = Arc::new(TranspositionTable::new(1 << 20));

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
            pgn_move_history: Vec::new(),
            from_perspective: Colour::White,
        }
    }

    pub fn new_game(&mut self, fen: Option<&str>) {
        let board = Board::new(fen, self.hasher.clone()).unwrap();

        let num = self.attack_table.generate_legal_moves(
            &board, board.turn_colour, &mut self.legal_moves.move_list
        );
        self.legal_moves.num = num;
        let transposition_table = Arc::new(TranspositionTable::new(1 << 20));
        self.transposition_table = transposition_table;
        self.board = board;
        self.pgn_move_history.clear();
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
        let legal_moves: Vec<Move> = self.legal_moves.move_list[0..self.legal_moves.num]
            .to_vec();
        self.board.make_move(chess_move);
        self.generate_legal_moves();
        let move_state = if self.attack_table.king_in_check(&self.board) {
            if self.legal_moves.num == 0 {
                Some(MoveState::Checkmate)
            } else {
                Some(MoveState::Check)
            }
        } else {
            None
        };
        self.pgn_move_history.push(
            generate_move_notation(&chess_move, legal_moves, move_state)
        );
    }

    pub fn undo_move(&mut self) {
        self.board.undo_move();
        self.generate_legal_moves();
        self.pgn_move_history.pop();
    }

    fn generate_legal_moves(&mut self) {
        let n = self.attack_table.generate_legal_moves(
            &self.board, self.board.turn_colour, &mut self.legal_moves.move_list
        );
        self.legal_moves.num = n;
    }

    pub fn search_position(
        &mut self, search_method: SearchMethod, quiet: bool, num_threads: i32,
    ) -> (Option<Move>, i32, u32) 
    {
        let stop_searching = Arc::new(AtomicBool::new(false));
        let pair = Arc::new((Mutex::new(()), Condvar::new()));
        let mut threads = Vec::new();
        for i in 0..num_threads {
            let legal_moves = self.legal_moves.clone();
            let stop_flag = stop_searching.clone();
            let pair2 = pair.clone();
            let b = self.board.clone();
            let at = self.attack_table.clone();
            let tt = self.transposition_table.clone();
            let starting_depth: u32 = ((i % 4) + 1) as u32;
            let depth = match search_method {
                SearchMethod::ToDepth(n) => n,
                SearchMethod::ToTime(_) => 1000000000,
            };
            threads.push(
                std::thread::spawn(move || search_position(
                    legal_moves,
                    stop_flag, 
                    b,
                    at,
                    starting_depth,
                    depth, 
                    tt,
                    pair2,
                    quiet,
                )
            ));
            std::thread::sleep(std::time::Duration::from_millis(100));
        }

        if let SearchMethod::ToTime(time) = search_method {
            let guard = pair.0.lock().unwrap();
            let _rv = pair.1.wait_timeout(guard, time).unwrap();
            stop_searching.store(true, Ordering::Relaxed);
        }
        let mut best_move: Option<Move> = None;
        let mut best_eval: i32 = 0;
        let mut best_depth: u32 = 0;
        let mut thread_num = 1;
        for handle in threads {
            let (m, eval, depth) = handle.join().unwrap();
            println!("Thread {} exited with result: best-move = {:?}, eval = {}, depth = {}", thread_num, m, eval, depth);
            if depth > best_depth {
                best_depth = depth;
                best_eval = eval;
                best_move = m;
            }
            thread_num += 1;
        }
        (best_move, best_eval, best_depth)
        // search_position(stop_flag, &mut self.board, &self.attack_table, depth, &mut self.transposition_table)
    }

    pub fn get_game_state(&self) -> GameState {
        if self.legal_moves.num == 0 {
            if self.attack_table.king_in_check(&self.board) {
                match self.board.turn_colour {
                    Colour::White => return GameState::BlackCheckmatesWhite,
                    Colour::Black => return GameState::WhiteCheckmatesBlack,
                }
            } else {
                return GameState::Stalemate;
            }
        } else if self.board.is_threefold_repetition() {
            return GameState::ThreefoldRepetition;
        }  else if self.board.half_move_num >= 50 {
            return GameState::FiftyMoveRule;
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

    pub fn eval(&self) -> i32 {
        eval_position(&self.board, &self.attack_table)
    }

    pub fn flip_board(&mut self) {
        if self.from_perspective == Colour::White {
            self.from_perspective = Colour::Black;
        } else {
            self.from_perspective = Colour::White;
        }
    }

    pub fn generate_pgn_moves(&self) -> String {
        let mut rv = String::new();
        let mut move_num = 1;
        for i in 0..self.pgn_move_history.len() {
            if i % 2 == 0 {
                rv.push_str(&move_num.to_string());
                rv.push_str(&". ");
                move_num += 1;
            }

            rv.push_str(&self.pgn_move_history[i]);
            rv.push(' ');
        }
        rv
    }
}

impl fmt::Display for Engine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.board.print(f, self.from_perspective)
    }
}