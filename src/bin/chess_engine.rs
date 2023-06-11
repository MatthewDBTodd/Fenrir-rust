use std::io::{self, Write};
use std::time::Instant;

use chess::attack_table::AttackTable;
use chess::board::Board;
use chess::shared_perft::*;
use chess::search_move::*;


fn get_user_input() -> String {
    let mut input = String::new();
    print!("> ");
    io::stdout().flush().expect("couldn't flush stdout");
    io::stdin().read_line(&mut input).expect("couldn't read from stdin");
    input.trim().to_string()
}

fn main() {
    print!("Generating attack tables... ");
    std::io::stdout().flush().expect("Couldn't flush stdout");
    let attack_table = AttackTable::init();
    println!("done");
    println!("Enter starting position fen: ");
    let fen = get_user_input();
    let fen = if fen.is_empty() {
        println!("Fen is empty, using starting position");
        String::from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
    } else {
        fen
    };

    let mut board = Board::new(Some(&fen)).unwrap();

    loop {
        println!("{board}");
        let input = get_user_input();

        if input.is_empty() {
            continue;
        } else if input == "quit" {
            break;
        } else if input == "undo" {
            board.undo_move();
        } else if input.starts_with("gen") {
            let parts: Vec<&str> = input.split_whitespace().collect();
            assert!(parts.len() == 2);
            let depth = parts[1].parse::<u32>();
            if depth.is_ok() {
                let depth = depth.unwrap();
                println!("Searching for best move at depth {depth}...");
                let start = Instant::now();
                let (best_move, eval) = search_position(&mut board, &attack_table, depth);
                let duration = start.elapsed().as_secs_f64();
                println!("Best move = {} with eval = {eval}. Found in {} seconds", move_string(&best_move), duration);
                board.make_move(best_move);
            } else {
                println!("invalid input");
                continue;
            }
        } else {
            let chess_move = match string_to_move(&input, &board) {
                Ok(m) => m,
                Err(e) => panic!("{}", e),
            };
            println!("{} -> {:?}", input, chess_move);
            board.make_move(chess_move);
        }
    }
}
