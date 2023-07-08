use std::io::{self, Write};
use std::time::Instant;
use std::time::Duration;

use fenrir::engine::{Engine, SearchMethod, GameState};
use fenrir::shared_perft::*;


fn get_user_input() -> String {
    let mut input = String::new();
    print!("> ");
    io::stdout().flush().expect("couldn't flush stdout");
    io::stdin().read_line(&mut input).expect("couldn't read from stdin");
    input.trim().to_string()
}

fn main() {
    println!("Enter starting position fen: ");
    let fen = get_user_input();
    let fen = if fen.is_empty() {
        println!("Fen is empty, using starting position");
        None
    } else {
        Some(&fen[..])
    };

    let mut engine = Engine::new(fen);
    loop {
        println!("{engine}");
        println!("Eval = {}", engine.eval());
        match engine.get_game_state() {
            GameState::WhiteCheckmatesBlack => { println!("Game over: White wins"); break; },
            GameState::BlackCheckmatesWhite => { println!("Game over: Black wins"); break; },
            GameState::Stalemate => { println!("Game over: Draw due to stalemate"); break; },
            GameState::ThreefoldRepetition => { println!("Game over: Draw due to threefold repetition"); break; },
            GameState::FiftyMoveRule => { println!("Game over: Draw due to 50 move rule"); break; },
            _ => (),
        }

        let input = get_user_input();

        if input.is_empty() {
            continue;
        } else if input == "quit" {
            break;
        } else if input == "undo" {
            engine.undo_move();
        } else if input.starts_with("search") {
            let parts: Vec<&str> = input.split_whitespace().collect();
            assert!(parts.len() == 3);
            let val = parts[2].parse::<u32>();
            if val.is_err() {
                println!("Invalid value: {}", parts[2]);
                continue;
            }
            let val = val.unwrap();
            let method = parts[1];
            let method = if method == "depth" {
                println!("Searching for best move at depth {val}...");
                SearchMethod::ToDepth(val)
            } else if method == "time" {
                println!("Searching for best move for {val}ms...");
                SearchMethod::ToTime(Duration::from_millis(val as u64))
            } else {
                println!("Invalid search method: {}", parts[1]);
                continue;
            };

            let start = Instant::now();

            let (best_move, eval, depth_searched) = engine.search_position(method);
            let duration = start.elapsed().as_secs_f64();
            let best_move = best_move.unwrap();
            println!("Best move = {} with eval = {eval} at depth {depth_searched}. Found in {} seconds", move_string(&best_move), duration);
            engine.make_move(best_move);
        } else if input == "eval" {
            println!("Eval = {}", engine.eval());
        } else {
            let chess_move = match engine.string_to_move(&input) {
                Ok(m) => m,
                Err(e) => {
                    println!("Invalid input {} with error {}", input, e);
                    continue;
                },
            };
            println!("{} -> {:?}", input, chess_move);
            engine.make_move(chess_move);
        }
    }
}
