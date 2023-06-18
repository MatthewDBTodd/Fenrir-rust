use std::io::{self, Write};
use std::time::Instant;
use fenrir::engine::Engine;
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
        let input = get_user_input();

        if input.is_empty() {
            continue;
        } else if input == "quit" {
            break;
        } else if input == "undo" {
            engine.undo_move();
        } else if input.starts_with("gen") {
            let parts: Vec<&str> = input.split_whitespace().collect();
            assert!(parts.len() == 2);
            let depth = parts[1].parse::<u32>();
            if depth.is_ok() {
                let depth = depth.unwrap();
                println!("Searching for best move at depth {depth}...");
                let start = Instant::now();
                let (best_move, eval) = engine.search_position(depth);
                let duration = start.elapsed().as_secs_f64();
                println!("Best move = {} with eval = {eval}. Found in {} seconds", move_string(&best_move), duration);
                engine.make_move(best_move);
            } else {
                println!("invalid input");
                continue;
            }
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
