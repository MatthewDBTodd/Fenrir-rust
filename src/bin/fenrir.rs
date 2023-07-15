use std::io::{self, Write};
use std::time::Instant;
use std::time::Duration;
use clap::{Command, Arg, ArgAction};

use fenrir::engine::{Engine, SearchMethod, GameState};
use fenrir::shared_perft::*;

const VERSION: &str = "Fenrir-0.0003";

fn get_user_input(quiet: bool) -> String {
    let mut input = String::new();
    if !quiet {
        print!("> ");
    }
    io::stdout().flush().expect("couldn't flush stdout");
    io::stdin().read_line(&mut input).expect("couldn't read from stdin");
    input.trim().to_string()
}

fn play_game(engine: &mut Engine, quiet: bool) -> bool {
    let mut first_move = true;
    loop {
        if !quiet {
            println!("{engine}");
            println!("Eval = {}", engine.eval());
        }
        match engine.get_game_state() {
            GameState::WhiteCheckmatesBlack => { 
                if quiet {
                    println!("Game over: {}", GameState::WhiteCheckmatesBlack as usize);
                    break;
                } else {
                    println!("Game over: White wins"); break; 
                }
            },
            GameState::BlackCheckmatesWhite => { 
                if quiet {
                    println!("Game over: {}", GameState::BlackCheckmatesWhite as usize);
                    break;
                } else {
                    println!("Game over: Black wins"); break; 
                }
            },
            GameState::Stalemate => { 
                if quiet {
                    println!("Game over: {}", GameState::Stalemate as usize);
                    break;
                } else {
                    println!("Game over: Draw due to stalemate"); break; 
                }
            },
            GameState::ThreefoldRepetition => { 
                if quiet {
                    println!("Game over: {}", GameState::ThreefoldRepetition as usize);
                    break;
                } else {
                    println!("Game over: Draw due to threefold repetition"); break; 
                }
            },
            GameState::FiftyMoveRule => { 
                if quiet {
                    println!("Game over: {}", GameState::FiftyMoveRule as usize);
                    break;
                } else {
                    println!("Game over: Draw due to 50 move rule"); break; 
                }
            },
            GameState::Ongoing => {
                if first_move {
                    first_move = false;
                } else if quiet && !first_move {
                    println!("ok");
                }
            },
        }

        let input = get_user_input(quiet);

        if input.is_empty() {
            continue;
        } else if input == "quit" {
            return true;
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
                if !quiet {
                    println!("Searching for best move at depth {val}...");
                }
                SearchMethod::ToDepth(val)
            } else if method == "time" {
                if !quiet {
                    println!("Searching for best move for {val}ms...");
                }
                SearchMethod::ToTime(Duration::from_millis(val as u64))
            } else {
                println!("Invalid search method: {}", parts[1]);
                continue;
            };

            let start = Instant::now();

            let (best_move, eval, depth_searched) = engine.search_position(method, quiet);
            let duration = start.elapsed().as_secs_f64();
            let best_move = best_move.unwrap();
            if quiet {
                println!("{}", move_string(&best_move));
            } else {
                println!("Best move = {} with eval = {eval} at depth {depth_searched}. Found in {} seconds", move_string(&best_move), duration);
            }
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
            if !quiet {
                println!("{} -> {:?}", input, chess_move);
            }
            engine.make_move(chess_move);
        }
    }
    false
}

fn get_fen(quiet: bool) -> Option<String> {
    let fen = if !quiet {
        println!("Enter starting position: ");
        let fen = get_user_input(quiet);
        let fen = if fen.is_empty() {
            println!("Fen is empty, using starting position");
            None
        } else {
            Some(fen.to_string())
        };
        fen
    } else {
        None
    };
    fen
}

fn main() {
    let cli = Command::new("fenrir")
        .about("Runs the Fenrir chess engine")
        .arg(
            Arg::new("quiet")
            .short('q')
            .long("quiet")
            .action(ArgAction::SetTrue)
            .value_name("QUIET")
            .required(false)
        ).get_matches();
    
    let quiet = cli.get_flag("quiet");

    let fen = get_fen(quiet);

    let mut engine = Engine::new(fen.as_deref());
    println!("{VERSION}");
    loop {
        let should_quit = play_game(&mut engine, quiet);
        let mut pgn_moves = engine.generate_pgn_moves();
        match engine.get_game_state() {
            GameState::WhiteCheckmatesBlack => pgn_moves.push_str(&"1-0"),
            GameState::BlackCheckmatesWhite => pgn_moves.push_str(&"0-1"),
            GameState::Stalemate => {
                pgn_moves.push_str(&"{ Draw due to stalemate } ");
                pgn_moves.push_str(&"1/2-1/2");
            },
            GameState::ThreefoldRepetition => {
                pgn_moves.push_str(&"{ Draw due to three-fold repetition } ");
                pgn_moves.push_str(&"1/2-1/2");
            },
            GameState::FiftyMoveRule => {
                pgn_moves.push_str(&"{ Draw due to fifty move rule } ");
                pgn_moves.push_str(&"1/2-1/2");
            },
            GameState::Ongoing => (),
        }
        println!("{}", pgn_moves);

        if should_quit {
            break;
        }
        let fen = get_fen(quiet);
        engine.new_game(fen.as_deref());
    }
}
