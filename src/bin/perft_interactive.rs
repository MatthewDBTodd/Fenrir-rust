use clap::{Command, Arg, ArgAction};
use fenrir::engine::Engine;
use std::io::{self, Write};

fn get_user_input() -> String {
    let mut input = String::new();
    print!("> ");
    io::stdout().flush().expect("couldn't flush stdout");
    io::stdin().read_line(&mut input).expect("couldn't read from stdin");
    input.trim().to_string()
}

fn main() {
    let cli = Command::new("perft")
        .about("Interactive move gen")
        .arg(
            Arg::new("fen")
                .short('f')
                .long("fen")
                .action(ArgAction::Set)
                .value_name("FEN")
                .help("The FEN string to search on")
                .default_value("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        ).get_matches();

        let fen = cli.get_one::<String>("fen").unwrap();
        let mut engine = Engine::new(Some(fen));

        loop {
            println!("{engine}");
            let input = get_user_input();
            if input == "quit" {
                break;
            } else if input == "undo" {
                engine.undo_move();
            } else if input.starts_with("gen") {
                let parts: Vec<&str> = input.split_whitespace().collect();
                assert!(parts.len() == 2);
                let depth = parts[1].parse::<u32>();
                if depth.is_ok() {
                    let nodes = engine.perft_debug(depth.unwrap(), false);
                    println!("{nodes} nodes");
                } else {
                    println!("invalid input");
                    continue;
                }
            } else {
                let chess_move = match engine.string_to_move(&input) {
                    Ok(m) => m,
                    Err(e) => panic!("{}", e),
                };
                println!("{} -> {:?}", input, chess_move);
                engine.make_move(chess_move);
            }
        }
}