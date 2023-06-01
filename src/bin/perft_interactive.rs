use clap::{Command, Arg, ArgAction};
use chess::{shared_perft::*};
use chess::board::Board;
use chess::attack_table::AttackTable;
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
        let attack_table = AttackTable::init();
        let mut board = Board::new(Some(fen)).unwrap();

        loop {
            println!("{board}");
            let input = get_user_input();
            if input == "quit" {
                break;
            } else if input == "undo" {
                board.undo_move();
            } else if input.starts_with("gen") {
                let parts: Vec<&str> = input.split_whitespace().collect();
                assert!(parts.len() == 2);
                let depth = parts[1].parse::<u32>();
                if depth.is_ok() {
                    let nodes = perft_debug(&mut board, &attack_table, depth.unwrap(), true);
                    println!("{nodes} nodes");
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