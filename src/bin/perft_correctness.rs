use std::time::Instant;

use clap::{Command, Arg, ArgAction};
use fenrir::engine::Engine;

fn main() {
    let cli = Command::new("perft")
        .about("Runs perft on the given fen to the given depth")
        .arg(
            Arg::new("depth")
                .short('d')
                .long("depth")
                .action(ArgAction::Set)
                .value_parser((clap::value_parser!(u32)))
                .value_name("DEPTH")
                .help("The depth to search on the given FEN string")
                .required(true)
        )
        .arg(
            Arg::new("fen")
                .short('f')
                .long("fen")
                .action(ArgAction::Set)
                .value_name("FEN")
                .help("The FEN string to search on")
                .default_value("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        )
        .arg(
            Arg::new("verbose")
                .short('v')
                .long("verbose")
                .action(ArgAction::SetTrue)
                .value_name("VERBOSE")
                .required(false)
        )
        .arg(
            Arg::new("print_timings")
                .short('t')
                .long("print-timings")
                .action(ArgAction::SetTrue)
                .value_name("PRINT_TIMINGS")
                .required(false)
        )
        .arg(
            Arg::new("moves")
                .value_name("MOVES")
                .required(false)
                .action(ArgAction::Set)
                .value_delimiter(' ')
                .num_args(0..)
        ).get_matches();

        let depth = cli.get_one::<u32>("depth").copied().unwrap();
        let fen = cli.get_one::<String>("fen").unwrap();
        let verbose = cli.get_flag("verbose");
        let print_timings = cli.get_flag("print_timings");
        let moves: Vec<String> = cli.get_many("moves")
            .unwrap_or_default()
            .cloned()
            .collect();

        if verbose {
            println!("depth = {depth}, fen = {fen}, verbose = {verbose}, moves = {:?}", moves);
        }
        let mut engine = Engine::new(Some(fen));
        if verbose {
            println!("Starting position:\n{engine}");
        }
        for chess_move in moves {
            let translated_move = match engine.string_to_move(&chess_move) {
                Ok(m) => m,
                Err(e) => panic!("{}", e),
            };
            if verbose {
                println!("{} -> {:?}", chess_move, translated_move);
            }
            engine.make_move(translated_move);
        }
        if verbose {
            println!("{engine}");
        }
        let start = Instant::now();
        let nodes = engine.perft_debug(depth, verbose);
        let duration = start.elapsed().as_secs_f64();
        let nodes_per_sec = (nodes as f64 / duration).round() as u64;
        println!("\n{nodes}");
        println!("\nFinished in {duration}s at {nodes_per_sec} nodes per second");


}
