use clap::{Command, Arg, ArgAction};
use chess::{shared_perft::*};
use chess::board::Board;
use chess::attack_table::AttackTable;

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
            Arg::new("interactive")
                .short('i')
                .long("interactive")
                .action(ArgAction::SetTrue)
                .value_name("INTERACTIVE")
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
        let moves: Vec<String> = cli.get_many("moves")
            .unwrap_or_default()
            .cloned()
            .collect();

        if verbose {
            println!("depth = {depth}, fen = {fen}, verbose = {verbose}, moves = {:?}", moves);
        }
        let attack_table = AttackTable::init();
        let mut board = Board::new(Some(fen)).unwrap();
        if verbose {
            println!("Starting position:\n{}", board);
        }
        for chess_move in moves {
            let translated_move = match string_to_move(&chess_move, &board) {
                Ok(m) => m,
                Err(e) => panic!("{}", e),
            };
            if verbose {
                println!("{} -> {:?}", chess_move, translated_move);
            }
            board.make_move(translated_move);
        }
        if verbose {
            println!("{}", board);
        }
        let nodes = perft_debug(&mut board, &attack_table, depth, verbose);
        println!("\n{nodes}")

}