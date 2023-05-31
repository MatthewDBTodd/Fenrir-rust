use num_traits::FromPrimitive;
use clap::{Parser, Command, Arg, ArgAction};
use chess::{shared_perft::*, attack_table};
use chess::board::Board;
use chess::attack_table::AttackTable;
use chess::chess_move::{Move, MoveType};
use chess::Square;
use chess::Piece;

#[derive(Parser)]
struct Cli {
    depth: u8,
    fen: String,

}

fn string_to_square(square: &str) -> Result<Square, &'static str> {
    assert!(square.len() == 2);
    let file = square.chars().nth(0).unwrap();
    if file < 'a' || file > 'h' {
        return Err("Invalid file");
    }
    let file = file as u32 - 'a' as u32;
    let rank = square.chars().nth(1).unwrap().to_digit(10).ok_or("Non digit rank").unwrap();
    if !(1..=8).contains(&rank) {
        return Err("Invalid rank");
    }
    let square_idx = ((rank-1) * 8) + file;

    FromPrimitive::from_u32(square_idx).ok_or("Couldn't convert to square")

}

fn char_to_piece(piece: char) -> Result<Piece, &'static str> {
    match piece {
        'q' => Ok(Piece::Queen),
        'r' => Ok(Piece::Rook),
        'b' => Ok(Piece::Bishop),
        'n' => Ok(Piece::Knight),
        _ => Err("Invalid promotion piece"),
    }
}

fn string_to_move(chess_move: &str, board: &Board) -> Result<Move, &'static str> {
    if chess_move.len() != 4 && chess_move.len() != 5 {
        return Err("Invalid length");
    }
    let source_sq = string_to_square(&chess_move[..2])?;
    let dest_sq = string_to_square(&chess_move[2..4])?;
    let piece = board.bitboard.get_piece_type_for_square(source_sq)
        .ok_or("No piece on source square")?;
    let captured_piece = board.bitboard.get_piece_type_for_square(dest_sq);
    let move_type: MoveType = if chess_move.len() == 5 {
        assert!(piece == Piece::Pawn);
        let promotion_piece = char_to_piece(chess_move.chars().nth(4).unwrap())?;
        if captured_piece.is_some() {
            MoveType::CapturePromotion(captured_piece.unwrap(), promotion_piece)
        } else {
            MoveType::MovePromotion(promotion_piece)
        }
    } else if captured_piece.is_some() {
        if board.bitboard.square_empty(dest_sq) {
            assert!(captured_piece.unwrap() == Piece::Pawn);
            MoveType::EnPassant
        } else {
            MoveType::Capture(captured_piece.unwrap())
        }
    } else if piece == Piece::Pawn {
        let source_mask: u64 = 1 << source_sq as u32;
        let dest_mask: u64 = 1 << dest_sq as u32;
        if (source_mask & 0xFF00 != 0 && dest_mask & 0xFF000000 != 0) ||
            (source_mask & 0xFF000000000000 != 0 && dest_mask & 0xFF00000000 != 0)  {

            MoveType::DoublePawnPush
        } else {
            MoveType::Quiet
        }
    } else if piece == Piece::King {
        if (source_sq == Square::E1 && dest_sq == Square::G1) {
            MoveType::CastleKingSide
        } else if (source_sq == Square::E1 && dest_sq == Square::C1) {
            MoveType::CastleQueenSide
        } else if (source_sq == Square::E8 && dest_sq == Square::G8) {
            MoveType::CastleKingSide
        } else if (source_sq == Square::E8 && dest_sq == Square::C8) {
            MoveType::CastleQueenSide
        } else {
            MoveType::Quiet
        }
    } else {
        MoveType::Quiet
    };
    Ok(Move {
        source_sq,
        dest_sq,
        piece,
        move_type,
    })
}

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
            Arg::new("moves")
                .value_name("MOVES..")
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
