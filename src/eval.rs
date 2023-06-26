use crate::{board::Board, Colour, chess_move::Move, Piece, attack_table::AttackTable};

pub const CHECKMATE: i32 = 100_000;
pub const DRAW: i32 = 0;

pub fn eval_position(board: &Board, attack_table: &AttackTable) -> i32 {
    // let mut move_list = [Move::default(); 256];
    // let white_legal_moves = attack_table.generate_legal_moves(board, Colour::White, &mut move_list);
    // let black_legal_moves = attack_table.generate_legal_moves(board, Colour::Black, &mut move_list);
    // println!("----------------------------------------------------");
    // println!("BOARD EVAL");
    // println!("{}", board);
    let white_rough_moves = attack_table.get_num_legal_moves(board, Colour::White);
    let black_rough_moves = attack_table.get_num_legal_moves(board, Colour::Black);
    // println!("white legal/rough = {} {}", white_legal_moves, white_rough_moves);
    // println!("black legal/rough = {} {}", black_legal_moves, black_rough_moves);
    if board.turn_colour == Colour::White && white_rough_moves == 0 {
        // hack for now, there may actually be a legal en_passant, have to check for that
        let mut move_list = [Move::default(); 256];
        let legal_moves = attack_table.generate_legal_moves(board, Colour::White, &mut move_list);
        if legal_moves == 0 && attack_table.king_in_check(board) {
            return -CHECKMATE;
        } else if legal_moves == 0 {
            return DRAW;
        }
    } else if board.turn_colour == Colour::Black && black_rough_moves == 0 {
        // hack for now, there may actually be a legal en_passant, have to check for that
        let mut move_list = [Move::default(); 256];
        let legal_moves = attack_table.generate_legal_moves(board, Colour::Black, &mut move_list);
        if legal_moves == 0 && attack_table.king_in_check(board) {
            return CHECKMATE;
        } else if legal_moves == 0 {
            return DRAW;
        }
    }
    let mut eval: i32 = 0;
    let mut w_pst_eval = 0;
    let mut b_pst_eval = 0;
    for (piece_type, weight) in [
        (Piece::Queen, 900),
        (Piece::Rook, 500),
        (Piece::Bishop, 330),
        (Piece::Knight, 320),
        (Piece::Pawn, 100),
    ] {
        let mut white_pieces = board.bitboard.get_colour_piece_mask(piece_type, Colour::White);
        let white_count = white_pieces.count_ones();
        while white_pieces != 0 {
            let white_piece = white_pieces & white_pieces.wrapping_neg();
            white_pieces ^= white_piece;

            w_pst_eval += (PIECE_SQUARE_TABLES[Colour::White as usize][piece_type as usize][white_piece.trailing_zeros() as usize]);
        }


        let mut black_pieces = board.bitboard.get_colour_piece_mask(piece_type, Colour::Black);
        let black_count = black_pieces.count_ones();
        while black_pieces != 0 {
            let black_piece = black_pieces & black_pieces.wrapping_neg();
            black_pieces ^= black_piece;

            b_pst_eval += (PIECE_SQUARE_TABLES[Colour::Black as usize][piece_type as usize][black_piece.trailing_zeros() as usize]);
        }

        let val = weight * (white_count as i32 - black_count as i32);
        // println!("{:?} {} * ({} - {}) = {}", piece_type, weight, white_count, black_count, val);
        eval += val;
        // eval += weight * (white_count as i32 - black_count as i32);
    }

    let val = w_pst_eval - b_pst_eval;
    eval += val;
    // println!("\nmobility:");
    let val = 10 * (white_rough_moves as i32 - black_rough_moves as i32);
    // println!("10 * ({} - {}) = {}", white_rough_moves, black_rough_moves, val);
    eval += val;
    // eval += 10 * (white_rough_moves as i32 - black_rough_moves as i32);

    let (w_doubled, w_isolated) = pawn_eval(board, Colour::White);
    let (b_doubled, b_isolated) = pawn_eval(board, Colour::Black);
    // let pawns = 5 * ((w_doubled - b_doubled) + (w_isolated - b_isolated)) as i32;
    let pawns = 50 * ((w_doubled - b_doubled) + (w_isolated - b_isolated)) as i32;
    // println!("\npawns:");
    // println!("5 * (({} - {}) + ({} - {})) = {}", w_doubled, b_doubled, w_isolated, b_isolated, pawns);
    eval += pawns;
    eval = match board.turn_colour {
        Colour::White => 1 * eval,
        Colour::Black => -1 * eval,
    };
    // println!("\nFinal eval = {}", eval);
    eval
}

// returns doubled and isolated pawns
fn pawn_eval(board: &Board, colour: Colour) -> (u32, u32) {
    let pawns = board.bitboard.get_colour_piece_mask(Piece::Pawn, colour);
    let mut doubled_pawns: u32 = 0;
    let mut isolated_pawns: u32 = 0;
    let mut pawns_tmp = pawns;
    while pawns_tmp != 0 {
        let pawn = pawns_tmp & pawns_tmp.wrapping_neg();
        pawns_tmp ^= pawn;
        let idx = pawn.trailing_zeros() as usize;
        let pawns_on_file = pawn & FILES[idx];
        pawns_tmp = pawns_tmp & !pawns_on_file;
        let num_pawns_on_file = pawns_on_file.count_ones();
        doubled_pawns += num_pawns_on_file - 1;

        if pawns & ISOLATED[idx] == 0 {
            isolated_pawns += num_pawns_on_file;
        }
    }
    (doubled_pawns, isolated_pawns)
}

// indexed by [colour][piece][square]
#[rustfmt::skip]
const PIECE_SQUARE_TABLES: [[[i32; 64]; 6]; 2] = [
    // white
    [
        // pawns
        [
            0, 0, 0, 0, 0, 0, 0, 0,
            5, 10, 10,-20,-20, 10, 10, 5,
            5, -5,-10, 0, 0,-10, -5, 5,
            0,  0,  0, 20, 20,  0,  0,  0,
            5,  5, 10, 25, 25, 10,  5,  5,
            10, 10, 20, 30, 30, 20, 10, 10,
            50, 50, 50, 50, 50, 50, 50, 50,
            0, 0, 0, 0, 0, 0, 0, 0,
        ],
        // king
        [
            20, 30, 10,  0,  0, 10, 30, 20,
            20, 20,  0,  0,  0,  0, 20, 20,
            -10,-20,-20,-20,-20,-20,-20,-10,
            -20,-30,-30,-40,-40,-30,-30,-20,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
        ],
        // queen
        [
            -20,-10,-10, -5, -5,-10,-10,-20,
            -10,  0,  5,  0,  0,  0,  0,-10, 
            -10,  5,  5,  5,  5,  5,  0,-10,
            0,  0,  5,  5,  5,  5,  0, -5,
            -5,  0,  5,  5,  5,  5,  0, -5,
            -10,  0,  5,  5,  5,  5,  0,-10,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -20,-10,-10, -5, -5,-10,-10,-20,
        ],
        // bishop
        [
            -20,-10,-10,-10,-10,-10,-10,-20,
            -10,  5,  0,  0,  0,  0,  5,-10,
            -10, 10, 10, 10, 10, 10, 10,-10,
            -10,  0, 10, 10, 10, 10,  0,-10,
            -10,  5,  5, 10, 10,  5,  5,-10,
            -10,  0,  5, 10, 10,  5,  0,-10,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -20,-10,-10,-10,-10,-10,-10,-20,
        ],
        // knight
        [
            -50,-40,-30,-30,-30,-30,-40,-50,
            -40,-20,  0,  5,  5,  0,-20,-40,
            -30,  5, 10, 15, 15, 10,  5,-30,
            -30,  0, 15, 20, 20, 15,  0,-30,
            -30,  5, 15, 20, 20, 15,  5,-30,
            -30,  0, 10, 15, 15, 10,  0,-30,
            -40,-20,  0,  0,  0,  0,-20,-40,
            -50,-40,-30,-30,-30,-30,-40,-50,
        ],
        // rook
        [
            0,  0,  0,  5,  5,  0,  0,  0,
            -5,  0,  0,  0,  0,  0,  0, -5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            5, 10, 10, 10, 10, 10, 10,  5,
            0,  0,  0,  0,  0,  0,  0,  0,
        ],
    ],
    // black
    [
        // pawns
        [
            0,  0,  0,  0,  0,  0,  0,  0,
            50, 50, 50, 50, 50, 50, 50, 50,
            10, 10, 20, 30, 30, 20, 10, 10,
            5,  5, 10, 25, 25, 10,  5,  5,
            0,  0,  0, 20, 20,  0,  0,  0,
            5, -5,-10,  0,  0,-10, -5,  5,
            5, 10, 10,-20,-20, 10, 10,  5,
            0,  0,  0,  0,  0,  0,  0,  0
        ],
        // king
        [
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -30,-40,-40,-50,-50,-40,-40,-30,
            -20,-30,-30,-40,-40,-30,-30,-20,
            -10,-20,-20,-20,-20,-20,-20,-10,
            20, 20,  0,  0,  0,  0, 20, 20,
            20, 30, 10,  0,  0, 10, 30, 20
        ],
        // queen
        [ 
            -20,-10,-10, -5, -5,-10,-10,-20,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -10,  0,  5,  5,  5,  5,  0,-10,
            -5,  0,  5,  5,  5,  5,  0, -5,
            0,  0,  5,  5,  5,  5,  0, -5,
            -10,  5,  5,  5,  5,  5,  0,-10,
            -10,  0,  5,  0,  0,  0,  0,-10,
            -20,-10,-10, -5, -5,-10,-10,-20
        ],
        // bishop
        [
            -20,-10,-10,-10,-10,-10,-10,-20,
            -10,  0,  0,  0,  0,  0,  0,-10,
            -10,  0,  5, 10, 10,  5,  0,-10,
            -10,  5,  5, 10, 10,  5,  5,-10,
            -10,  0, 10, 10, 10, 10,  0,-10,
            -10, 10, 10, 10, 10, 10, 10,-10,
            -10,  5,  0,  0,  0,  0,  5,-10,
            -20,-10,-10,-10,-10,-10,-10,-20,
        ],
        // knight
        [
            -50,-40,-30,-30,-30,-30,-40,-50,
            -40,-20,  0,  0,  0,  0,-20,-40,
            -30,  0, 10, 15, 15, 10,  0,-30,
            -30,  5, 15, 20, 20, 15,  5,-30,
            -30,  0, 15, 20, 20, 15,  0,-30,
            -30,  5, 10, 15, 15, 10,  5,-30,
            -40,-20,  0,  5,  5,  0,-20,-40,
            -50,-40,-30,-30,-30,-30,-40,-50,
        ],
        // rook
        [
            0,  0,  0,  0,  0,  0,  0,  0,
            5, 10, 10, 10, 10, 10, 10,  5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            -5,  0,  0,  0,  0,  0,  0, -5,
            0,  0,  0,  5,  5,  0,  0,  0
        ]
    ]
];

#[rustfmt::skip]
const FILES: [u64; 64] = [
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
    0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
    0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080,
];

const ISOLATED: [u64; 64] = [
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
    0x202020202020202, 0x505050505050505, 0xa0a0a0a0a0a0a0a, 0x1414141414141414,
    0x2828282828282828, 0x5050505050505050, 0xa0a0a0a0a0a0a0a0, 0x4040404040404040,
];
