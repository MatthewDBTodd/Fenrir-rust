use num_traits::FromPrimitive;

use crate::board::Board;
use crate::pawn::PawnAttackTable;
use crate::sliding_piece::Magic;
use crate::{king, knight, Piece, Colour, Square};
use crate::chess_move::{Move, MoveType};
use crate::bitboard::BitBoard;
use crate::test_helpers::bitmask_to_board;
include!("blocker_table.rs");

pub struct AttackTable {
    king: Vec<u64>,
    knight: Vec<u64>,
    pawn_moves: [Vec<u64>; 2],
    pawn_attacks: [Vec<u64>; 2],
    bishop_attacks: Vec<Magic>,
    rook_attacks: Vec<Magic>,
}

pub struct BoardStatus {
    friendly_colour: Colour,
    enemy_colour: Colour,
    // danger squares are attacked squares but also including rays behind the king
    // as they're used to show squares that would leave the king in check
    danger_squares: u64,
    // mask of all the pinned pieces
    pinned_pieces: u64,
    // Key is the u64 mask of a friendly piece that is pinned to the king
    // Value is the u64 mask of all the pseudo-legal squares that piece can move to
    // while not leaving the king in check. As it's pseudo-legal squares, in 
    // reality the pinned might not have any legal moves. This must be checked later
    pinned_pseudo_legal_squares: [(u64, u64); 8],
    // a list of tuples where the first element is the attacking piece, the second
    // value is a bitmask of an enemy piece who currently has the king in check.
    // The third value is a mask of all the squares that a friendly piece could 
    // move to to block the checking piece
    king_attacking_pieces: [(Piece, u64, u64); 2],
    num_checking_pieces: u8,
    
    
    // a list of pairs where the first element is a bitmask of a friendly piece
    // who can potentially move and cause a discovered check
    // The second value is a mask of all the pseudo-legal squares that piece can
    // move to to initiate the discovered check. As it's pseudo-legal squares, in
    // reality the piece might not legally be able to move out of the way. This
    // must be checked later
    // discovered_check_pseudo_legal_squares: [(Piece, u64, u64); 8],
    // num_discovered_checks: u8,
    
    // a list of squares each friendly piece can move to to put the king in check,
    // indexed by Piece
    // check_squares: [u64; 6],
}

struct MoveList<'a> {
    moves: &'a mut [Move; 256],
    num_moves: usize,
}

impl<'a> MoveList<'a> {
    fn new(moves: &'a mut [Move; 256]) -> Self {
        Self {
            moves,
            num_moves: 0,
        }
    }

    fn push(&mut self, chess_move: Move) {
        self.moves[self.num_moves] = chess_move;
        self.num_moves += 1;
    }

    fn get_idx(&self) -> usize {
        self.num_moves
    }

    fn get_slice(&mut self, start: usize) -> &mut [Move] {
        &mut self.moves[start..self.num_moves]
    }
}

impl AttackTable {
    pub fn init() -> Self {
        let pawn = PawnAttackTable::init();
        Self {
            king: king::init(),
            knight: knight::init(),
            pawn_moves: pawn.moves,
            pawn_attacks: pawn.attacks,
            bishop_attacks: Magic::init(Piece::Bishop),
            rook_attacks: Magic::init(Piece::Rook),
        }
    }
    
    // even though we can get turn colour from the board, we want to pass it in to allow for 
    // position eval at leaf nodes, where part of the function is to evaluate number of legal moves
    // for both sides
    pub fn generate_legal_moves(&self, board: &Board, friendly_colour: Colour, 
                                moves: &mut [Move; 256]) -> usize {

        let mut moves = MoveList::new(moves);
        let board_status = self.get_board_status(&board.bitboard, friendly_colour,);
        
        // first check if king is in double check
        // means we only generate king moves
        if board_status.num_checking_pieces == 2 {
            self.get_king_captures(board, &mut moves, &board_status);
            self.get_king_moves(board, &mut moves, &board_status);
            return moves.num_moves;
        } else if board_status.num_checking_pieces == 1 {
            self.get_out_of_single_check(board, &mut moves, &board_status);
            return moves.num_moves;
        }
        // We already know if reaching this point the king is not in check
        // then for kingside/queenside, if it's still enabled, then check if the
        // squares between the king and rook are empty, and that the intermediate squares are not
        // intersecting with the danger squares
        // if castling is still enabled for a side, then we already know the rook and king are still
        // on their origin squares, as board.make_move() disables castling if the king is ever moved
        // and if the rooks ever move/are captured
        self.get_legal_castling_rights(board, &mut moves, &board_status);
        
        if let Some(ep_square) = board.en_passant {
            self.get_en_passant_moves(ep_square, board, &mut moves, &board_status);
        }
        
        // Captures
        self.get_captures(board, &mut moves, &board_status);
        self.get_king_captures(&board, &mut moves, &board_status);
        
        // Quiet moves
        self.get_quiet_moves(&board, &mut moves, &board_status);
        self.get_king_moves(&board, &mut moves, &board_status);

        moves.num_moves
            
    }

    // we ignore promotions and en_passant for speed, this is meant to be a rough count
    pub fn get_num_legal_moves(&self, board: &Board, colour: Colour) -> u32 {
        let board_status = self.get_board_status(&board.bitboard, colour);
        let occupied = board.bitboard.get_entire_mask();
        let friendly_pieces = board.bitboard.get_colour_mask(colour);
        if board_status.num_checking_pieces == 2 {
            let king_idx = board.bitboard.get_colour_piece_mask(Piece::King, colour)
                .trailing_zeros() as usize;
            let king_moves = self.king[king_idx] & !board_status.danger_squares &
                !friendly_pieces;
            // println!("double check = {}", bitmask_to_board(king_moves));
            return king_moves.count_ones();
        } else if board_status.num_checking_pieces == 1 {
            let mut move_count: u32 = 0;
            let king_idx = board.bitboard.get_colour_piece_mask(Piece::King, colour)
                .trailing_zeros() as usize;
            let king_moves = self.king[king_idx] & !board_status.danger_squares &
                !friendly_pieces;
            move_count += king_moves.count_ones();

            let (_, checking_piece_mask, block_squares) = board_status.king_attacking_pieces[0];
            let legal_squares = checking_piece_mask | block_squares;
            for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
                let mut all_pieces = board.bitboard.get_colour_piece_mask(piece_type, board_status.friendly_colour);
                
                // a pinned piece can't do anything about stopping check, remove them
                all_pieces &= !board_status.pinned_pieces;

                while all_pieces != 0 {
                    let one_piece = all_pieces & all_pieces.wrapping_neg();
                    all_pieces ^= one_piece;
                    let legal_moves = self.get_single_piece_pseudo_attacks(
                        piece_type, colour, one_piece, occupied
                    ) & !friendly_pieces & legal_squares;
                    move_count += legal_moves.count_ones();
                }
            }
            return move_count;
        }
        let mut move_count: u32 = 0;
        for piece in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
            let mut all_pieces = board.bitboard.get_colour_piece_mask(piece, colour); 
            while all_pieces != 0 {
                let one_piece = all_pieces & all_pieces.wrapping_neg();
                all_pieces ^= one_piece;
                // println!("{}", bitmask_to_board(one_piece));

                let pseudo_moves = if piece == Piece::Pawn {
                    let enemy_mask = board.bitboard.get_colour_mask(!&colour);
                    self.get_single_piece_moves(
                        Piece::Pawn, colour, one_piece, occupied,
                    ) | self.get_single_piece_captures(
                        Piece::Pawn, colour, one_piece, occupied, enemy_mask 
                    )
                } else {
                    self.get_single_piece_pseudo_attacks(
                        piece, colour, one_piece, occupied
                    ) & !friendly_pieces
                };
                let legal_moves = if one_piece & board_status.pinned_pieces != 0 {
                    pseudo_moves & board_status.pinned_pseudo_legal_squares
                        .iter()
                        .find(|&&(first, _)| first == one_piece)
                        .map(|&(_, second)| second)
                        .unwrap()
                } else {
                    pseudo_moves
                };
                // println!("{}", bitmask_to_board(legal_moves));
                // println!("================================");
                move_count += legal_moves.count_ones();
            }
        }
        let king_idx = board.bitboard.get_colour_piece_mask(Piece::King, colour)
            .trailing_zeros() as usize;
        let king_moves = self.king[king_idx] & !board_status.danger_squares & 
            !friendly_pieces;
        // println!("king moves = {}", bitmask_to_board(king_moves));
        move_count += king_moves.count_ones();

        let (mut kingside, mut queenside) = board.castling_rights.can_castle(board_status.friendly_colour);
        if kingside {
            let king_travel_squares: u64 = match board_status.friendly_colour {
                Colour::White => 0x60,
                Colour::Black => 0x6000000000000000,
            };
            kingside = king_travel_squares & board.bitboard.get_entire_mask() == 0 
                    && king_travel_squares & board_status.danger_squares == 0;
        }
        
        if queenside {
            let (in_between_squares, king_travel_squares): (u64, u64) = match board_status.friendly_colour {
                Colour::White => (0xE, 0xC),
                Colour::Black => (0xE00000000000000, 0xC00000000000000),
            };
            queenside = in_between_squares & board.bitboard.get_entire_mask() == 0
                     && king_travel_squares & board_status.danger_squares == 0;
        }
        if kingside {
            move_count += 1;
        }
        if queenside {
            move_count += 1;
        }
        move_count
    }
    
    
    // For a given piece type and a bitmask of a single source square, return
    // a bitmask of all the squares that piece attacks
    // NOTE: for pawns it only returns ATTACKS
    // TODO: make the colour argument Option<Colour>, as only needed for pawns
    fn get_single_piece_pseudo_attacks(
            &self, 
            piece: Piece, 
            colour: Colour, 
            source_sq_mask: u64, 
            occupied: u64) -> u64 {

        // should only be 1 bit set
        debug_assert!(source_sq_mask.is_power_of_two(), 
                      "piece = {:?}, colour = {:?}, mask = {}, occupied = {}", 
                      piece, colour, source_sq_mask, occupied);

        let idx = source_sq_mask.trailing_zeros() as usize;
        match piece {
            Piece::Pawn => self.pawn_attacks[colour as usize][idx],
            Piece::King => self.king[idx],
            Piece::Knight => self.knight[idx],
            Piece::Bishop => self.bishop_attacks[idx].get_attacks(occupied),
            Piece::Rook => self.rook_attacks[idx].get_attacks(occupied),
            Piece::Queen => self.bishop_attacks[idx].get_attacks(occupied) 
                            | self.rook_attacks[idx].get_attacks(occupied),
        }
    }
    
    fn get_single_piece_captures(
            &self,
            piece: Piece,
            colour: Colour,
            source_sq_mask: u64,
            occupied: u64, 
            enemy_colour_mask: u64) -> u64 {

        
        // should only be 1 bit set
        debug_assert!(source_sq_mask.is_power_of_two(), 
                      "piece = {:?}, colour = {:?}, mask = {}, occupied = {}", 
                      piece, colour, source_sq_mask, occupied);
        
        let idx = source_sq_mask.trailing_zeros() as usize;
        let pseudo_attacks = match piece {
            Piece::Pawn => self.pawn_attacks[colour as usize][idx],
            Piece::King => self.king[idx],
            Piece::Knight => self.knight[idx],
            Piece::Bishop => self.bishop_attacks[idx].get_attacks(occupied),
            Piece::Rook => self.rook_attacks[idx].get_attacks(occupied),
            Piece::Queen => self.bishop_attacks[idx].get_attacks(occupied) 
                            | self.rook_attacks[idx].get_attacks(occupied),
            
        };
        pseudo_attacks & enemy_colour_mask
    }
    
    fn get_single_piece_moves(
            &self,
            piece: Piece,
            colour: Colour,
            source_sq_mask: u64,
            occupied: u64) -> u64 {

        // should only be 1 bit set
        debug_assert!(source_sq_mask.is_power_of_two(), 
                      "piece = {:?}, colour = {:?}, mask = {}, occupied = {}", 
                      piece, colour, source_sq_mask, occupied);
        
        let idx = source_sq_mask.trailing_zeros() as usize;
        let pseudo_moves = match piece {
            Piece::Pawn => self.pawn_moves[colour as usize][idx],
            Piece::King => self.king[idx],
            Piece::Knight => self.knight[idx],
            Piece::Bishop => self.bishop_attacks[idx].get_attacks(occupied),
            Piece::Rook => self.rook_attacks[idx].get_attacks(occupied),
            Piece::Queen => self.bishop_attacks[idx].get_attacks(occupied) 
                            | self.rook_attacks[idx].get_attacks(occupied),
            
        };
        // if not a power of two then it's a double pawn push square, so have to 
        // check if the pawn is blocked, to prevent bunny hopping pawns
        if piece == Piece::Pawn && !pseudo_moves.is_power_of_two() {
            if colour == Colour::White && ((source_sq_mask << 8) & occupied != 0) {
                return 0u64;
            } else if colour == Colour::Black && ((source_sq_mask >> 8) & occupied != 0) {
                return 0u64;
            }
        } 
        pseudo_moves & !occupied
    }

    fn get_king_captures(&self, board: &Board, moves: &mut MoveList, board_status: &BoardStatus) {
        let occupied = board.bitboard.get_entire_mask();
        let enemy_colour_mask = board.bitboard.get_colour_mask(board_status.enemy_colour);
        let king_bitmask = board.bitboard.get_colour_piece_mask(Piece::King, board_status.friendly_colour);
        let king_source_sq: Square = FromPrimitive::from_u32(king_bitmask.trailing_zeros()).unwrap();
        
        let king_captures = self.get_single_piece_captures(
            Piece::King, 
            board_status.friendly_colour, 
            king_bitmask,
            occupied, 
            enemy_colour_mask,
        );
        
        // mask out danger squares
        let king_captures = king_captures & !board_status.danger_squares;

        // in descending order of piece value
        for captured_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
            let mut piece_captures = king_captures & board.bitboard.get_colour_piece_mask(
                captured_piece, board_status.enemy_colour,
            );
            // loop through each capture available for that piece type
            while piece_captures != 0 {
                let dest_sq = piece_captures & piece_captures.wrapping_neg();
                piece_captures ^= dest_sq;
                let dest_sq: Square = FromPrimitive::from_u32(dest_sq.trailing_zeros()).unwrap();
                moves.push(Move {
                    source_sq: king_source_sq,
                    dest_sq,
                    piece: Piece::King,
                    move_type: MoveType::Capture(captured_piece),
                });
            }
        }
    }

    fn get_king_moves(&self, board: &Board, moves: &mut MoveList, board_status: &BoardStatus) {
        let occupied = board.bitboard.get_entire_mask();
        let king_bitmask = board.bitboard.get_colour_piece_mask(Piece::King, board_status.friendly_colour);
        let king_source_sq: Square = FromPrimitive::from_u32(king_bitmask.trailing_zeros()).unwrap();
        
        let king_moves = self.get_single_piece_moves(
            Piece::King,
            board_status.friendly_colour,
            king_bitmask,
            occupied,
        );

        // mask out danger squares
        let mut king_moves = king_moves & !board_status.danger_squares;
        
        while king_moves != 0 {
            let dest_sq = king_moves & king_moves.wrapping_neg();
            king_moves ^= dest_sq;
            let dest_sq: Square = FromPrimitive::from_u32(dest_sq.trailing_zeros()).unwrap();
            moves.push(Move {
                source_sq: king_source_sq,
                dest_sq,
                piece: Piece::King,
                move_type: MoveType::Quiet,
            });
        }
    }

        
        
    // En-passant - Holy Hell
    // En-passant is a tricky one to ensure legality, as it could potentially put yourself
    // in check if the two pawns involved are blocking check
    // Easiest to remove the two pawns from the bitmask and check if it leaves the king in check
    // en-passant is rare enough that its efficiency isn't too important 
    // To check if en-passant is possible, place a pawn of the opposing colour on the ep square
    // and see if its attacks intersect with any friendly pawns
    fn get_en_passant_moves(&self, ep_square: Square, board: &Board, moves: &mut MoveList, board_status: &BoardStatus) {
        let occupied = board.bitboard.get_entire_mask();
        let ep_square_mask = 1 << ep_square as u32;
        // as we only want the intersection with pawns we make the last argument just the pawn mask
        let mut ep_captures = self.get_single_piece_captures(
            Piece::Pawn,
            board_status.enemy_colour,
            ep_square_mask,
            occupied,
            board.bitboard.get_colour_piece_mask(Piece::Pawn, board_status.friendly_colour),
        );
        
        // If there's two pawns that can perform en-passant, then we don't need to check for 
        // legality. 
        // For example, the position 7K/8/8/8/k2pPp1R/8/8/8 b - e3 0 1 whichever of the two black
        // pawns does ep still leaves one blocking check.
        // But 7K/8/8/8/k2pP2R/8/8/8 b - e3 0 1 has only one possible pawn that can do ep, and
        // doing it would leave its own king in check
        let needs_extra_legality_check = ep_captures.is_power_of_two();

        while ep_captures != 0 {
            let ep_capture = ep_captures & ep_captures.wrapping_neg();
            ep_captures ^= ep_capture;

            // pawn is pinned
            if ep_capture & board_status.pinned_pieces != 0 {
                let pseudo_legal_squares = board_status.pinned_pseudo_legal_squares
                    .iter()
                    .find(|&&(first, _)| first == ep_capture)
                    .map(|&(_, second)| second)
                    .unwrap();

                // means the ep_capture is illegal as the capturing pawn is pinned
                if ep_square_mask & pseudo_legal_squares == 0 {
                    continue;
                }
                
                // If the pawn is pinned then we don't have to do the awkward legality check with
                // horizontal discovered check as the pawn can't be pinned to the king horizontally
                // while also being able to do en-passant
                moves.push(Move {
                    source_sq: FromPrimitive::from_u32(ep_capture.trailing_zeros()).unwrap(),
                    dest_sq: ep_square,
                    piece: Piece::Pawn,
                    move_type: MoveType::EnPassant,
                });                        
            } else {
                // now check for illegality if both pawns are pinned
                if needs_extra_legality_check {
                    // get the mask of the two pawns involved and remove from occupied, then see if 
                    // that mask results in check
                    let ep_target_pawn_mask = match ep_square {
                        Square::A3 => 1 << Square::A4 as u32,
                        Square::B3 => 1 << Square::B4 as u32,
                        Square::C3 => 1 << Square::C4 as u32,
                        Square::D3 => 1 << Square::D4 as u32,
                        Square::E3 => 1 << Square::E4 as u32,
                        Square::F3 => 1 << Square::F4 as u32,
                        Square::G3 => 1 << Square::G4 as u32,
                        Square::H3 => 1 << Square::H4 as u32,
                        Square::A6 => 1 << Square::A5 as u32,
                        Square::B6 => 1 << Square::B5 as u32,
                        Square::C6 => 1 << Square::C5 as u32,
                        Square::D6 => 1 << Square::D5 as u32,
                        Square::E6 => 1 << Square::E5 as u32,
                        Square::F6 => 1 << Square::F5 as u32,
                        Square::G6 => 1 << Square::G5 as u32,
                        Square::H6 => 1 << Square::H5 as u32,
                        _ => panic!("invalid en passant square"),
                    };
                    // println!("occupied = {}", bitmask_to_board(occupied));
                    let occupied_tmp = occupied & !(ep_capture | ep_target_pawn_mask);
                    // println!("occupied_tmp = {}", bitmask_to_board(occupied_tmp));
                    
                    let king_mask = board.bitboard.get_colour_piece_mask(
                        Piece::King,
                        board_status.friendly_colour,
                    );
                    // println!("king mask = {}", bitmask_to_board(king_mask));
                    
                    let king_idx = king_mask.trailing_zeros() as usize;

                    // we've already checked if the pawn is pinned, this is purely checking
                    // if removing both pawns reveals check, that can only happen horizontally
                    let rook_horizontal_attacks = self.get_single_piece_pseudo_attacks(
                        Piece::Rook, 
                        board_status.enemy_colour, 
                        king_mask, 
                        occupied_tmp
                    ) & (EAST[king_idx] | WEST[king_idx]);

                    let rook_queen_mask = board.bitboard.get_colour_piece_mask(
                        Piece::Rook,
                        board_status.enemy_colour,
                    ) | board.bitboard.get_colour_piece_mask(
                        Piece::Queen,
                        board_status.enemy_colour,
                    );

                    // results in check, that's illegal
                    if rook_horizontal_attacks & rook_queen_mask != 0 {
                        continue;
                    }
                }

                // if reaching this point, all legality checks for en-passant have passed
                moves.push(Move {
                    source_sq: FromPrimitive::from_u32(ep_capture.trailing_zeros()).unwrap(),
                    dest_sq: ep_square,
                    piece: Piece::Pawn,
                    move_type: MoveType::EnPassant,
                });                        
            }
        }
    }

    fn get_captures(&self, board: &Board, moves: &mut MoveList, board_status: &BoardStatus) {
        // get all captures first

/* 
// Better capture ordering but 2-3 times slower

        let occupied = board.bitboard.get_entire_mask();
        for captured_piece_type in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
            let mut all_pieces = board.bitboard.get_colour_piece_mask(
                captured_piece_type, board_status.enemy_colour
            );
            while all_pieces != 0 {
                let one_captured_piece = all_pieces & all_pieces.wrapping_neg();
                all_pieces ^= one_captured_piece;

                for capturing_piece in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
                    let all_capturing_pieces = board.bitboard.get_colour_piece_mask(
                        capturing_piece, board_status.friendly_colour,
                    );
                    let mut candidate_captures = self.get_single_piece_captures(
                        capturing_piece, board_status.enemy_colour, one_captured_piece,
                        occupied, all_capturing_pieces,
                    );
                    while candidate_captures != 0 {
                        let single_capture = candidate_captures & candidate_captures.wrapping_neg();
                        candidate_captures ^= single_capture;

                        if single_capture & board_status.pinned_pieces != 0 {
                            let pseudo_legal_squares = board_status.pinned_pseudo_legal_squares
                                .iter()
                                .find(|&&(first, _)| first == single_capture)
                                .map(|&(_, second)| second)
                                .unwrap();

                            if one_captured_piece & pseudo_legal_squares == 0 {
                                continue;
                            }
                        }

                        let source_sq = FromPrimitive::from_u32(single_capture.trailing_zeros()).unwrap();
                        let dest_sq = FromPrimitive::from_u32(one_captured_piece.trailing_zeros()).unwrap();
                        
                        if capturing_piece == Piece::Pawn && 
                            (one_captured_piece & 0xFF00000000000000 != 0 || 
                                one_captured_piece & 0xFF != 0) {

                            for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                                moves.push(Move {
                                    source_sq,
                                    dest_sq,
                                    piece: Piece::Pawn,
                                    move_type: MoveType::CapturePromotion(captured_piece_type, promotion_piece),
                                });
                            }
                        } else {
                            moves.push(Move {
                                source_sq,
                                dest_sq,
                                piece: capturing_piece,
                                move_type: MoveType::Capture(captured_piece_type), 
                            });
                        }
                    } 
                }
            }
        }
*/

// Worse capture ordering, but 2-3 times faster
        let occupied = board.bitboard.get_entire_mask();
        let start_idx = moves.get_idx();
        for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
            let all_pieces = board.bitboard.get_colour_piece_mask(piece_type, board_status.friendly_colour);

            let mut unpinned = all_pieces & !board_status.pinned_pieces;
            while unpinned != 0 {
                let one_piece = unpinned & unpinned.wrapping_neg();
                unpinned ^= one_piece;
                
                let captures = self.get_single_piece_captures(
                    piece_type,
                    board_status.friendly_colour,
                    one_piece,
                    occupied,
                    board.bitboard.get_colour_mask(board_status.enemy_colour),
                );
                
                if captures == 0 {
                    continue;
                }
                // in reverse order of piece value
                for captured_piece_type in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
                    let mut all_captures_of_piece = captures & board.bitboard.get_colour_piece_mask(
                        captured_piece_type, board_status.enemy_colour,
                    );

                    while all_captures_of_piece != 0 {
                        let one_capture = all_captures_of_piece & all_captures_of_piece.wrapping_neg();
                        all_captures_of_piece ^= one_capture;

                        let source_sq = FromPrimitive::from_u32(one_piece.trailing_zeros()).unwrap();
                        let dest_sq = FromPrimitive::from_u32(one_capture.trailing_zeros()).unwrap();
                        
                        if piece_type == Piece::Pawn && (one_capture & 0xFF00000000000000 != 0 || one_capture & 0xFF != 0) {

                            for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                                moves.push(Move {
                                    source_sq,
                                    dest_sq,
                                    piece: Piece::Pawn,
                                    move_type: MoveType::CapturePromotion(captured_piece_type, promotion_piece),
                                });
                            }
                        } else {
                            moves.push(Move {
                                source_sq,
                                dest_sq,
                                piece: piece_type,
                                move_type: MoveType::Capture(captured_piece_type), 
                            });
                        }
                    }
                }
            } 
            
            let mut pinned_pieces = all_pieces & board_status.pinned_pieces;
            while pinned_pieces != 0 {
                let pinned_piece = pinned_pieces & pinned_pieces.wrapping_neg();
                pinned_pieces ^= pinned_piece;

                let pseudo_legal_squares = board_status.pinned_pseudo_legal_squares
                    .iter()
                    .find(|&&(first, _)| first == pinned_piece)
                    .map(|&(_, second)| second)
                    .unwrap();

                let captures = self.get_single_piece_captures(
                    piece_type,
                    board_status.friendly_colour,
                    pinned_piece,
                    occupied,
                    board.bitboard.get_colour_mask(board_status.enemy_colour),
                );
                
                let legal_captures = captures & pseudo_legal_squares;
                
                // in reverse order of piece value
                for captured_piece_type in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
                    let mut all_captures_of_piece = legal_captures & board.bitboard.get_colour_piece_mask(
                        captured_piece_type, board_status.enemy_colour,
                    );

                    while all_captures_of_piece != 0 {
                        let one_capture = all_captures_of_piece & all_captures_of_piece.wrapping_neg();
                        all_captures_of_piece ^= one_capture;

                        if piece_type == Piece::Pawn && (one_capture & 0xFF00000000000000 != 0 || one_capture & 0xFF != 0) {
                            for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                                moves.push(Move {
                                    source_sq: FromPrimitive::from_u32(pinned_piece.trailing_zeros()).unwrap(),
                                    dest_sq: FromPrimitive::from_u32(one_capture.trailing_zeros()).unwrap(),
                                    piece: Piece::Pawn,
                                    move_type: MoveType::CapturePromotion(captured_piece_type, promotion_piece),
                                });
                            }                            
                        } else {
                            moves.push(Move {
                                source_sq: FromPrimitive::from_u32(pinned_piece.trailing_zeros()).unwrap(),
                                dest_sq: FromPrimitive::from_u32(one_capture.trailing_zeros()).unwrap(),
                                piece: piece_type,
                                move_type: MoveType::Capture(captured_piece_type), 
                            });
                        }
                    }
                }
            }
        }

        // sort captures by value gaind
        let captures = moves.get_slice(start_idx);
        // println!("captures before = {:?}", captures);
        let val_fn = |chess_move: &Move| {
            let captured_piece = match chess_move.move_type {
                MoveType::Capture(p) => p, 
                MoveType::CapturePromotion(p, _) => p,
                _ => panic!("Invalid move type"),
            };
            let piece_values = [100, 20000, 900, 330, 320, 500];
            piece_values[captured_piece as usize] - piece_values[chess_move.piece as usize]
        };
        captures.sort_by(|a, b| {
            let a_val = val_fn(a);
            let b_val = val_fn(b);
            b_val.partial_cmp(&a_val).unwrap_or(std::cmp::Ordering::Equal)
        });
        // println!("captures after = {:?}", captures);
    }

    fn get_quiet_moves(&self, board: &Board, moves: &mut MoveList, board_status: &BoardStatus) {
        let occupied = board.bitboard.get_entire_mask();
        for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
            let mut all_pieces = board.bitboard.get_colour_piece_mask(
                piece_type,
                board_status.friendly_colour,
            );

            while all_pieces != 0 {
                let one_piece = all_pieces & all_pieces.wrapping_neg();
                all_pieces ^= one_piece;
                
                let source_sq: Square = FromPrimitive::from_u32(one_piece.trailing_zeros()).unwrap();

                let quiet_moves = self.get_single_piece_moves(
                    piece_type,
                    board_status.friendly_colour,
                    one_piece,
                    occupied,
                );

                let mut quiet_moves = if one_piece & board_status.pinned_pieces != 0 {
                    let legal_squares = board_status.pinned_pseudo_legal_squares
                        .iter()
                        .find(|&&(first, _)| first == one_piece)
                        .map(|&(_, second)| second)
                        .unwrap();
                    quiet_moves & legal_squares
                } else {
                    quiet_moves
                };
                
                while quiet_moves != 0 {
                    let quiet_move = quiet_moves & quiet_moves.wrapping_neg();
                    quiet_moves ^= quiet_move;

                    let dest_sq: Square = FromPrimitive::from_u32(quiet_move.trailing_zeros()).unwrap();
                    
                    if piece_type == Piece::Pawn && (quiet_move & 0xFF != 0 || quiet_move & 0xFF00000000000000 != 0) {
                        for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                            moves.push(Move {
                                source_sq,
                                dest_sq,
                                piece: piece_type,
                                move_type: MoveType::MovePromotion(promotion_piece),
                            });
                        }
                    } else if piece_type == Piece::Pawn && 
                        (
                            (one_piece & 0xFF00 != 0 && quiet_move & 0xFF000000 != 0) ||
                            (one_piece & 0xFF000000000000 != 0 && quiet_move & 0xFF00000000 != 0)
                        ) {
                        moves.push(Move {
                            source_sq,
                            dest_sq,
                            piece: piece_type,
                            move_type: MoveType::DoublePawnPush,
                        });
                    } else {
                        moves.push(Move {
                            source_sq,
                            dest_sq,
                            piece: piece_type,
                            move_type: MoveType::Quiet,
                        });
                    }
                }
            }
        }
    }

    /*
    * Ways to get out of a single check:
    * 1. Capture the checking piece with any piece (can include capture-promotions and en-passant)
    * 2. Block the check with any piece apart from the king (can include promotions)
    * 3. Move the king to a non danger square (either quiet or capture)
    */
    fn get_out_of_single_check(&self, board: &Board, moves: &mut MoveList, board_status: &BoardStatus) {
        let occupied = board.bitboard.get_entire_mask();
        let enemy_colour_mask = board.bitboard.get_colour_mask(board_status.enemy_colour);
        let (checking_piece, checking_piece_mask, block_squares) = board_status.king_attacking_pieces[0];
        
        let checking_piece_sq: Square = FromPrimitive::from_u32(checking_piece_mask.trailing_zeros()).unwrap();
        
        // Captures of the checking piece first. Non king pieces can only capture the checking 
        // piece to stop check
        for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
            let mut all_pieces = board.bitboard.get_colour_piece_mask(piece_type, board_status.friendly_colour);
            
            // a pinned piece can't do anything about stopping check, remove them
            all_pieces &= !board_status.pinned_pieces;

            while all_pieces != 0 {
                let one_piece = all_pieces & all_pieces.wrapping_neg();
                all_pieces ^= one_piece;

                let captures = self.get_single_piece_captures(
                    piece_type,
                    board_status.friendly_colour,
                    one_piece, 
                    occupied,
                    enemy_colour_mask,
                );
                
                let captures_of_checking_piece = captures & checking_piece_mask;
                if captures_of_checking_piece == 0 {
                    continue;
                }
                
                // for each piece, should only be one way to capture the checking piece
                debug_assert!(captures_of_checking_piece.is_power_of_two());

                let source_sq: Square = FromPrimitive::from_u32(one_piece.trailing_zeros()).unwrap();
                
                // Means we can capture the checking piece via pawn capture-promotion. Hooray!
                if piece_type == Piece::Pawn && 
                    (checking_piece_mask & 0xFF00000000000000 != 0 || checking_piece_mask & 0xFF != 0) {

                    for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                        moves.push(Move {
                            source_sq,
                            dest_sq: checking_piece_sq,
                            piece: piece_type,
                            move_type: MoveType::CapturePromotion(checking_piece, promotion_piece), 
                        });
                    }
                } else {
                    moves.push(Move {
                        source_sq,
                        dest_sq: checking_piece_sq,
                        piece: piece_type,
                        move_type: MoveType::Capture(checking_piece)
                    });
                }
            }
        }

        // en-passant captures. If EP of the checking pawn is possible, we don't need to check 
        // for the awkward revealed check when both pawns move from ep, as the king is already
        // in check from the double pawn pushing pawn
        if let Some(ep_square) = board.en_passant {
            let double_push_pawn = match ep_square {
                Square::A3 => Square::A4,
                Square::B3 => Square::B4,
                Square::C3 => Square::C4,
                Square::D3 => Square::D4,
                Square::E3 => Square::E4,
                Square::F3 => Square::F4,
                Square::G3 => Square::G4,
                Square::H3 => Square::H4,
                Square::A6 => Square::A5,
                Square::B6 => Square::B5,
                Square::C6 => Square::C5,
                Square::D6 => Square::D5,
                Square::E6 => Square::E5,
                Square::F6 => Square::F5,
                Square::G6 => Square::G5,
                Square::H6 => Square::H5,
                _ => panic!("Invalid en passant square"),
            };
            let ep_square_mask = 1 << (ep_square as u32);
            let double_push_pawn_mask = 1 << (double_push_pawn as u32);
            // means the double pushing pawn is the one checking the king, check if we can ep it
            if double_push_pawn_mask & checking_piece_mask != 0 {
                // as we only want the intersection with pawns we make the last argument just the pawn mask
                let mut ep_captures = self.get_single_piece_captures(
                    Piece::Pawn,
                    board_status.enemy_colour,
                    ep_square_mask,
                    occupied,
                    board.bitboard.get_colour_piece_mask(Piece::Pawn, board_status.friendly_colour),
                );

                while ep_captures != 0 {
                    let ep_capture = ep_captures & ep_captures.wrapping_neg();
                    ep_captures ^= ep_capture;

                    if ep_capture & board_status.pinned_pieces == 0 {
                        moves.push(Move {
                            source_sq: FromPrimitive::from_u32(ep_capture.trailing_zeros()).unwrap(),
                            dest_sq: ep_square,
                            piece: Piece::Pawn,
                            move_type: MoveType::EnPassant,
                        });
                    }
                }
            }
        }
        
        // King captures. The king can get out of check by capturing any available piece
        self.get_king_captures(board, moves, &board_status);
        self.get_king_moves(board, moves, &board_status);

        // other piece moves to block check
        // Only sliding pieces can be blocked
        if [Piece::Queen, Piece::Rook, Piece::Bishop].contains(&checking_piece) {
            for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen]  {
                let mut all_pieces = board.bitboard.get_colour_piece_mask(piece_type, board_status.friendly_colour);
                while all_pieces != 0 {
                    let one_piece = all_pieces & all_pieces.wrapping_neg();
                    all_pieces ^= one_piece;
                    let mut piece_moves = self.get_single_piece_moves(
                        piece_type,
                        board_status.friendly_colour,
                        one_piece,
                        occupied,
                    ) & block_squares;

                    while piece_moves != 0 {
                        let piece_move = piece_moves & piece_moves.wrapping_neg();
                        piece_moves ^= piece_move;

                        let source_sq: Square = FromPrimitive::from_u32(one_piece.trailing_zeros()).unwrap();
                        let dest_sq: Square = FromPrimitive::from_u32(piece_move.trailing_zeros()).unwrap();
                        
                        // Can block with a move promotion. Hooray!
                        if piece_type == Piece::Pawn &&
                            (piece_move & 0xFF00000000000000 != 0 || piece_move & 0xFF != 0) {

                            for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                                moves.push(Move {
                                    source_sq,
                                    dest_sq,
                                    piece: piece_type,
                                    move_type: MoveType::MovePromotion(promotion_piece),
                                });
                            }
                        } else if piece_type == Piece::Pawn &&
                            (
                                (one_piece & 0xFF00 != 0 && piece_move & 0xFF000000 != 0) ||
                                (one_piece & 0xFF000000000000 != 0 && piece_move & 0xFF00000000 != 0)
                            ) {
                            moves.push(Move {
                                source_sq,
                                dest_sq,
                                piece: piece_type,
                                move_type: MoveType::DoublePawnPush,
                            });
                        } else {
                            moves.push(Move {
                                source_sq,
                                dest_sq,
                                piece: piece_type,
                                move_type: MoveType::Quiet,
                            });
                        }
                    }
                }
            }
        }

    }
    
    fn get_legal_castling_rights(&self, board: &Board, moves: &mut MoveList, board_status: &BoardStatus) {
        let (mut kingside, mut queenside) = board.castling_rights.can_castle(board_status.friendly_colour);
        
        // For castling we have to check that:
        // 1. The intermediate squares between the king and rook are empty
        // 2. The intermediate squares the king travels are not under attack
        // For kingside castling, those squares are the same, but for queenside
        // castling they are not. i.e. for white queenside castling b1 c1 and d1
        // need to be vacated, but only c1 and d1 need to be unattacked, you can
        // still queenside castle if b1 is under attack as the king does not traverse
        // it
        if kingside {
            let king_travel_squares: u64 = match board_status.friendly_colour {
                Colour::White => 0x60,
                Colour::Black => 0x6000000000000000,
            };
            kingside = king_travel_squares & board.bitboard.get_entire_mask() == 0 
                    && king_travel_squares & board_status.danger_squares == 0;
        }
        
        if queenside {
            let (in_between_squares, king_travel_squares): (u64, u64) = match board_status.friendly_colour {
                Colour::White => (0xE, 0xC),
                Colour::Black => (0xE00000000000000, 0xC00000000000000),
            };
            queenside = in_between_squares & board.bitboard.get_entire_mask() == 0
                     && king_travel_squares & board_status.danger_squares == 0;
        }

        if kingside {
            let (source_sq, dest_sq) = match board_status.friendly_colour {
                Colour::White => (Square::E1, Square::G1),
                Colour::Black => (Square::E8, Square::G8),
            };
            
            moves.push(Move {
                source_sq,
                dest_sq,
                piece: Piece::King,
                move_type: MoveType::CastleKingSide,
            });
        }

        if queenside {
            let (source_sq, dest_sq) = match board_status.friendly_colour {
                Colour::White => (Square::E1, Square::C1),
                Colour::Black => (Square::E8, Square::C8),
            };
            
            moves.push(Move {
                source_sq,
                dest_sq,
                piece: Piece::King,
                move_type: MoveType::CastleQueenSide,
            });
        }
    }

    pub fn king_in_check(&self, board: &Board) -> bool {
        let king_square = board.bitboard.get_colour_piece_mask(
            Piece::King, board.turn_colour
        );
        let occupied = board.bitboard.get_entire_mask();
        for piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
            let piece_mask = board.bitboard.get_colour_piece_mask(
                piece, !&board.turn_colour,
            );
            let attacks_from_king_sq = self.get_single_piece_pseudo_attacks(
                piece, board.turn_colour, king_square, occupied
            );

            if piece_mask & attacks_from_king_sq != 0 {
                return true;
            }
        }
        false
    }

    /*
     * To find pinned pieces:
     * for each sliding piece, loop through each ray direction individually
     * get the full attack mask from the magic bitboard
     * get that direction mask for that square from the masks below
     * AND with the ray direction mask
     * get the full attack mask for the same piece but from king square via the magic bitboard
     * get the opposite ray direction mask for that sliding piece from the king square
     * e.g. NW -> SE, N -> S etc.
     * AND with the ray direction mask
     * intersect the two results, any pinned piece will result
     * 
     * Example:
     * Position: r1bqkbnr/ppp2ppp/2np4/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1
     * The knight on C6 is pinned
     *
     * 1. Get the full attack mask for the bishop on B5:
     *
     *    8| 0 0 0 0 0 0 0 0 
     *    7| 0 0 0 0 0 0 0 0 
     *    6| 1 0 1 0 0 0 0 0 
     *    5| 0 0 0 0 0 0 0 0 
     *    4| 1 0 1 0 0 0 0 0 
     *    3| 0 0 0 1 0 0 0 0 
     *    2| 0 0 0 0 1 0 0 0 
     *    1| 0 0 0 0 0 1 0 0 
     *      ---------------
     *       A B C D E F G H
     *
     * 2. Get the NE ray direction from the B5 square:
     *
     *    8| 0 0 0 0 1 0 0 0 
     *    7| 0 0 0 1 0 0 0 0 
     *    6| 0 0 1 0 0 0 0 0 
     *    5| 0 0 0 0 0 0 0 0 
     *    4| 0 0 0 0 0 0 0 0 
     *    3| 0 0 0 0 0 0 0 0 
     *    2| 0 0 0 0 0 0 0 0 
     *    1| 0 0 0 0 0 0 0 0 
     *      ---------------
     *       A B C D E F G H
     *
     * 3. AND the two together:
     *    8| 0 0 0 0 0 0 0 0 
     *    7| 0 0 0 0 0 0 0 0 
     *    6| 0 0 1 0 0 0 0 0 
     *    5| 0 0 0 0 0 0 0 0 
     *    4| 0 0 0 0 0 0 0 0 
     *    3| 0 0 0 0 0 0 0 0 
     *    2| 0 0 0 0 0 0 0 0 
     *    1| 0 0 0 0 0 0 0 0 
     *      ---------------
     *       A B C D E F G H
     *
     * 4. Get the full attack mask for a bishop on the king square on E8:
     *
     *    8| 0 0 0 0 0 0 0 0 
     *    7| 0 0 0 1 0 1 0 0 
     *    6| 0 0 1 0 0 0 0 0 
     *    5| 0 0 0 0 0 0 0 0 
     *    4| 0 0 0 0 0 0 0 0 
     *    3| 0 0 0 0 0 0 0 0 
     *    2| 0 0 0 0 0 0 0 0 
     *    1| 0 0 0 0 0 0 0 0 
     *      ---------------
     *       A B C D E F G H
     *
     * 5. Get the SW ray direction (opposite of NE) from the E8 square:
     *
     *    8| 0 0 0 0 0 0 0 0 
     *    7| 0 0 0 1 0 0 0 0 
     *    6| 0 0 1 0 0 0 0 0 
     *    5| 0 1 0 0 0 0 0 0 
     *    4| 1 0 0 0 0 0 0 0 
     *    3| 0 0 0 0 0 0 0 0 
     *    2| 0 0 0 0 0 0 0 0 
     *    1| 0 0 0 0 0 0 0 0 
     *      ---------------
     *       A B C D E F G H
     *
     * 6. AND the two together:
     *    8| 0 0 0 0 0 0 0 0 
     *    7| 0 0 0 1 0 0 0 0 
     *    6| 0 0 1 0 0 0 0 0 
     *    5| 0 0 0 0 0 0 0 0 
     *    4| 0 0 0 0 0 0 0 0 
     *    3| 0 0 0 0 0 0 0 0 
     *    2| 0 0 0 0 0 0 0 0 
     *    1| 0 0 0 0 0 0 0 0 
     *      ---------------
     *       A B C D E F G H
     *
     * 7. Intersect the two resulting masks from steps 3 and 6:
     *    8| 0 0 0 0 0 0 0 0 
     *    7| 0 0 0 0 0 0 0 0 
     *    6| 0 0 1 0 0 0 0 0 
     *    5| 0 0 0 0 0 0 0 0 
     *    4| 0 0 0 0 0 0 0 0 
     *    3| 0 0 0 0 0 0 0 0 
     *    2| 0 0 0 0 0 0 0 0 
     *    1| 0 0 0 0 0 0 0 0 
     *      ---------------
     *       A B C D E F G H
     *
     * The resulting mask shows the piece on square C6 is pinned
     *
     * We also need to generate all the danger squares, so we can combine the calculation
     * of pinned pieces with this for efficency, as we calculate the attack masks
     * for the sliding pieces, we OR it with the danger squares bitboard. Then we
     * add the attack squares for the king, knight and pawn to the danger bitboard
     */

    pub fn get_board_status(&self, bitboard: &BitBoard, friendly_colour: Colour) -> BoardStatus {
        // first get the king bitboard for the colour to move
        let friendly_king_square = bitboard.get_colour_piece_mask(Piece::King, friendly_colour);
        let friendly_king_idx = friendly_king_square.trailing_zeros() as usize;
        
        
        // we need the occupied squares to calculate sliding piece attacks
        let occupied = bitboard.get_entire_mask();

        // but we want to remove the king because:
        // 1. for danger squares, we have to include behind the king
        // 2. it's not needed for calculating pinned pieces
        let kingless_mask = occupied ^ friendly_king_square;
        
        // as we're calculating pinned pieces for colour_to_move, we need to
        // iterate through the sliding pieces of the opposite colour
        let enemy_colour = !&friendly_colour;

        // will mark danger squares while calculating pinned pieces 
        let mut danger_squares: u64 = 0;
        let mut pinned_pieces: u64 = 0;
        // There can be maximum of 8 pieces pinned to the board at any one time
        // i.e. one for each direction the king can move
        let mut pinned_legal_squares: [(u64, u64); 8] = [(0, 0); 8];
        let mut num_pinned: usize = 0;

        // A king can only be in check from at most 2 pieces
        let mut king_attacking_pieces: [(Piece, u64, u64); 2] = [(Piece::Pawn, 0, 0); 2];
        let mut num_checking_pieces: u8 = 0;
        // let mut discovered_check_pseudo_legal_squares: [(Piece, u64, u64); 8] = [(Piece::Pawn, 0, 0); 8];
        // let mut num_discovered_checks: u8 = 0;
        // let mut check_squares: [u64; 6] = [0, 0, 0, 0, 0, 0];

        let king_blockers = self.get_single_piece_captures(
            Piece::Queen,
            enemy_colour,
            friendly_king_square,
            occupied,
            bitboard.get_colour_mask(friendly_colour),
        );

        let unobstructed_rook_mask = ROOK[friendly_king_idx];
        let unobstructed_bishop_mask = BISHOP[friendly_king_idx];
        let unobstructed_queen_mask = unobstructed_rook_mask | unobstructed_bishop_mask;
        
        for piece in [Piece::Bishop, Piece::Rook, Piece::Queen] {
            // for that piece, also add check squares friendly pieces can move to
            // to put the enemy king in check
            // we put the colour as enemy colour as colour only affects the 
            // lookup of pawn moves
            // check_squares[piece as usize] = self.get_single_piece_pseudo_attacks(
            //     piece, 
            //     enemy_colour, 
            //     enemy_king_square, 
            //     occupied
            // );
                
            // Start of checks for friendly pieces that are pinned to the friendly king

            // get bitmasks of all pieces of that type
            let mut enemy_piece_mask = bitboard.get_colour_piece_mask(piece, enemy_colour);
            
            while enemy_piece_mask != 0 {
                // single out the least significant bit
                let source_square = enemy_piece_mask & enemy_piece_mask.wrapping_neg();
                enemy_piece_mask ^= source_square;
                
                // get the full attacks for that piece on the source square
                let enemy_piece_att_squares = self.get_single_piece_pseudo_attacks(
                    piece, 
                    enemy_colour, 
                    source_square, 
                    kingless_mask
                );
                
                // add to danger squares
                danger_squares |= enemy_piece_att_squares;

                // king in check
                if enemy_piece_att_squares & friendly_king_square != 0 {
                    king_attacking_pieces[num_checking_pieces as usize] = 
                        (piece, 
                        source_square, 
                        BLOCKER_TABLE[source_square.trailing_zeros() as usize][friendly_king_idx]);
                    num_checking_pieces += 1;
                    continue;
                }

                let unobstructed_mask = match piece {
                    Piece::Rook => unobstructed_rook_mask,
                    Piece::Bishop => unobstructed_bishop_mask,
                    Piece::Queen => unobstructed_queen_mask,
                    _ => panic!("This should not happen"),
                };

                // means the piece and the king are not on the same rays, don't need to check for 
                // pins
                if unobstructed_mask & source_square == 0 {
                    continue;
                }
                

                // Only candidate pinned pieces at this point, as a queen could be attacking a 
                // king blocker not on the same ray as the king. e.g. the pawn on B3 in the position
                // 8/8/1q6/8/3Q4/1P2K3/8/8 w - - 0 1, the pawn is a blocker for the king to the west
                // and is also under attack by the queen, but it is not pinned
                let pin_candidates = king_blockers & enemy_piece_att_squares;

                // despite being on the same ray, there's other pieces in the way
                if pin_candidates == 0 {
                    continue;
                }

                let piece_idx = source_square.trailing_zeros() as usize;

                // TODO: find a way to stop this being a vector
                let ray_tables = match piece {
                    Piece::Bishop if piece_idx > friendly_king_idx => vec![
                        (&SOUTH_EAST, &NORTH_WEST),
                        (&SOUTH_WEST, &NORTH_EAST),
                    ],
                    Piece::Bishop => vec![
                        (&NORTH_EAST, &SOUTH_WEST),
                        (&NORTH_WEST, &SOUTH_EAST),
                    ],
                    Piece::Rook if piece_idx > friendly_king_idx => vec![
                        (&SOUTH, &NORTH),
                        (&WEST, &EAST),
                    ],
                    Piece::Rook => vec![
                        (&NORTH, &SOUTH),
                        (&EAST, &WEST),
                    ],
                    Piece::Queen if piece_idx > friendly_king_idx => vec![
                        (&SOUTH, &NORTH),
                        (&WEST, &EAST),
                        (&SOUTH_EAST, &NORTH_WEST),
                        (&SOUTH_WEST, &NORTH_EAST),
                    ],
                    Piece::Queen => vec![
                        (&NORTH, &SOUTH),
                        (&EAST, &WEST),
                        (&NORTH_EAST, &SOUTH_WEST),
                        (&NORTH_WEST, &SOUTH_EAST),
                    ],
                    _ => panic!("This should not happen"),
                };
                
                for ray_direction in ray_tables.iter() {
                    let pinned = ray_direction.0[piece_idx] &
                                 ray_direction.1[friendly_king_idx] & 
                                 pin_candidates;
                    pinned_pieces |= pinned;
                    if pinned != 0 {
                        debug_assert!(pinned.is_power_of_two());
                        pinned_legal_squares[num_pinned] = (
                            pinned,
                            ray_direction.0[piece_idx] & ray_direction.1[friendly_king_idx] | source_square,
                        );
                        num_pinned += 1;
                        break;
                    }
                }
            }
/* -------------------------------------------------------------------------------------------------
 * This section calculates potential discovered checks for the friendly colour
 * However, it's made things rather complicated in generating move order, for now I'm more concerned
 * with correctness of move gen rather than optimising move order, will keep this commented out 
 * for now and revisit once I'm confident move generation is correct.
 * 
            // start of checks for friendly pieces pinned to the enemy king 
            // i.e. those that can cause a discovered check
            
            // get bitmasks of all pieces of that type
            let mut friendly_piece_mask = bitboard.get_colour_piece_mask(piece, friendly_colour);
            
            // get the full attacks for the piece if it was on the enemy king square
            let king_att_squares = self.get_single_piece_pseudo_attacks(
                piece, 
                friendly_colour, 
                enemy_king_square, 
                occupied 
            );
            
            // iterate through those pieces one by one
            while friendly_piece_mask != 0 {
                let source_square = friendly_piece_mask & friendly_piece_mask.wrapping_neg();
                
                // get the full attacks for that piece on the source square
                let friendly_piece_att_squares = self.get_single_piece_pseudo_attacks(
                    piece, 
                    friendly_colour, 
                    source_square, 
                    occupied 
                );
                
                let piece_idx = source_square.trailing_zeros() as usize;
                
                // loop through each ray direction separately
                for ray_direction in ray_tables.iter() {
                    // get the ray mask for that direction
                    let ray_mask = ray_direction.0[piece_idx];
                    let piece_ray_mask = ray_mask & friendly_piece_att_squares;
                    
                    // now get the mask for the opposite direction with the piece if placed on 
                    // the king square
                    let ray_mask = ray_direction.1[enemy_king_idx];
                    let king_ray_mask = ray_mask & king_att_squares;
                    
                    let pinned = piece_ray_mask & king_ray_mask & bitboard.get_colour_mask(friendly_colour);
                    
                    // means we have found a pinned piece for that piece, can skip the remaining
                    // ray directions as a piece can't pin a piece in multiple ray directions
                    // Technically this could incorrectly break early due to the comment above,
                    // but as that can only happen when the piece can have the king in check it 
                    // can't pin a piece in one of the other ray directions anyway
                    if pinned != 0 {
                        // check that there's only one pinned piece
                        debug_assert!(pinned.is_power_of_two());

                        discovered_check_pseudo_legal_squares[num_discovered_checks as usize] = (
                            bitboard.get_piece_type_from_mask(pinned),
                            pinned,
                            // the piece can move to any square that's not in the ray blocking the
                            // enemy king to initiate discovered check, i.e. the rest of the board
                            !(piece_ray_mask | king_ray_mask | source_square | enemy_king_square)
                        );
                        num_discovered_checks += 1;
                        break;
                    }
                }
                // remove least significant bit from piece mask for next iteration
                friendly_piece_mask ^= source_square;
            }
*-------------------------------------------------------------------------------------------------*/
        }
        
        // We now have the completed mask for pinned pieces, and have partially done the danger
        // squares with the pinned pieces. Now need to complete the danger squares with king, pawn,
        // and knight attacks
        for piece in [Piece::Pawn, Piece::King, Piece::Knight] {
            let mut source_squares = bitboard.get_colour_piece_mask(piece, enemy_colour);
            
            // for that piece, also add check squares friendly pieces can move to
            // to put the enemy king in check
            // check_squares[piece as usize] = self.get_single_piece_pseudo_attacks(
            //     piece, 
            //     enemy_colour, 
            //     enemy_king_square, 
            //     occupied
            // );

            while source_squares != 0 {
                let source_square = source_squares & source_squares.wrapping_neg();
                source_squares ^= source_square;

                let attacking_squares = self.get_single_piece_pseudo_attacks(
                    piece, 
                    enemy_colour, 
                    source_square, 
                    occupied
                );
                
                danger_squares |= attacking_squares;
                
                // if piece is putting the king in check
                if attacking_squares & friendly_king_square == friendly_king_square {
                    debug_assert!(piece != Piece::King);
                    // As a non sliding piece can't be blocked it can only be captured
                    king_attacking_pieces[num_checking_pieces as usize] = (piece, source_square, source_square);
                    num_checking_pieces += 1;
                }
            }
        }

        BoardStatus {
            friendly_colour,
            enemy_colour,
            danger_squares,
            pinned_pieces,
            pinned_pseudo_legal_squares: pinned_legal_squares,
            king_attacking_pieces,
            num_checking_pieces,
            // discovered_check_pseudo_legal_squares,
            // num_discovered_checks,
            // check_squares,
        }
    }
}

#[rustfmt::skip]
const ROOK: [u64; 64] = [
	0x01010101010101fe, 0x02020202020202fd, 0x04040404040404fb, 0x08080808080808f7, 
	0x10101010101010ef, 0x20202020202020df, 0x40404040404040bf, 0x808080808080807f, 
	0x010101010101fe01, 0x020202020202fd02, 0x040404040404fb04, 0x080808080808f708, 
	0x101010101010ef10, 0x202020202020df20, 0x404040404040bf40, 0x8080808080807f80, 
	0x0101010101fe0101, 0x0202020202fd0202, 0x0404040404fb0404, 0x0808080808f70808, 
	0x1010101010ef1010, 0x2020202020df2020, 0x4040404040bf4040, 0x80808080807f8080, 
	0x01010101fe010101, 0x02020202fd020202, 0x04040404fb040404, 0x08080808f7080808, 
	0x10101010ef101010, 0x20202020df202020, 0x40404040bf404040, 0x808080807f808080, 
	0x010101fe01010101, 0x020202fd02020202, 0x040404fb04040404, 0x080808f708080808, 
	0x101010ef10101010, 0x202020df20202020, 0x404040bf40404040, 0x8080807f80808080, 
	0x0101fe0101010101, 0x0202fd0202020202, 0x0404fb0404040404, 0x0808f70808080808, 
	0x1010ef1010101010, 0x2020df2020202020, 0x4040bf4040404040, 0x80807f8080808080, 
	0x01fe010101010101, 0x02fd020202020202, 0x04fb040404040404, 0x08f7080808080808, 
	0x10ef101010101010, 0x20df202020202020, 0x40bf404040404040, 0x807f808080808080, 
	0xfe01010101010101, 0xfd02020202020202, 0xfb04040404040404, 0xf708080808080808, 
	0xef10101010101010, 0xdf20202020202020, 0xbf40404040404040, 0x7f80808080808080, 
	];

#[rustfmt::skip]
const BISHOP: [u64; 64] = [
	0x8040201008040200, 0x0080402010080500, 0x0000804020110a00, 0x0000008041221400, 
	0x0000000182442800, 0x0000010204885000, 0x000102040810a000, 0x0102040810204000, 
	0x4020100804020002, 0x8040201008050005, 0x00804020110a000a, 0x0000804122140014, 
	0x0000018244280028, 0x0001020488500050, 0x0102040810a000a0, 0x0204081020400040, 
	0x2010080402000204, 0x4020100805000508, 0x804020110a000a11, 0x0080412214001422, 
	0x0001824428002844, 0x0102048850005088, 0x02040810a000a010, 0x0408102040004020, 
	0x1008040200020408, 0x2010080500050810, 0x4020110a000a1120, 0x8041221400142241, 
	0x0182442800284482, 0x0204885000508804, 0x040810a000a01008, 0x0810204000402010, 
	0x0804020002040810, 0x1008050005081020, 0x20110a000a112040, 0x4122140014224180, 
	0x8244280028448201, 0x0488500050880402, 0x0810a000a0100804, 0x1020400040201008, 
	0x0402000204081020, 0x0805000508102040, 0x110a000a11204080, 0x2214001422418000, 
	0x4428002844820100, 0x8850005088040201, 0x10a000a010080402, 0x2040004020100804, 
	0x0200020408102040, 0x0500050810204080, 0x0a000a1120408000, 0x1400142241800000, 
	0x2800284482010000, 0x5000508804020100, 0xa000a01008040201, 0x4000402010080402, 
	0x0002040810204080, 0x0005081020408000, 0x000a112040800000, 0x0014224180000000, 
	0x0028448201000000, 0x0050880402010000, 0x00a0100804020100, 0x0040201008040201, 
	];


#[rustfmt::skip]
const NORTH: [u64; 64] = [
	0x0101010101010100, 0x0202020202020200, 0x0404040404040400, 0x0808080808080800, 
	0x1010101010101000, 0x2020202020202000, 0x4040404040404000, 0x8080808080808000, 
	0x0101010101010000, 0x0202020202020000, 0x0404040404040000, 0x0808080808080000, 
	0x1010101010100000, 0x2020202020200000, 0x4040404040400000, 0x8080808080800000, 
	0x0101010101000000, 0x0202020202000000, 0x0404040404000000, 0x0808080808000000, 
	0x1010101010000000, 0x2020202020000000, 0x4040404040000000, 0x8080808080000000, 
	0x0101010100000000, 0x0202020200000000, 0x0404040400000000, 0x0808080800000000, 
	0x1010101000000000, 0x2020202000000000, 0x4040404000000000, 0x8080808000000000, 
	0x0101010000000000, 0x0202020000000000, 0x0404040000000000, 0x0808080000000000, 
	0x1010100000000000, 0x2020200000000000, 0x4040400000000000, 0x8080800000000000, 
	0x0101000000000000, 0x0202000000000000, 0x0404000000000000, 0x0808000000000000, 
	0x1010000000000000, 0x2020000000000000, 0x4040000000000000, 0x8080000000000000, 
	0x0100000000000000, 0x0200000000000000, 0x0400000000000000, 0x0800000000000000, 
	0x1000000000000000, 0x2000000000000000, 0x4000000000000000, 0x8000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
];

#[rustfmt::skip]
const SOUTH: [u64; 64] = [
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000001, 0x0000000000000002, 0x0000000000000004, 0x0000000000000008, 
	0x0000000000000010, 0x0000000000000020, 0x0000000000000040, 0x0000000000000080, 
	0x0000000000000101, 0x0000000000000202, 0x0000000000000404, 0x0000000000000808, 
	0x0000000000001010, 0x0000000000002020, 0x0000000000004040, 0x0000000000008080, 
	0x0000000000010101, 0x0000000000020202, 0x0000000000040404, 0x0000000000080808, 
	0x0000000000101010, 0x0000000000202020, 0x0000000000404040, 0x0000000000808080, 
	0x0000000001010101, 0x0000000002020202, 0x0000000004040404, 0x0000000008080808, 
	0x0000000010101010, 0x0000000020202020, 0x0000000040404040, 0x0000000080808080, 
	0x0000000101010101, 0x0000000202020202, 0x0000000404040404, 0x0000000808080808, 
	0x0000001010101010, 0x0000002020202020, 0x0000004040404040, 0x0000008080808080, 
	0x0000010101010101, 0x0000020202020202, 0x0000040404040404, 0x0000080808080808, 
	0x0000101010101010, 0x0000202020202020, 0x0000404040404040, 0x0000808080808080, 
	0x0001010101010101, 0x0002020202020202, 0x0004040404040404, 0x0008080808080808, 
	0x0010101010101010, 0x0020202020202020, 0x0040404040404040, 0x0080808080808080, 
];

#[rustfmt::skip]
const EAST: [u64; 64] = [
	0x00000000000000fe, 0x00000000000000fc, 0x00000000000000f8, 0x00000000000000f0, 
	0x00000000000000e0, 0x00000000000000c0, 0x0000000000000080, 0x0000000000000000, 
	0x000000000000fe00, 0x000000000000fc00, 0x000000000000f800, 0x000000000000f000, 
	0x000000000000e000, 0x000000000000c000, 0x0000000000008000, 0x0000000000000000, 
	0x0000000000fe0000, 0x0000000000fc0000, 0x0000000000f80000, 0x0000000000f00000, 
	0x0000000000e00000, 0x0000000000c00000, 0x0000000000800000, 0x0000000000000000, 
	0x00000000fe000000, 0x00000000fc000000, 0x00000000f8000000, 0x00000000f0000000, 
	0x00000000e0000000, 0x00000000c0000000, 0x0000000080000000, 0x0000000000000000, 
	0x000000fe00000000, 0x000000fc00000000, 0x000000f800000000, 0x000000f000000000, 
	0x000000e000000000, 0x000000c000000000, 0x0000008000000000, 0x0000000000000000, 
	0x0000fe0000000000, 0x0000fc0000000000, 0x0000f80000000000, 0x0000f00000000000, 
	0x0000e00000000000, 0x0000c00000000000, 0x0000800000000000, 0x0000000000000000, 
	0x00fe000000000000, 0x00fc000000000000, 0x00f8000000000000, 0x00f0000000000000, 
	0x00e0000000000000, 0x00c0000000000000, 0x0080000000000000, 0x0000000000000000, 
	0xfe00000000000000, 0xfc00000000000000, 0xf800000000000000, 0xf000000000000000, 
	0xe000000000000000, 0xc000000000000000, 0x8000000000000000, 0x0000000000000000, 
];

#[rustfmt::skip]
const WEST: [u64; 64] = [
	0x0000000000000000, 0x0000000000000001, 0x0000000000000003, 0x0000000000000007, 
	0x000000000000000f, 0x000000000000001f, 0x000000000000003f, 0x000000000000007f, 
	0x0000000000000000, 0x0000000000000100, 0x0000000000000300, 0x0000000000000700, 
	0x0000000000000f00, 0x0000000000001f00, 0x0000000000003f00, 0x0000000000007f00, 
	0x0000000000000000, 0x0000000000010000, 0x0000000000030000, 0x0000000000070000, 
	0x00000000000f0000, 0x00000000001f0000, 0x00000000003f0000, 0x00000000007f0000, 
	0x0000000000000000, 0x0000000001000000, 0x0000000003000000, 0x0000000007000000, 
	0x000000000f000000, 0x000000001f000000, 0x000000003f000000, 0x000000007f000000, 
	0x0000000000000000, 0x0000000100000000, 0x0000000300000000, 0x0000000700000000, 
	0x0000000f00000000, 0x0000001f00000000, 0x0000003f00000000, 0x0000007f00000000, 
	0x0000000000000000, 0x0000010000000000, 0x0000030000000000, 0x0000070000000000, 
	0x00000f0000000000, 0x00001f0000000000, 0x00003f0000000000, 0x00007f0000000000, 
	0x0000000000000000, 0x0001000000000000, 0x0003000000000000, 0x0007000000000000, 
	0x000f000000000000, 0x001f000000000000, 0x003f000000000000, 0x007f000000000000, 
	0x0000000000000000, 0x0100000000000000, 0x0300000000000000, 0x0700000000000000, 
	0x0f00000000000000, 0x1f00000000000000, 0x3f00000000000000, 0x7f00000000000000, 
];

#[rustfmt::skip]
const NORTH_EAST: [u64; 64] = [
	0x8040201008040200, 0x0080402010080400, 0x0000804020100800, 0x0000008040201000, 
	0x0000000080402000, 0x0000000000804000, 0x0000000000008000, 0x0000000000000000, 
	0x4020100804020000, 0x8040201008040000, 0x0080402010080000, 0x0000804020100000, 
	0x0000008040200000, 0x0000000080400000, 0x0000000000800000, 0x0000000000000000, 
	0x2010080402000000, 0x4020100804000000, 0x8040201008000000, 0x0080402010000000, 
	0x0000804020000000, 0x0000008040000000, 0x0000000080000000, 0x0000000000000000, 
	0x1008040200000000, 0x2010080400000000, 0x4020100800000000, 0x8040201000000000, 
	0x0080402000000000, 0x0000804000000000, 0x0000008000000000, 0x0000000000000000, 
	0x0804020000000000, 0x1008040000000000, 0x2010080000000000, 0x4020100000000000, 
	0x8040200000000000, 0x0080400000000000, 0x0000800000000000, 0x0000000000000000, 
	0x0402000000000000, 0x0804000000000000, 0x1008000000000000, 0x2010000000000000, 
	0x4020000000000000, 0x8040000000000000, 0x0080000000000000, 0x0000000000000000, 
	0x0200000000000000, 0x0400000000000000, 0x0800000000000000, 0x1000000000000000, 
	0x2000000000000000, 0x4000000000000000, 0x8000000000000000, 0x0000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	];

#[rustfmt::skip]
const SOUTH_EAST: [u64; 64] = [
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000002, 0x0000000000000004, 0x0000000000000008, 0x0000000000000010, 
	0x0000000000000020, 0x0000000000000040, 0x0000000000000080, 0x0000000000000000, 
	0x0000000000000204, 0x0000000000000408, 0x0000000000000810, 0x0000000000001020, 
	0x0000000000002040, 0x0000000000004080, 0x0000000000008000, 0x0000000000000000, 
	0x0000000000020408, 0x0000000000040810, 0x0000000000081020, 0x0000000000102040, 
	0x0000000000204080, 0x0000000000408000, 0x0000000000800000, 0x0000000000000000, 
	0x0000000002040810, 0x0000000004081020, 0x0000000008102040, 0x0000000010204080, 
	0x0000000020408000, 0x0000000040800000, 0x0000000080000000, 0x0000000000000000, 
	0x0000000204081020, 0x0000000408102040, 0x0000000810204080, 0x0000001020408000, 
	0x0000002040800000, 0x0000004080000000, 0x0000008000000000, 0x0000000000000000, 
	0x0000020408102040, 0x0000040810204080, 0x0000081020408000, 0x0000102040800000, 
	0x0000204080000000, 0x0000408000000000, 0x0000800000000000, 0x0000000000000000, 
	0x0002040810204080, 0x0004081020408000, 0x0008102040800000, 0x0010204080000000, 
	0x0020408000000000, 0x0040800000000000, 0x0080000000000000, 0x0000000000000000, 
	];

#[rustfmt::skip]
const SOUTH_WEST: [u64; 64] = [
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000000, 0x0000000000000001, 0x0000000000000002, 0x0000000000000004, 
	0x0000000000000008, 0x0000000000000010, 0x0000000000000020, 0x0000000000000040, 
	0x0000000000000000, 0x0000000000000100, 0x0000000000000201, 0x0000000000000402, 
	0x0000000000000804, 0x0000000000001008, 0x0000000000002010, 0x0000000000004020, 
	0x0000000000000000, 0x0000000000010000, 0x0000000000020100, 0x0000000000040201, 
	0x0000000000080402, 0x0000000000100804, 0x0000000000201008, 0x0000000000402010, 
	0x0000000000000000, 0x0000000001000000, 0x0000000002010000, 0x0000000004020100, 
	0x0000000008040201, 0x0000000010080402, 0x0000000020100804, 0x0000000040201008, 
	0x0000000000000000, 0x0000000100000000, 0x0000000201000000, 0x0000000402010000, 
	0x0000000804020100, 0x0000001008040201, 0x0000002010080402, 0x0000004020100804, 
	0x0000000000000000, 0x0000010000000000, 0x0000020100000000, 0x0000040201000000, 
	0x0000080402010000, 0x0000100804020100, 0x0000201008040201, 0x0000402010080402, 
	0x0000000000000000, 0x0001000000000000, 0x0002010000000000, 0x0004020100000000, 
	0x0008040201000000, 0x0010080402010000, 0x0020100804020100, 0x0040201008040201, 
	];

#[rustfmt::skip]
const NORTH_WEST: [u64; 64] = [
	0x0000000000000000, 0x0000000000000100, 0x0000000000010200, 0x0000000001020400, 
	0x0000000102040800, 0x0000010204081000, 0x0001020408102000, 0x0102040810204000, 
	0x0000000000000000, 0x0000000000010000, 0x0000000001020000, 0x0000000102040000, 
	0x0000010204080000, 0x0001020408100000, 0x0102040810200000, 0x0204081020400000, 
	0x0000000000000000, 0x0000000001000000, 0x0000000102000000, 0x0000010204000000, 
	0x0001020408000000, 0x0102040810000000, 0x0204081020000000, 0x0408102040000000, 
	0x0000000000000000, 0x0000000100000000, 0x0000010200000000, 0x0001020400000000, 
	0x0102040800000000, 0x0204081000000000, 0x0408102000000000, 0x0810204000000000, 
	0x0000000000000000, 0x0000010000000000, 0x0001020000000000, 0x0102040000000000, 
	0x0204080000000000, 0x0408100000000000, 0x0810200000000000, 0x1020400000000000, 
	0x0000000000000000, 0x0001000000000000, 0x0102000000000000, 0x0204000000000000, 
	0x0408000000000000, 0x0810000000000000, 0x1020000000000000, 0x2040000000000000, 
	0x0000000000000000, 0x0100000000000000, 0x0200000000000000, 0x0400000000000000, 
	0x0800000000000000, 0x1000000000000000, 0x2000000000000000, 0x4000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 
	];


#[cfg(test)]
mod tests {
    use crate::test_helpers::*;
    use super::*;
    use crate::Square;
    
    macro_rules! test_all_pinned_piece_legal_moves {
        ($test_description:expr,
         $pinned_pieces:expr,
         $expected:expr,
         $actual:expr $(,)?
        ) => {{
            let mut pinned_pieces = $pinned_pieces;
            while pinned_pieces != 0 {
                let pinned_piece = $pinned_pieces & pinned_pieces.wrapping_neg();
                //debug_assert!($expected.contains_key(&pinned_piece));
                debug_assert!($expected.iter().any(|(pinned, _)| *pinned == pinned_piece));
                debug_assert!($actual.iter().any(|(pinned, _)| *pinned == pinned_piece));

                test_bitboard_eq!(
                    $test_description,
                    // *$expected.get(&pinned_piece).unwrap(),
                    $expected.iter().find(|&&(pinned, _)| pinned == pinned_piece).unwrap().1,
                    $actual.iter().find(|&&(pinned, _)| pinned == pinned_piece).unwrap().1,
                );
                pinned_pieces ^= pinned_piece;
            }
        }};
    }

    // One big mammoth test as initialising the attack table is expensive so only want to do it once
    #[test]
    #[ignore]
    fn test_board_status() {
        let attack_table = AttackTable::init();

        let bitboard = BitBoard::try_from(
            "r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1"
        ).unwrap(); 
        let board_status = attack_table.get_board_status(&bitboard, Colour::Black);

        test_bitboard_eq!(
            "Checking for black danger squares for r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1",
            fen_to_hex("8/5K1K/K1K3KK/1K1KKKK1/K1KKK1KK/KKKKKKKK/KK1KKKKK/1KKKKKKK w - - 0 1"),
            board_status.danger_squares,
        );
        
        test_bitboard_eq!(
            "Checking for black pinned pieces for r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1",
            fen_to_hex("8/5p2/2n5/4p3/8/8/8/8 w - - 0 1"),
            board_status.pinned_pieces,
        );
        
        let mut expected_pinned_legal_squares: [(u64, u64); 8] = [(0, 0); 8];
        expected_pinned_legal_squares[0] = (1 << Square::C6 as u32, fen_to_hex("8/3n4/2n5/1n6/8/8/8/8 w - - 0 1"));
        expected_pinned_legal_squares[1] = (1 << Square::F7 as u32, fen_to_hex("8/5p2/6p1/7p/8/8/8/8 w - - 0 1"));
        expected_pinned_legal_squares[2] = (1 << Square::E5 as u32, fen_to_hex("8/4p3/4p3/4p3/4p3/4p3/4p3/4p3 w - - 0 1"));

        test_all_pinned_piece_legal_moves!(
            "Checking the legal squares for each pinned piece for r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1 w - - 0 1",
            board_status.pinned_pieces,
            &expected_pinned_legal_squares[0..3],
            &board_status.pinned_pseudo_legal_squares[0..3],
        );
        
        // assert_eq!(0, board_status.num_discovered_checks, 
        //           "Check no discovered checks for r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1"
        // );
        
        let expected_check_squares: [u64; 6] = [
            fen_to_hex("8/8/8/8/8/8/5p1p/8 w - - 0 1"), // pawns
            fen_to_hex("8/8/8/8/8/8/5kkk/5k1k w - - 0 1"), // king
            fen_to_hex("8/8/8/8/8/8/5qqq/4qq1q w - - 0 1"), // queen
            fen_to_hex("8/8/8/8/8/8/5p1p/8 w - - 0 1"), // bishop
            fen_to_hex("8/8/8/8/8/5n1n/4n3/8 w - - 0 1"), // knight
            fen_to_hex("8/8/8/8/8/8/6r1/4rr1r w - - 0 1"), // rook
        ];
        
        // for (i, (expected, actual)) in expected_check_squares.iter()
        //                                                      .zip(board_status.check_squares.iter())
        //                                                      .enumerate() {
        //     let description = format!(
        //         "Check squares for {:?} for r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1", 
        //         Piece::try_from(i).unwrap()
        //     );
        //     test_bitboard_eq!(
        //         description,
        //         *expected,
        //         *actual,
        //     );
        // }

        
        // -----------------------------------------------------------------------------------------
        let bitboard = BitBoard::try_from(
            "rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR"
        ).unwrap();
        let board_status = attack_table.get_board_status(&bitboard, Colour::White);
        
        test_bitboard_eq!(
            "Checking for white danger squaares for rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR",
            fen_to_hex("1kkkkkkk/kk1kkkkk/kkkkkkkk/k1kkk1kk/1kkkkkk1/k1k1k1kk/4k2k/8 w - - 0 1"),
            board_status.danger_squares,
        );

        test_bitboard_eq!(
            "Checking for white pinned pieces for rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR",
            fen_to_hex("8/8/8/8/8/6P1/4P3/8 w - - 0 1"),
            board_status.pinned_pieces,
        );

        let mut expected_pinned_legal_squares: [(u64, u64); 8] = [(0, 0); 8];
        expected_pinned_legal_squares[0] = (1 << Square::E2 as u32, fen_to_hex("4P3/4P3/4P3/4P3/4P3/4P3/4P3/8 w - - 0 1"));
        expected_pinned_legal_squares[1] = (1 << Square::G3 as u32, fen_to_hex("8/8/8/8/7P/6P1/5P2/8 w - - 0 1"));
        
        test_all_pinned_piece_legal_moves!(
            "Checking the legal squares for each pinned piece for rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR",
            board_status.pinned_pieces,
            expected_pinned_legal_squares,
            board_status.pinned_pseudo_legal_squares,
        );
        
        // assert_eq!(0, board_status.num_discovered_checks,
        //            "Check no discovered checks for rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR"
        // );

        let expected_check_squares: [u64; 6] = [
            fen_to_hex("8/5P1P/8/8/8/8/8/8 w - - 0 1"), // pawns
            fen_to_hex("5K1K/5KKK/8/8/8/8/8/8 w - - 0 1"), // king
            fen_to_hex("4QQ1Q/5QQQ/8/8/8/8/8/8 w - - 0 1"), // queen
            fen_to_hex("8/5P1P/8/8/8/8/8/8 w - - 0 1"), // bishop
            fen_to_hex("8/4N3/5N1N/8/8/8/8/8 w - - 0 1"), // knight
            fen_to_hex("4RR1R/6R1/8/8/8/8/8/8 w - - 0 1"), // rook
        ];

        //for (i, (expected, actual)) in expected_check_squares.iter()
        //                                                     .zip(board_status.check_squares.iter())
        //                                                     .enumerate() {
        //    let description = format!(
        //        "Check squares for {:?} for rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR", 
        //        Piece::try_from(i).unwrap()
        //    );
        //    test_bitboard_eq!(
        //        description,
        //        *expected,
        //        *actual,                
        //    );
        //}
        
        // -----------------------------------------------------------------------------------------
        let bitboard = BitBoard::try_from(
            "8/1K6/8/4k3/8/2r3P1/8/Q3R3"
        ).unwrap();

        let board_status = attack_table.get_board_status(&bitboard, Colour::Black);

        test_bitboard_eq!(
            "Checking for black danger squares for 8/1K6/8/4k3/8/2r3P1/8/Q3R3 and that they include squares behind the king",
            fen_to_hex("QQQ1Q3/Q1Q1Q3/QQQ1Q3/Q3Q3/Q3QQ1Q/Q1Q1Q3/QQ2Q3/QQQQQQQQ w - - 0 1"),
            board_status.danger_squares,
        );

        test_bitboard_eq!(
            "Checking for black pinned pieces for 8/1K6/8/4k3/8/2r3P1/8/Q3R3",
            fen_to_hex("8/8/8/8/8/2r5/8/8 w - - 0 1"),
            board_status.pinned_pieces,
        );
            
        let mut expected_pinned_legal_squares: [(u64, u64); 8] = [(0, 0); 8];
        expected_pinned_legal_squares[0] = (1 << Square::C3 as u32, fen_to_hex("8/8/8/8/3r4/2r5/1r6/r7 w - - 0 1"));

        test_all_pinned_piece_legal_moves!(
            "Checking the legal squares for each pinned piece for 8/1K6/8/4k3/8/2r3P1/8/Q3R3",
            board_status.pinned_pieces,
            expected_pinned_legal_squares,
            board_status.pinned_pseudo_legal_squares,
        );

        // assert_eq!(0, board_status.num_discovered_checks,
        //            "Check no discovered checks for 8/1K6/8/4k3/8/2r3P1/8/Q3R3"
        // );
        
        let expected_check_squares: [u64; 6] = [
            fen_to_hex("p1p5/8/8/8/8/8/8/8 w - - 0 1"), // pawns
            fen_to_hex("kkk5/k1k5/kkk5/8/8/8/8/8 w - - 0 1"), // king
            fen_to_hex("qqq5/q1qqqqqq/qqq5/1q1q4/1q2q3/1q3q2/1q4q1/1q5q w - - 0 1"), // queen
            fen_to_hex("b1b5/8/b1b5/3b4/4b3/5b2/6b1/7b w - - 0 1"), // bishop
            fen_to_hex("3n4/8/3n4/n1n5/8/8/8/8 w - - 0 1"), // knight
            fen_to_hex("1r6/r1rrrrrr/1r6/1r6/1r6/1r6/1r6/1r6 w - - 0 1"), // rook
        ];
        
        // for (i, (expected, actual)) in expected_check_squares.iter()
        //                                                      .zip(board_status.check_squares.iter())
        //                                                      .enumerate() {
        //     let description = format!(
        //         "Check squares for {:?} for 8/1K6/8/4k3/8/2r3P1/8/Q3R3",
        //         Piece::try_from(i).unwrap()
        //     );
        //     test_bitboard_eq!(
        //         description,
        //         *expected,
        //         *actual,                
        //     );
        // }
        // -----------------------------------------------------------------------------------------
        let bitboard = BitBoard::try_from(
            "8/4k3/3r4/4p3/4b3/Q5P1/7K/4R3",
        ).unwrap();
        
        let board_status = attack_table.get_board_status(&bitboard, Colour::Black);

        test_bitboard_eq!(
            "Checking for black danger squares for 8/4k3/3r4/4p3/4b3/Q5P1/7K/4R3",
            fen_to_hex("K7/K7/K2K4/K1K5/KK2KK1K/1KKKKKPK/KK2K1K1/KKKK1KKK w - - 0 1"),
            board_status.danger_squares,
        );

        test_bitboard_eq!(
            "Checking for black pinned pieces for 8/4k3/3r4/4p3/4b3/Q5P1/7K/4R3 and that two pieces blocking the king don't count as pinned",
            fen_to_hex("8/8/3r4/8/8/8/8/8 w - - 0 1"),
            board_status.pinned_pieces,
        );

        let mut expected_pinned_legal_squares: [(u64, u64); 8] = [(0, 0); 8];
        expected_pinned_legal_squares[0] = (1 << Square::D6 as u32, fen_to_hex("8/8/3r4/2r5/1r6/r7/8/8 w - - 0 1"));

        test_all_pinned_piece_legal_moves!(
            "Checking the legal squares for each pinned piece for 8/4k3/3r4/4p3/4b3/Q5P1/7K/4R3",
            board_status.pinned_pieces,
            expected_pinned_legal_squares,
            board_status.pinned_pseudo_legal_squares,
        );
        
        // -----------------------------------------------------------------------------------------
        let bitboard = BitBoard::try_from(
            "1kb5/pp6/N7/8/8/6Q1/8/1K6"
        ).unwrap();

        let board_status = attack_table.get_board_status(&bitboard, Colour::Black);

        let pieces_attacking_king = board_status.king_attacking_pieces.len();
        assert_eq!(2, pieces_attacking_king,
                  "1kb5/pp6/N7/8/8/6Q1/8/1K6 expects 2 pieces putting the king in check, but only got {}", 
                  pieces_attacking_king);
        
        let idx = board_status.king_attacking_pieces.iter().position(|&(piece, source_sq, _)| {
            piece == Piece::Knight && source_sq == 1 << Square::A6 as u32
        });
        debug_assert!(idx.is_some(), "Knight on A6 expected as king-attacking piece but not found");
        test_bitboard_eq!(
            "Checking for blocking and/or capturing squares to get out of check for the knight on A6 for 1kb5/pp6/N7/8/8/6Q1/8/1K6",
            fen_to_hex("8/8/N7/8/8/8/8/8 w - - 0 1"),
            board_status.king_attacking_pieces[idx.unwrap()].1,
        );
        
        let idx = board_status.king_attacking_pieces.iter().position(|&(piece, source_sq, _)| {
            piece == Piece::Queen && source_sq == 1 << Square::G3 as u32
        });
        debug_assert!(idx.is_some(), "Queen on G3 expected as king-attacking piece but not found");
        test_bitboard_eq!(
            "Checking for blocking to get out of check for the queen on G3 for 1kb5/pp6/N7/8/8/6Q1/8/1K6",
            fen_to_hex("8/2q5/3q4/4q3/5q2/8/8/8 w - - 0 1"),
            board_status.king_attacking_pieces[idx.unwrap()].2,
        );
        
        // -----------------------------------------------------------------------------------------
        let board = Board::new(Some("8/4k3/8/8/6p1/5p2/4K3/8 w - - 0 1")).unwrap();
        assert!(attack_table.king_in_check(&board));
        // -----------------------------------------------------------------------------------------
        let board = Board::new(Some("8/4k3/8/8/6p1/5p2/4K3/8 w - - 0 1")).unwrap();
        assert!(attack_table.king_in_check(&board));
        // -----------------------------------------------------------------------------------------
        // This test is about discovered checks, which for the timebeing has been commented out
        /*
        let bitboard = BitBoard::try_from(
            "8/8/1k6/2N5/8/1B6/5Q2/1R4K1",
        ).unwrap();

        let board_status = attack_table.get_board_status(&bitboard, Colour::White);

        let expected = (
            Piece::Bishop,
            1 << Square::B3 as u32,
            fen_to_hex("PPPPPPPP/PPPPPPPP/P1PPPPPP/P1PPPPPP/P1PPPPPP/P1PPPPPP/P1PPPPPP/P1PPPPPP"),
        );
        debug_assert!(board_status.discovered_check_pseudo_legal_squares.contains(&expected),
                "8/8/1k6/2N5/8/1B6/5Q2/1R4K1 shows the bishop on B3 as a potential discovered check"
        );
        
        let expected = (
            Piece::Knight,
            1 << Square::C5 as u32,
            fen_to_hex("PPPPPPPP/PPPPPPPP/P1PPPPPP/PP1PPPPP/PPP1PPPP/PPPP1PPP/PPPPP1PP/PPPPPPPP"),
        );
        debug_assert!(board_status.discovered_check_pseudo_legal_squares.contains(&expected),
                "8/8/1k6/2N5/8/1B6/5Q2/1R4K1 shows the knight on C5 as a potential discovered check"
        );
        */
    }
}
