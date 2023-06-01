use num_traits::FromPrimitive;

use crate::board::Board;
use crate::pawn::PawnAttackTable;
use crate::sliding_piece::Magic;
use crate::{king, knight, Piece, Colour, Square};
use crate::chess_move::{Move, MoveType};
use crate::bitboard::BitBoard;
use crate::test_helpers::bitmask_to_board;

pub struct AttackTable {
    king: Vec<u64>,
    knight: Vec<u64>,
    pawn_moves: [Vec<u64>; 2],
    pawn_attacks: [Vec<u64>; 2],
    bishop_attacks: Vec<Magic>,
    rook_attacks: Vec<Magic>,
}

pub struct BoardStatus {
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
    check_squares: [u64; 6],
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
    
    pub fn generate_legal_moves(&self, board: &Board, moves: &mut [Move; 256]) -> usize {

        let board_status = self.get_board_status(&board.bitboard, board.turn_colour,);
        
        let occupied = board.bitboard.get_entire_mask();
        
        let enemy_colour_mask = board.bitboard.get_colour_mask(!&board.turn_colour);
        
        let mut num_moves: usize = 0;
        // first check if king is in double check
        // means we only generate king moves
        if board_status.num_checking_pieces == 2 {
            let king_bitmask = board.bitboard.get_colour_piece_mask(Piece::King, board.turn_colour);
            let king_source_sq: Square = FromPrimitive::from_u32(king_bitmask.trailing_zeros()).unwrap();
            
            let king_captures = self.get_single_piece_captures(
                Piece::King, 
                board.turn_colour, 
                king_bitmask,
                occupied, 
                enemy_colour_mask,
            );
            
            // mask out danger squares
            let king_captures = king_captures & !board_status.danger_squares;

            // in descending order of piece value
            for captured_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
                let mut piece_captures = king_captures & board.bitboard.get_colour_piece_mask(
                    captured_piece, !&board.turn_colour,
                );
                // loop through each capture available for that piece type
                while piece_captures != 0 {
                    let dest_sq = piece_captures & piece_captures.wrapping_neg();
                    piece_captures ^= dest_sq;
                    let dest_sq: Square = FromPrimitive::from_u32(dest_sq.trailing_zeros()).unwrap();
                    moves[num_moves] = Move {
                        source_sq: king_source_sq,
                        dest_sq,
                        piece: Piece::King,
                        move_type: MoveType::Capture(captured_piece),
                    };
                    num_moves += 1;
                }
            }
            
            let king_moves = self.get_single_piece_moves(
                Piece::King,
                board.turn_colour,
                king_bitmask,
                occupied,
            );

            // mask out danger squares
            let mut king_moves = king_moves & !board_status.danger_squares;
            
            while king_moves != 0 {
                let dest_sq = king_moves & king_moves.wrapping_neg();
                king_moves ^= dest_sq;
                let dest_sq: Square = FromPrimitive::from_u32(dest_sq.trailing_zeros()).unwrap();
                moves[num_moves] = Move {
                    source_sq: king_source_sq,
                    dest_sq,
                    piece: Piece::King,
                    move_type: MoveType::Quiet,
                };
                num_moves += 1;
            }
            
            return num_moves;            

        /*
         * Ways to get out of a single check:
         * 1. Capture the checking piece with any piece (can include capture-promotions and en-passant)
         * 2. Block the check with any piece apart from the king (can include promotions)
         * 3. Move the king to a non danger square (either quiet or capture)
         */
        } else if board_status.num_checking_pieces == 1 {
            let (checking_piece, checking_piece_mask, block_squares) = board_status.king_attacking_pieces[0];
            
            let checking_piece_sq: Square = FromPrimitive::from_u32(checking_piece_mask.trailing_zeros()).unwrap();
            
            // Captures of the checking piece first. Non king pieces can only capture the checking 
            // piece to stop check
            for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
                let mut all_pieces = board.bitboard.get_colour_piece_mask(piece_type, board.turn_colour);
                
                // a pinned piece can't do anything about stopping check, remove them
                all_pieces &= !board_status.pinned_pieces;

                while all_pieces != 0 {
                    let one_piece = all_pieces & all_pieces.wrapping_neg();
                    all_pieces ^= one_piece;

                    let captures = self.get_single_piece_captures(
                        piece_type,
                        board.turn_colour,
                        one_piece, 
                        occupied,
                        enemy_colour_mask,
                    );
                    
                    let captures_of_checking_piece = captures & checking_piece_mask;
                    if captures_of_checking_piece == 0 {
                        continue;
                    }
                    
                    // for each piece, should only be one way to capture the checking piece
                    assert!(captures_of_checking_piece.is_power_of_two());

                    let source_sq: Square = FromPrimitive::from_u32(one_piece.trailing_zeros()).unwrap();
                    
                    // Means we can capture the checking piece via pawn capture-promotion. Hooray!
                    if piece_type == Piece::Pawn && 
                        (checking_piece_mask & 0xFF00000000000000 != 0 || checking_piece_mask & 0xFF != 0) {

                        for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                            moves[num_moves] = Move {
                                source_sq,
                                dest_sq: checking_piece_sq,
                                piece: piece_type,
                                move_type: MoveType::CapturePromotion(checking_piece, promotion_piece), 
                            };
                            num_moves += 1;
                        }
                    } else {
                        moves[num_moves] = Move {
                            source_sq,
                            dest_sq: checking_piece_sq,
                            piece: piece_type,
                            move_type: MoveType::Capture(checking_piece)
                        };
                        num_moves += 1;
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
                        !&board.turn_colour,
                        ep_square_mask,
                        occupied,
                        board.bitboard.get_colour_piece_mask(Piece::Pawn, board.turn_colour),
                    );

                    while ep_captures != 0 {
                        let ep_capture = ep_captures & ep_captures.wrapping_neg();
                        ep_captures ^= ep_capture;

                        if ep_capture & board_status.pinned_pieces == 0 {
                            moves[num_moves] = Move {
                                source_sq: FromPrimitive::from_u32(ep_capture.trailing_zeros()).unwrap(),
                                dest_sq: ep_square,
                                piece: Piece::Pawn,
                                move_type: MoveType::EnPassant,
                            };
                            num_moves += 1;
                        }
                    }
                }
            }
            
            // King captures. The king can get out of check by capturing any available piece
            let king_bitmask = board.bitboard.get_colour_piece_mask(Piece::King, board.turn_colour);
            let king_captures = self.get_single_piece_captures(
                Piece::King,
                board.turn_colour,
                king_bitmask,
                occupied,
                enemy_colour_mask, 
            );
            
            // remove danger squares
            let king_captures = king_captures & !board_status.danger_squares;
            
            // if there are available captures for the king
            if king_captures != 0 {
                let source_sq: Square = FromPrimitive::from_u32(king_bitmask.trailing_zeros()).unwrap();
                for piece_type in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
                    let mut all_captures = board.bitboard.get_colour_piece_mask(piece_type, !&board.turn_colour)
                            & king_captures;
                    
                    while all_captures != 0 {
                        let king_capture = all_captures & all_captures.wrapping_neg();
                        all_captures ^= king_capture;
                        let dest_sq: Square = FromPrimitive::from_u32(king_capture.trailing_zeros()).unwrap();       
                        moves[num_moves] = Move {
                            source_sq,
                            dest_sq,
                            piece: Piece::King,
                            move_type: MoveType::Capture(piece_type),
                        };
                        num_moves += 1;
                    }
                }
            }
            
            // captures of checking piece via en-passant
            // Normally we have to ensure doing en-passant doesn't leave us in check
            // but in the situation where the pawn we can capture with ep is the one putting us in 
            // check, then we know it's not possible for capturing it to leave us in check
            match board.en_passant {
                Some(ep_sq) if ep_sq == checking_piece_sq => {
                    // get the pawns that can ep by getting pawn captures for the opposite colour
                    // from the ep square
                    let mut ep_captures = self.get_single_piece_captures(
                        Piece::Pawn,
                        !&board.turn_colour,
                        1 << ep_sq as u32,
                        occupied,
                        board.bitboard.get_colour_mask(board.turn_colour),
                    );
                    while ep_captures != 0 {
                        let ep_capture = ep_captures & ep_captures.wrapping_neg();
                        ep_captures ^= ep_capture;
                        let source_sq: Square = FromPrimitive::from_u32(ep_capture.trailing_zeros()).unwrap();
                        moves[num_moves] = Move {
                            source_sq,
                            dest_sq: ep_sq,
                            piece: Piece::Pawn,
                            move_type: MoveType::EnPassant,
                        };
                        num_moves += 1;
                    }
                },
                _ => {},
            }
            
            // king moves to get out of check
            let king_quiet_moves = self.get_single_piece_moves(
                Piece::King,
                board.turn_colour,
                king_bitmask,
                occupied
            );
            let mut king_quiet_moves = king_quiet_moves & !board_status.danger_squares;
            let source_sq: Square = FromPrimitive::from_u32(king_bitmask.trailing_zeros()).unwrap();
            while king_quiet_moves != 0 {
                let king_quiet_move = king_quiet_moves & king_quiet_moves.wrapping_neg();
                let dest_sq: Square = FromPrimitive::from_u32(king_quiet_move.trailing_zeros()).unwrap();
                king_quiet_moves ^= king_quiet_move;
                moves[num_moves] = Move {
                    source_sq,
                    dest_sq,
                    piece: Piece::King,
                    move_type: MoveType::Quiet, 
                };
                num_moves += 1;
            }
            
            // other piece moves to block check
            // Only sliding pieces can be blocked
            if [Piece::Queen, Piece::Rook, Piece::Bishop].contains(&checking_piece) {
                for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen]  {
                    let mut all_pieces = board.bitboard.get_colour_piece_mask(piece_type, board.turn_colour);
                    while all_pieces != 0 {
                        let one_piece = all_pieces & all_pieces.wrapping_neg();
                        all_pieces ^= one_piece;
                        let piece_moves = self.get_single_piece_moves(
                            piece_type,
                            board.turn_colour,
                            one_piece,
                            occupied,
                        ) & block_squares;

                        let mut piece_moves = if one_piece & board_status.pinned_pieces != 0 {
                            let pseudo_legal_squares = board_status.pinned_pseudo_legal_squares
                                .iter()
                                .find(|&&(first, _)| first == one_piece)
                                .map(|&(_, second)| second)
                                .unwrap();
                            piece_moves & pseudo_legal_squares
                        } else {
                            piece_moves
                        };

                        while piece_moves != 0 {
                            let piece_move = piece_moves & piece_moves.wrapping_neg();
                            piece_moves ^= piece_move;

                            let source_sq: Square = FromPrimitive::from_u32(one_piece.trailing_zeros()).unwrap();
                            let dest_sq: Square = FromPrimitive::from_u32(piece_move.trailing_zeros()).unwrap();
                            
                            // Can block with a move promotion. Hooray!
                            if piece_type == Piece::Pawn &&
                                (piece_move & 0xFF00000000000000 != 0 || piece_move & 0xFF != 0) {

                                for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                                    moves[num_moves] = Move {
                                        source_sq,
                                        dest_sq,
                                        piece: piece_type,
                                        move_type: MoveType::MovePromotion(promotion_piece),
                                    };
                                    num_moves += 1;
                                }
                            } else if piece_type == Piece::Pawn &&
                                (
                                    (one_piece & 0xFF00 != 0 && piece_move & 0xFF000000 != 0) ||
                                    (one_piece & 0xFF000000000000 != 0 && piece_move & 0xFF00000000 != 0)
                                ) {
                                // println!("asdf");
                                moves[num_moves] = Move {
                                    source_sq,
                                    dest_sq,
                                    piece: piece_type,
                                    move_type: MoveType::DoublePawnPush,
                                };
                                num_moves += 1;
                            } else {
                                moves[num_moves] = Move {
                                    source_sq,
                                    dest_sq,
                                    piece: piece_type,
                                    move_type: MoveType::Quiet,
                                };
                                num_moves += 1;                            
                            }
                        }
                    }
                }
            }
            return num_moves;
        }
        
        // If reaching this point, the king is not in check
        
        // Insert castling here
        // We already know if reaching this point the king is not in check
        // then for kingside/queenside, if it's still enabled, then check if the
        // squares between the king and rook are empty, and that the intermediate squares are not
        // intersecting with the danger squares
        // if castling is still enabled for a side, then we already know the rook and king are still
        // on their origin squares, as board.make_move() disables castling if the king is ever moved
        // and if the rooks ever move/are captured
        let (kingside, queenside) = self.get_legal_castling_rights(board, &board_status);
        
        if kingside {
            let (source_sq, dest_sq) = match board.turn_colour {
                Colour::White => (Square::E1, Square::G1),
                Colour::Black => (Square::E8, Square::G8),
            };
            
            moves[num_moves] = Move {
                source_sq,
                dest_sq,
                piece: Piece::King,
                move_type: MoveType::CastleKingSide,
            };
            num_moves += 1;
        }

        if queenside {
            let (source_sq, dest_sq) = match board.turn_colour {
                Colour::White => (Square::E1, Square::C1),
                Colour::Black => (Square::E8, Square::C8),
            };
            
            moves[num_moves] = Move {
                source_sq,
                dest_sq,
                piece: Piece::King,
                move_type: MoveType::CastleQueenSide,
            };
            num_moves += 1;
        }
        
        
        
        // En-passant - Holy Hell
        // En-passant is a tricky one to ensure legality, as it could potentially put yourself
        // in check if the two pawns involved are blocking check
        // Easiest to remove the two pawns from the bitmask and check if it leaves the king in check
        // en-passant is rare enough that its efficiency isn't too important 
        // To check if en-passant is possible, place a pawn of the opposing colour on the ep square
        // and see if its attacks intersect with any friendly pawns
        if let Some(ep_square) = board.en_passant {
            let ep_square_mask = 1 << ep_square as u32;
            // as we only want the intersection with pawns we make the last argument just the pawn mask
            let mut ep_captures = self.get_single_piece_captures(
                Piece::Pawn,
                !&board.turn_colour,
                ep_square_mask,
                occupied,
                board.bitboard.get_colour_piece_mask(Piece::Pawn, board.turn_colour),
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
                    moves[num_moves] = Move {
                        source_sq: FromPrimitive::from_u32(ep_capture.trailing_zeros()).unwrap(),
                        dest_sq: ep_square,
                        piece: Piece::Pawn,
                        move_type: MoveType::EnPassant,
                    };                        
                    num_moves += 1;
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
                            board.turn_colour,
                        );
                        // println!("king mask = {}", bitmask_to_board(king_mask));
                        
                        let king_idx = king_mask.trailing_zeros() as usize;

                        // we've already checked if the pawn is pinned, this is purely checking
                        // if removing both pawns reveals check, that can only happen horizontally
                        let rook_horizontal_attacks = self.get_single_piece_pseudo_attacks(
                            Piece::Rook, 
                            !&board.turn_colour, 
                            king_mask, 
                            occupied_tmp
                        ) & (EAST[king_idx] | WEST[king_idx]);

                        let rook_queen_mask = board.bitboard.get_colour_piece_mask(
                            Piece::Rook,
                            !&board.turn_colour,
                        ) | board.bitboard.get_colour_piece_mask(
                            Piece::Queen,
                            !&board.turn_colour,
                        );

                        // results in check, that's illegal
                        if rook_horizontal_attacks & rook_queen_mask != 0 {
                            continue;
                        }
                    }

                    // if reaching this point, all legality checks for en-passant have passed
                    moves[num_moves] = Move {
                        source_sq: FromPrimitive::from_u32(ep_capture.trailing_zeros()).unwrap(),
                        dest_sq: ep_square,
                        piece: Piece::Pawn,
                        move_type: MoveType::EnPassant,
                    };                        
                    num_moves += 1;
                }
            }
        }
        
        // Captures
        // In ascending order of value
        for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
            let all_pieces = board.bitboard.get_colour_piece_mask(piece_type, board.turn_colour);

            let mut unpinned = all_pieces & !board_status.pinned_pieces;
            while unpinned != 0 {
                let one_piece = unpinned & unpinned.wrapping_neg();
                unpinned ^= one_piece;
                
                let captures = self.get_single_piece_captures(
                    piece_type,
                    board.turn_colour,
                    one_piece,
                    occupied,
                    board.bitboard.get_colour_mask(!&board.turn_colour),
                );
                
                // TODO, can skip this loop if captures == 0
                // in reverse order of piece value
                for captured_piece_type in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
                    let mut all_captures_of_piece = captures & board.bitboard.get_colour_piece_mask(
                        captured_piece_type, !&board.turn_colour,
                    );

                    while all_captures_of_piece != 0 {
                        let one_capture = all_captures_of_piece & all_captures_of_piece.wrapping_neg();
                        all_captures_of_piece ^= one_capture;

                        let source_sq = FromPrimitive::from_u32(one_piece.trailing_zeros()).unwrap();
                        let dest_sq = FromPrimitive::from_u32(one_capture.trailing_zeros()).unwrap();
                        
                        if piece_type == Piece::Pawn && (one_capture & 0xFF00000000000000 != 0 || one_capture & 0xFF != 0) {

                            for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                                moves[num_moves] = Move {
                                    source_sq,
                                    dest_sq,
                                    piece: Piece::Pawn,
                                    move_type: MoveType::CapturePromotion(captured_piece_type, promotion_piece),
                                };
                                num_moves += 1;
                            }
                        } else {
                            moves[num_moves] = Move {
                                source_sq,
                                dest_sq,
                                piece: piece_type,
                                move_type: MoveType::Capture(captured_piece_type), 
                            };
                            num_moves += 1;
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
                    board.turn_colour,
                    pinned_piece,
                    occupied,
                    board.bitboard.get_colour_mask(!&board.turn_colour),
                );
                
                let legal_captures = captures & pseudo_legal_squares;
                
                // in reverse order of piece value
                for captured_piece_type in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
                    let mut all_captures_of_piece = legal_captures & board.bitboard.get_colour_piece_mask(
                        captured_piece_type, !&board.turn_colour,
                    );

                    while all_captures_of_piece != 0 {
                        let one_capture = all_captures_of_piece & all_captures_of_piece.wrapping_neg();
                        all_captures_of_piece ^= one_capture;

                        if piece_type == Piece::Pawn && (one_capture & 0xFF00000000000000 != 0 || one_capture & 0xFF != 0) {
                            for promotion_piece in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight] {
                                moves[num_moves] = Move {
                                    source_sq: FromPrimitive::from_u32(pinned_piece.trailing_zeros()).unwrap(),
                                    dest_sq: FromPrimitive::from_u32(one_capture.trailing_zeros()).unwrap(),
                                    piece: Piece::Pawn,
                                    move_type: MoveType::CapturePromotion(captured_piece_type, promotion_piece),
                                };
                                num_moves += 1;
                            }                            
                        } else {
                            moves[num_moves] = Move {
                                source_sq: FromPrimitive::from_u32(pinned_piece.trailing_zeros()).unwrap(),
                                dest_sq: FromPrimitive::from_u32(one_capture.trailing_zeros()).unwrap(),
                                piece: piece_type,
                                move_type: MoveType::Capture(captured_piece_type), 
                            };
                            num_moves += 1;  
                        }
                    }
                }
            }
        }
        
        // King captures here
        let king_position = board.bitboard.get_colour_piece_mask(Piece::King, board.turn_colour);
        
        let king_captures = self.get_single_piece_captures(
            Piece::King,
            board.turn_colour,
            king_position,
            occupied,
            board.bitboard.get_colour_mask(!&board.turn_colour),
        );
        
        let king_captures = king_captures & !board_status.danger_squares;
        
        for captured_piece_type in [Piece::Queen, Piece::Rook, Piece::Bishop, Piece::Knight, Piece::Pawn] {
            let mut all_captures_of_piece = king_captures & board.bitboard.get_colour_piece_mask(
                captured_piece_type, !&board.turn_colour,
            );

            while all_captures_of_piece != 0 {
                let one_capture = all_captures_of_piece & all_captures_of_piece.wrapping_neg();
                all_captures_of_piece ^= one_capture;

                moves[num_moves] = Move {
                    source_sq: FromPrimitive::from_u32(king_position.trailing_zeros()).unwrap(),
                    dest_sq: FromPrimitive::from_u32(one_capture.trailing_zeros()).unwrap(),
                    piece: Piece::King,
                    move_type: MoveType::Capture(captured_piece_type),
                };
                num_moves += 1;
            }
        }
        
        // Quiet moves
        for piece_type in [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen] {
            let mut all_pieces = board.bitboard.get_colour_piece_mask(
                piece_type,
                board.turn_colour,
            );

            while all_pieces != 0 {
                let one_piece = all_pieces & all_pieces.wrapping_neg();
                all_pieces ^= one_piece;
                
                let source_sq: Square = FromPrimitive::from_u32(one_piece.trailing_zeros()).unwrap();

                let quiet_moves = self.get_single_piece_moves(
                    piece_type,
                    board.turn_colour,
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
                            moves[num_moves] = Move {
                                source_sq,
                                dest_sq,
                                piece: piece_type,
                                move_type: MoveType::MovePromotion(promotion_piece),
                            };
                            num_moves += 1;
                        }
                    } else if piece_type == Piece::Pawn && 
                        (
                            (one_piece & 0xFF00 != 0 && quiet_move & 0xFF000000 != 0) ||
                            (one_piece & 0xFF000000000000 != 0 && quiet_move & 0xFF00000000 != 0)
                        ) {
                        // println!("fuckers {}", num_moves);
                        moves[num_moves] = Move {
                            source_sq,
                            dest_sq,
                            piece: piece_type,
                            move_type: MoveType::DoublePawnPush,
                        };
                        // println!("{:?}", moves[num_moves]);
                        num_moves += 1;
                    } else {
                        moves[num_moves] = Move {
                            source_sq,
                            dest_sq,
                            piece: piece_type,
                            move_type: MoveType::Quiet,
                        };
                        num_moves += 1;
                    }
                }
            }
        }

        // king moves
        let king_position = board.bitboard.get_colour_piece_mask(Piece::King, board.turn_colour);
        assert!(king_position.is_power_of_two());
        let source_sq: Square = FromPrimitive::from_u32(king_position.trailing_zeros()).unwrap();
        let quiet_king_moves = self.get_single_piece_moves(
            Piece::King,
            board.turn_colour,
            king_position,
            occupied,
        );
        let mut quiet_king_moves = quiet_king_moves & !board_status.danger_squares;

        while quiet_king_moves != 0 {
            let king_move = quiet_king_moves & quiet_king_moves.wrapping_neg();
            quiet_king_moves ^= king_move;
            let dest_sq = FromPrimitive::from_u32(king_move.trailing_zeros()).unwrap();

            moves[num_moves] = Move {
                source_sq,
                dest_sq,
                piece: Piece::King,
                move_type: MoveType::Quiet,
            };
            num_moves +=1;
        }
        num_moves
            
        /*
         * simple algorithm:
         * if king in check from 2 pieces:
         *      generate king moves
         *      (danger_squares & king_moves) ^ king_moves
         *      add remaining moves to list
         *      return move_list
         * else if king in check from 1 piece:
         *      for each piece type:
         *          generate moves
         *          AND with checking piece
         *          if capture available
         *              add to move list
         *          AND with blocker squares
         *              add each to list
         *          generate king moves as above and add to list
         *      return move_list
         * else (king not in check)
         * 
         * for each piece type:
         *      generate moves
         *      mask out pinned pieces
         *      add each move to the list
         * for each pinned piece:
         *      AND with allowed_ray
         *      add remaining moves to list
         * if en_passant available:
         *      pawn_attacks & en_passant_square
         *      if pseudo-legal:
         *          make move
         *          if in check:
         *              undo move
         *          else
         *              undo move
         *              add en passant move to list
         * if castling available:
         *      
         * return list
         */
        /*
        if king in check from 2 pieces:
            generate king moves
            (danger squares & king moves) ^ king moves
            for each legal king move:
                add to move list
            return move list
        else if king in check from 1 piece:
            ...
        else :
            for each piece type
                mask out pinned pieces
                generate pseudo-legal moves
                
            ...

        // check for en_passant capture
        // check for castling
         */
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
        assert!(source_sq_mask.is_power_of_two(), 
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
        assert!(source_sq_mask.is_power_of_two(), 
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
        assert!(source_sq_mask.is_power_of_two(), 
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
    
    fn get_single_piece_pseudo_moves(
            &self,
            piece: Piece,
            colour: Colour,
            source_sq_mask: u64,
            occupied: u64) -> u64 {

        // should only be 1 bit set
        assert!(source_sq_mask.is_power_of_two(), 
                      "piece = {:?}, colour = {:?}, mask = {}, occupied = {}", 
                      piece, colour, source_sq_mask, occupied);
        
        match piece {
            Piece::Pawn => {
                let idx = source_sq_mask.trailing_zeros() as usize;
                self.pawn_moves[colour as usize][idx] ^ occupied
            },
            _ => self.get_single_piece_pseudo_attacks(piece, colour, source_sq_mask, occupied),
        }
    }
    
    // For a given piece type and a bitmask of all its source squares, return
    // a bitmask of all the squares those pieces attack
    fn get_all_piece_attacks(
            &self, 
            piece: Piece, 
            colour: Colour, 
            piece_mask: u64, 
            occupied: u64) -> u64 {

        let mut piece_mask = piece_mask;
        let mut rv: u64 = 0;
        while piece_mask != 0 {
            // single out the least significant bit
            let source_sq_mask = piece_mask & piece_mask.wrapping_neg();
            
            rv |= self.get_single_piece_pseudo_attacks(
                piece, 
                colour, 
                source_sq_mask, 
                occupied
            );
            // remove the least significant bit for the next iteration
            piece_mask ^= source_sq_mask;
        } 
        rv
    }

    // (kingside, queenside)
    fn get_legal_castling_rights(&self, board: &Board, board_status: &BoardStatus) -> (bool, bool) {
        let (mut kingside, mut queenside) = board.castling_rights.can_castle(board.turn_colour);
        
        // For castling we have to check that:
        // 1. The intermediate squares between the king and rook are empty
        // 2. The intermediate squares the king travels are not under attack
        // For kingside castling, those squares are the same, but for queenside
        // castling they are not. i.e. for white queenside castling b1 c1 and d1
        // need to be vacated, but only c1 and d1 need to be unattacked, you can
        // still queenside castle if b1 is under attack as the king does not traverse
        // it
        if kingside {
            let king_travel_squares: u64 = match board.turn_colour {
                Colour::White => 0x60,
                Colour::Black => 0x6000000000000000,
            };
            kingside = king_travel_squares & board.bitboard.get_entire_mask() == 0 
                    && king_travel_squares & board_status.danger_squares == 0;
        }
        
        if queenside {
            let (in_between_squares, king_travel_squares): (u64, u64) = match board.turn_colour {
                Colour::White => (0xE, 0xC),
                Colour::Black => (0xE00000000000000, 0xC00000000000000),
            };
            queenside = in_between_squares & board.bitboard.get_entire_mask() == 0
                     && king_travel_squares & board_status.danger_squares == 0;
        }
        
        (kingside, queenside)
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
        // println!("{:?}", bitboard);
        // println!("line {}", line!());
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

        // needed to get squares friendly pieces can move to to put the enemy king in check
        let enemy_king_square = bitboard.get_colour_piece_mask(Piece::King, enemy_colour);

        assert!(enemy_king_square.is_power_of_two(), "colour = {:?} board = {:?} enemy king = {} {}", 
            friendly_colour, bitboard, bitmask_to_board(enemy_king_square), enemy_king_square);

        assert!(enemy_king_square.is_power_of_two());

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
        let mut check_squares: [u64; 6] = [0, 0, 0, 0, 0, 0];
        
        for piece in [Piece::Bishop, Piece::Rook, Piece::Queen] {
            let ray_tables = match piece {
                Piece::Bishop => vec![
                    (&NORTH_EAST, &SOUTH_WEST),
                    (&NORTH_WEST, &SOUTH_EAST),
                    (&SOUTH_EAST, &NORTH_WEST),
                    (&SOUTH_WEST, &NORTH_EAST),
                ],
                Piece::Rook => vec![
                    (&NORTH, &SOUTH),
                    (&SOUTH, &NORTH),
                    (&EAST, &WEST),
                    (&WEST, &EAST),
                ],
                Piece::Queen => vec![
                    (&NORTH, &SOUTH),
                    (&SOUTH, &NORTH),
                    (&EAST, &WEST),
                    (&WEST, &EAST),
                    (&NORTH_EAST, &SOUTH_WEST),
                    (&NORTH_WEST, &SOUTH_EAST),
                    (&SOUTH_EAST, &NORTH_WEST),
                    (&SOUTH_WEST, &NORTH_EAST),
                ],
                _ => panic!("This should not happen"),
            };
            
            // for that piece, also add check squares friendly pieces can move to
            // to put the enemy king in check
            // we put the colour as enemy colour as colour only affects the 
            // lookup of pawn moves
            // println!("line {}", line!());
            check_squares[piece as usize] = self.get_single_piece_pseudo_attacks(
                piece, 
                enemy_colour, 
                enemy_king_square, 
                occupied
            );
                
            // Start of checks for friendly pieces that are pinned to the friendly king

            // get bitmasks of all pieces of that type
            let mut enemy_piece_mask = bitboard.get_colour_piece_mask(piece, enemy_colour);
            
            // get the full attacks for the piece if it was on the friendly king square
            // println!("line {}", line!());
            let king_att_squares = self.get_single_piece_pseudo_attacks(
                piece, 
                enemy_colour, 
                friendly_king_square, 
                kingless_mask
            );
            
            // iterate through those pieces one by one
            while enemy_piece_mask != 0 {
                // single out the least significant bit
                let source_square = enemy_piece_mask & enemy_piece_mask.wrapping_neg();
                
                // get the full attacks for that piece on the source square
                // println!("line {}", line!());
                let enemy_piece_att_squares = self.get_single_piece_pseudo_attacks(
                    piece, 
                    enemy_colour, 
                    source_square, 
                    kingless_mask
                );
                
                // add to danger squares
                danger_squares |= enemy_piece_att_squares;
                
                let piece_idx = source_square.trailing_zeros() as usize;

                // loop through each ray direction separately
                for ray_direction in ray_tables.iter() {
                    // get the ray mask for that direction
                    let ray_mask = ray_direction.0[piece_idx];
                    let piece_ray_mask = ray_mask & enemy_piece_att_squares;
                    
                    // now get the mask for the opposite direction with the piece if placed on 
                    // the king square
                    let ray_mask = ray_direction.1[friendly_king_idx];
                    let king_ray_mask = ray_mask & king_att_squares;
                    
                    // check if king is in check via the king ray mask
                    // third element is just the block squares
                    if king_ray_mask & source_square == source_square {
                        king_attacking_pieces[num_checking_pieces as usize] = (piece, source_square, king_ray_mask ^ source_square);
                        num_checking_pieces += 1;
                    }
                    
                    // intersection of the two 
                    // If there's no pieces in between the sliding piece and the king, 
                    // i.e. the king is in check, this algorithm will incorrectly mark all the 
                    // intermediate squares between the two as pinned, as the two opposite rays
                    // overlap in all the intermediate squares. e.g. on the board
                    // "8/8/8/4k3/8/2r3P1/8/Q3R3 w - - 0 1" it incorrectly marks E2, E3 and E4 as
                    // "pinned". The intersection with the colour mask removes that.
                    let pinned = piece_ray_mask & king_ray_mask & bitboard.get_colour_mask(friendly_colour);
                    
                    pinned_pieces |= pinned;

                    // means we have found a pinned piece for that piece, can skip the remaining
                    // ray directions as a piece can't pin a piece in multiple ray directions
                    // Technically this could incorrectly break early due to the comment above,
                    // but as that can only happen when the piece can have the king in check it 
                    // can't pin a piece in one of the other ray directions anyway
                    if pinned != 0 {
                        // check there's only one pinned piece
                        assert!(pinned.is_power_of_two());
                        // The pinned piece can only move along the ray direction from the king to
                        // the pinning piece (including capturing), so just union the two masks and
                        // the source_square of the pinning piece as that hasn't been included in 
                        // its attack mask
                        pinned_legal_squares[num_pinned] = (pinned, piece_ray_mask | king_ray_mask | source_square);
                        num_pinned += 1;
                        break;
                    }
                }
                // remove least significant bit from piece mask for next iteration
                enemy_piece_mask ^= source_square;
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
                        assert!(pinned.is_power_of_two());

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
            check_squares[piece as usize] = self.get_single_piece_pseudo_attacks(
                piece, 
                enemy_colour, 
                enemy_king_square, 
                occupied
            );

            while source_squares != 0 {
                let source_square = source_squares & source_squares.wrapping_neg();
                let attacking_squares = self.get_single_piece_pseudo_attacks(
                    piece, 
                    enemy_colour, 
                    source_square, 
                    occupied
                );
                
                danger_squares |= attacking_squares;
                
                // if piece is putting the king in check
                if attacking_squares & friendly_king_square == friendly_king_square {
                    assert!(piece != Piece::King);
                    // As a non sliding piece can't be blocked it can only be captured
                    king_attacking_pieces[num_checking_pieces as usize] = (piece, source_square, source_square);
                    num_checking_pieces += 1;
                }
                source_squares ^= source_square;
            }
        }

        BoardStatus {
            danger_squares,
            pinned_pieces,
            pinned_pseudo_legal_squares: pinned_legal_squares,
            king_attacking_pieces,
            num_checking_pieces,
            // discovered_check_pseudo_legal_squares,
            // num_discovered_checks,
            check_squares,
        }
    }
}

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
                //assert!($expected.contains_key(&pinned_piece));
                assert!($expected.iter().any(|(pinned, _)| *pinned == pinned_piece));
                assert!($actual.iter().any(|(pinned, _)| *pinned == pinned_piece));

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
        
        for (i, (expected, actual)) in expected_check_squares.iter()
                                                             .zip(board_status.check_squares.iter())
                                                             .enumerate() {
            let description = format!(
                "Check squares for {:?} for r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1", 
                Piece::try_from(i).unwrap()
            );
            test_bitboard_eq!(
                description,
                *expected,
                *actual,
            );
        }

        
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

        for (i, (expected, actual)) in expected_check_squares.iter()
                                                             .zip(board_status.check_squares.iter())
                                                             .enumerate() {
            let description = format!(
                "Check squares for {:?} for rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR", 
                Piece::try_from(i).unwrap()
            );
            test_bitboard_eq!(
                description,
                *expected,
                *actual,                
            );
        }
        
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
        
        for (i, (expected, actual)) in expected_check_squares.iter()
                                                             .zip(board_status.check_squares.iter())
                                                             .enumerate() {
            let description = format!(
                "Check squares for {:?} for 8/1K6/8/4k3/8/2r3P1/8/Q3R3",
                Piece::try_from(i).unwrap()
            );
            test_bitboard_eq!(
                description,
                *expected,
                *actual,                
            );
        }
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
        assert!(idx.is_some(), "Knight on A6 expected as king-attacking piece but not found");
        test_bitboard_eq!(
            "Checking for blocking and/or capturing squares to get out of check for the knight on A6 for 1kb5/pp6/N7/8/8/6Q1/8/1K6",
            fen_to_hex("8/8/N7/8/8/8/8/8 w - - 0 1"),
            board_status.king_attacking_pieces[idx.unwrap()].1,
        );
        
        let idx = board_status.king_attacking_pieces.iter().position(|&(piece, source_sq, _)| {
            piece == Piece::Queen && source_sq == 1 << Square::G3 as u32
        });
        assert!(idx.is_some(), "Queen on G3 expected as king-attacking piece but not found");
        test_bitboard_eq!(
            "Checking for blocking to get out of check for the queen on G3 for 1kb5/pp6/N7/8/8/6Q1/8/1K6",
            fen_to_hex("8/2q5/3q4/4q3/5q2/8/8/8 w - - 0 1"),
            board_status.king_attacking_pieces[idx.unwrap()].2,
        );
        
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
        assert!(board_status.discovered_check_pseudo_legal_squares.contains(&expected),
                "8/8/1k6/2N5/8/1B6/5Q2/1R4K1 shows the bishop on B3 as a potential discovered check"
        );
        
        let expected = (
            Piece::Knight,
            1 << Square::C5 as u32,
            fen_to_hex("PPPPPPPP/PPPPPPPP/P1PPPPPP/PP1PPPPP/PPP1PPPP/PPPP1PPP/PPPPP1PP/PPPPPPPP"),
        );
        assert!(board_status.discovered_check_pseudo_legal_squares.contains(&expected),
                "8/8/1k6/2N5/8/1B6/5Q2/1R4K1 shows the knight on C5 as a potential discovered check"
        );
        */
    }
}
