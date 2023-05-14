use std::collections::HashMap;

use crate::board::Board;
use crate::pawn::PawnAttackTable;
use crate::sliding_piece::Magic;
use crate::{king, knight, Piece, Colour, Square, Move};
use crate::bitboard::BitBoard;

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
    pinned_pseudo_legal_squares: HashMap<u64, u64>,
    // a list of pairs where the first element is a bitmask of an enemy piece who
    // currently has the king in check.
    // The second value is a mask of all the squares that a friendly piece could 
    // move to to block or capture the checking piece
    king_attacking_pieces: Vec<(u64, u64)>,
    // a list of pairs where the first element is a bitmask of a friendly piece
    // who can potentially move and cause a discovered check
    // The second value is a mask of all the pseudo-legal squares that piece can
    // move to to initiate the discovered check. As it's pseudo-legal squares, in
    // reality the piece might not legally be able to move out of the way. This
    // must be checked later
    discovered_check_pseudo_legal_squares:  Vec<(u64, u64)>,
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
    
    pub fn generate_legal_moves(&self, board: &Board, moves: &[Move; 256]) -> usize {

        0usize 
        // let board_status = self.get_board_status(&bitboard, colour_to_move);
        // 
        // let in_check = !board_status.king_attacking_pieces.is_empty();
        // 
        // if in_check {
        //     // if in double check, just generate king moves

        //     // otherwise    
        // }
    }
    
    // For a given piece type and a bitmask of a single source square, return
    // a bitmask of all the squares that piece attacks
    fn get_single_piece_attacks(
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
            
            rv |= self.get_single_piece_attacks(
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
        let enemy_colour = match friendly_colour {
            Colour::White => Colour::Black,
            Colour::Black => Colour::White,
        };

        // needed to get squares friendly pieces can move to to put the enemy king in check
        let enemy_king_square = bitboard.get_colour_piece_mask(Piece::King, enemy_colour);
        let enemy_king_idx = enemy_king_square.trailing_zeros() as usize;

        // will mark danger squares while calculating pinned pieces 
        let mut danger_squares: u64 = 0;
        let mut pinned_pieces: u64 = 0;
        // There can be maximum of 8 pieces pinned to the board at any one time
        // i.e. one for each direction the king can move
        let mut pinned_legal_squares: HashMap<u64, u64> = HashMap::with_capacity(8);
        // A king can only be in check from at most 2 pieces
        let mut king_attacking_pieces = Vec::with_capacity(2);
        let mut discovered_check_pseudo_legal_squares: Vec<(u64, u64)> = Vec::new();
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
            check_squares[piece as usize] = self.get_single_piece_attacks(
                piece, 
                enemy_colour, 
                enemy_king_square, 
                occupied
            );
                
            // Start of checks for friendly pieces that are pinned to the friendly king

            // get bitmasks of all pieces of that type
            let mut enemy_piece_mask = bitboard.get_colour_piece_mask(piece, enemy_colour);
            
            // get the full attacks for the piece if it was on the friendly king square
            let king_att_squares = self.get_single_piece_attacks(
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
                let enemy_piece_att_squares = self.get_single_piece_attacks(
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
                    if king_ray_mask & source_square == source_square {
                        king_attacking_pieces.push((source_square, king_ray_mask));
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
                        debug_assert!(pinned.is_power_of_two());
                        // The pinned piece can only move along the ray direction from the king to
                        // the pinning piece (including capturing), so just union the two masks and
                        // the source_square of the pinning piece as that hasn't been included in 
                        // its attack mask
                        pinned_legal_squares.insert(pinned, piece_ray_mask | king_ray_mask | source_square);
                        break;
                    }
                }
                // remove least significant bit from piece mask for next iteration
                enemy_piece_mask ^= source_square;
            }
            
            // start of checks for friendly pieces pinned to the enemy king 
            // i.e. those that can cause a discovered check
            
            // get bitmasks of all pieces of that type
            let mut friendly_piece_mask = bitboard.get_colour_piece_mask(piece, friendly_colour);
            
            // get the full attacks for the piece if it was on the enemy king square
            let king_att_squares = self.get_single_piece_attacks(
                piece, 
                friendly_colour, 
                enemy_king_square, 
                occupied 
            );
            
            // iterate through those pieces one by one
            while friendly_piece_mask != 0 {
                let source_square = friendly_piece_mask & friendly_piece_mask.wrapping_neg();
                
                // get the full attacks for that piece on the source square
                let friendly_piece_att_squares = self.get_single_piece_attacks(
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
                        discovered_check_pseudo_legal_squares.push((
                            pinned,
                            // the piece can move to any square that's not in the ray blocking the
                            // enemy king to initiate discovered check, i.e. the rest of the board
                            !(piece_ray_mask | king_ray_mask | source_square | enemy_king_square)
                        ));
                    }
                }
                // remove least significant bit from piece mask for next iteration
                friendly_piece_mask ^= source_square;
            }
        }
        
        // We now have the completed mask for pinned pieces, and have partially done the danger
        // squares with the pinned pieces. Now need to complete the danger squares with king, pawn,
        // and knight attacks
        for piece in [Piece::Pawn, Piece::King, Piece::Knight] {
            let mut source_squares = bitboard.get_colour_piece_mask(piece, enemy_colour);
            
            // for that piece, also add check squares friendly pieces can move to
            // to put the enemy king in check
            check_squares[piece as usize] = self.get_single_piece_attacks(
                piece, 
                enemy_colour, 
                enemy_king_square, 
                occupied
            );

            while source_squares != 0 {
                let source_square = source_squares & source_squares.wrapping_neg();
                let attacking_squares = self.get_single_piece_attacks(
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
                    king_attacking_pieces.push((source_square, source_square));
                }
                source_squares ^= source_square;
            }
        }

        BoardStatus {
            danger_squares,
            pinned_pieces,
            pinned_pseudo_legal_squares: pinned_legal_squares,
            king_attacking_pieces,
            discovered_check_pseudo_legal_squares,
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
                assert!($expected.contains_key(&pinned_piece));
                assert!($actual.contains_key(&pinned_piece));

                test_bitboard_eq!(
                    $test_description,
                    *$expected.get(&pinned_piece).unwrap(),
                    *$actual.get(&pinned_piece).unwrap(),
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
        
        let mut expected_pinned_legal_squares: HashMap<u64, u64> = HashMap::with_capacity(3);
        expected_pinned_legal_squares.insert(1 << Square::C6 as u32, fen_to_hex("8/3n4/2n5/1n6/8/8/8/8 w - - 0 1"));
        expected_pinned_legal_squares.insert(1 << Square::F7 as u32, fen_to_hex("8/5p2/6p1/7p/8/8/8/8 w - - 0 1"));
        expected_pinned_legal_squares.insert(1 << Square::E5 as u32, fen_to_hex("8/4p3/4p3/4p3/4p3/4p3/4p3/4p3 w - - 0 1"));

        test_all_pinned_piece_legal_moves!(
            "Checking the legal squares for each pinned piece for r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1 w - - 0 1",
            board_status.pinned_pieces,
            expected_pinned_legal_squares,
            board_status.pinned_pseudo_legal_squares,
        );
        
        assert_eq!(0, board_status.discovered_check_pseudo_legal_squares.len(), 
                  "Check no discovered checks for r1bqkb1r/ppp2ppp/2np1n2/1B2p2Q/8/2N2N2/PPPP1PPP/R1B1R1K1"
        );
        
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

        let mut expected_pinned_legal_squares: HashMap<u64, u64> = HashMap::with_capacity(2);
        expected_pinned_legal_squares.insert(1 << Square::E2 as u32, fen_to_hex("4P3/4P3/4P3/4P3/4P3/4P3/4P3/8 w - - 0 1"));
        expected_pinned_legal_squares.insert(1 << Square::G3 as u32, fen_to_hex("8/8/8/8/7P/6P1/5P2/8 w - - 0 1"));
        
        test_all_pinned_piece_legal_moves!(
            "Checking the legal squares for each pinned piece for rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR",
            board_status.pinned_pieces,
            expected_pinned_legal_squares,
            board_status.pinned_pseudo_legal_squares,
        );
        
        assert_eq!(0, board_status.discovered_check_pseudo_legal_squares.len(),
                   "Check no discovered checks for rnb1r1k1/pppp1ppp/5n2/8/1b5q/2N2PP1/PPPPP2P/R1BQKBNR"
        );

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
            
        let mut expected_pinned_legal_squares: HashMap<u64, u64> = HashMap::with_capacity(1);
        expected_pinned_legal_squares.insert(1 << Square::C3 as u32, fen_to_hex("8/8/8/8/3r4/2r5/1r6/r7 w - - 0 1"));

        test_all_pinned_piece_legal_moves!(
            "Checking the legal squares for each pinned piece for 8/1K6/8/4k3/8/2r3P1/8/Q3R3",
            board_status.pinned_pieces,
            expected_pinned_legal_squares,
            board_status.pinned_pseudo_legal_squares,
        );

        assert_eq!(0, board_status.discovered_check_pseudo_legal_squares.len(),
                   "Check no discovered checks for 8/1K6/8/4k3/8/2r3P1/8/Q3R3"
        );
        
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

        let mut expected_pinned_legal_squares: HashMap<u64, u64> = HashMap::with_capacity(1);
        expected_pinned_legal_squares.insert(1 << Square::D6 as u32, fen_to_hex("8/8/3r4/2r5/1r6/r7/8/8 w - - 0 1"));

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
        
        let idx = board_status.king_attacking_pieces.iter().position(|&(a, _)| a == 1 << Square::A6 as u32);
        assert!(idx.is_some(), "Knight on A6 expected as king-attacking piece but not found");
        test_bitboard_eq!(
            "Checking for blocking and/or capturing squares to get out of check for the knight on A6 for 1kb5/pp6/N7/8/8/6Q1/8/1K6",
            fen_to_hex("8/8/N7/8/8/8/8/8 w - - 0 1"),
            board_status.king_attacking_pieces[idx.unwrap()].1,
        );
        
        let idx = board_status.king_attacking_pieces.iter().position(|&(a, _)| a == 1 << Square::G3 as u32);
        assert!(idx.is_some(), "Queen on G3 expected as king-attacking piece but not found");
        test_bitboard_eq!(
            "Checking for blocking and/or capturing squares to get out of check for the queen on G3 for 1kb5/pp6/N7/8/8/6Q1/8/1K6",
            fen_to_hex("8/2Q5/3Q4/4Q3/5Q2/6Q1/8/8 w - - 0 1"),
            board_status.king_attacking_pieces[idx.unwrap()].1,
        );
        
        // -----------------------------------------------------------------------------------------
        // 
        let bitboard = BitBoard::try_from(
            "8/8/1k6/2N5/8/1B6/5Q2/1R4K1",
        ).unwrap();

        let board_status = attack_table.get_board_status(&bitboard, Colour::White);

        let expected = (
            1 << Square::B3 as u32,
            fen_to_hex("PPPPPPPP/PPPPPPPP/P1PPPPPP/P1PPPPPP/P1PPPPPP/P1PPPPPP/P1PPPPPP/P1PPPPPP"),
        );
        assert!(board_status.discovered_check_pseudo_legal_squares.contains(&expected),
                "8/8/1k6/2N5/8/1B6/5Q2/1R4K1 shows the bishop on B3 as a potential discovered check"
        );
        
        let expected = (
            1 << Square::C5 as u32,
            fen_to_hex("PPPPPPPP/PPPPPPPP/P1PPPPPP/PP1PPPPP/PPP1PPPP/PPPP1PPP/PPPPP1PP/PPPPPPPP"),
        );
        assert!(board_status.discovered_check_pseudo_legal_squares.contains(&expected),
                "8/8/1k6/2N5/8/1B6/5Q2/1R4K1 shows the knight on C5 as a potential discovered check"
        );
    }
}
