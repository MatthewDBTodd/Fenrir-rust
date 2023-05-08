use crate::{Piece, Square};
use rand::Rng;
use rand::rngs::ThreadRng;
use crate::masks::*;
use crate::test_helpers::hex_to_board;

pub struct Magic {
    attacks: Vec<u64>,
    magic: u64,
    shift: u8,
    mask: u64
}

fn rand_sparse_u64(rng: &mut ThreadRng) -> u64 {
    rng.gen::<u64>() & rng.gen::<u64>() & rng.gen::<u64>()
}

impl Magic {
    pub fn init(piece: Piece) -> Vec<Magic> {
        let mut rv = Vec::with_capacity(64);
        let mut square_mask: u64 = 1;
        for _ in 0..64 {
            rv.push(Magic::init_for_square(square_mask, piece));
            square_mask <<= 1;
        }
        rv
    }
    
    pub fn get_attacks(&self, occupied_unmasked: u64) -> u64 {
        let occupied_masked = occupied_unmasked & self.mask;
        let idx = get_index_from_blockers(occupied_masked, self.magic, self.shift);
        self.attacks[idx]
    }

    fn init_for_square(square_mask: u64, piece: Piece) -> Self {
        debug_assert!(square_mask.is_power_of_two());
        // given a mask (with only a single bit set), to find its corresponding
        // index, simply use trailing zeros. e.g. 0b1000.trailing_zeros() returns 3.
        let square_index = square_mask.trailing_zeros() as usize;
        
        // raw_block_mask for that square with the edges zeroed out, as they don't
        // affect the outcome
        let raw_block_mask = match piece {
            Piece::Rook => ROOK_MASKS[square_index],
            Piece::Bishop => BISHOP_MASKS[square_index],
            _ => panic!("Non sliding piece called in gen_attacks()"),
        };
        
        // generate attacks for all blocker permutations
        let permutations = gen_attacks(square_mask, raw_block_mask, piece);
        debug_assert!(permutations.attacks.len() == permutations.blockers.len());

        let mut attacks = vec![0; permutations.blockers.len()];
        let mut magic: u64;
        let mut rng = rand::thread_rng();
        let shift = (64 - raw_block_mask.count_ones()) as u8;
        loop {
            // generate random candidate magic number, apparently fewer bits is
            // better
            magic = rand_sparse_u64(&mut rng);
            
            let mut failed = false;
            for i in 0..permutations.blockers.len() {
                let index = get_index_from_blockers(
                    permutations.blockers[i], magic, shift);
                debug_assert!(index < attacks.len());
                
                // unused index, can carry on
                if attacks[index] == 0 {
                    attacks[index] = permutations.attacks[i];
                } 
                // index already taken, fail and move on to next attempt
                else {
                    attacks = vec![0; permutations.blockers.len()];
                    failed = true;
                    break; 
                }
            }    
            
            if !failed {
                break;
            }
        }

        Magic {
            attacks,
            magic,
            shift,
            mask: raw_block_mask,
        }
    }
    
}

fn get_index_from_blockers(
    blocker_mask: u64, magic: u64, shift_amount: u8 
) -> usize {
    ((blocker_mask.wrapping_mul(magic)) >> shift_amount) as usize
}

struct SquareAttackPermutations {
    blockers: Vec<u64>,
    attacks: Vec<u64>,
}

// Given a square and a piece, calculate attacks for all permutations of blockers
fn gen_attacks(square_mask: u64, raw_block_mask: u64, piece: Piece) -> SquareAttackPermutations {
    // Need to know how many bits are in the block mask, as we use it to size the 
    // vector of attacks. As we need to iterate through all subsets of blockers,
    // the size of the vector is 2^num_bits
    let num_bits = raw_block_mask.count_ones();
    let mut blockers = Vec::with_capacity(1 << num_bits);
    let mut attacks = Vec::with_capacity(1 << num_bits);
    
    // The shift amount for each ray direction
    let direction_shifts: [Box<dyn Fn(u64) -> u64>; 4] = if piece == Piece::Rook {
        [
            Box::new(|n| {
                north!(n, 1) 
            }),
            Box::new(|n| {
                south!(n, 1)
            }),
            Box::new(|n| {
                east!(n, 1)
            }),
            Box::new(|n| {
                west!(n, 1)
            }),
            //8, -8, 1, -1
        ]
    } else {
        [
            Box::new(|n| {
                north_east!(n)
            }),
            Box::new(|n| {
                north_west!(n)
            }),
            Box::new(|n| {
                south_east!(n)
            }),
            Box::new(|n| {
                south_west!(n)
            }),
        // [7, 9, -9, -7]
        ]
    };
    
    // Traverse all subsets of the blocker mask
    // See https://www.chessprogramming.org/Traversing_Subsets_of_a_Set
    let mut blocker_permutation: u64 = 0;
    loop {
        blockers.push(blocker_permutation);
        attacks.push(gen_attack_squares_from_permutation(
            square_mask, 
            &direction_shifts, 
            blocker_permutation
        ));
        // next permutation
        blocker_permutation = 
            (blocker_permutation.wrapping_sub(raw_block_mask)) & raw_block_mask;
        if blocker_permutation == 0 {
            break;
        }
    }
    SquareAttackPermutations {
        blockers,
        attacks,
    }
}

// given a square, a piece, and a blocker-permutation-mask, calculate its attack
// squares.
fn gen_attack_squares_from_permutation(
    square_mask: u64, 
    directions: &[Box<dyn Fn(u64) -> u64>; 4], 
    blocker_permutation: u64) -> u64 {

    // println!("-----------------------");
    // println!("blockers = {}", hex_to_board(blocker_permutation));
    let mut result: u64 = 0;
    for dir_shift in directions {
        // tmp_mask is initially square mask
        let mut tmp_mask = square_mask;
        // if moving north/south it eventually shifts off the end of the u64 so 
        // will == 0. If east/west the opposite edges are zeroed in the macros
        // will also == 0.
        while tmp_mask != 0 {
            tmp_mask = dir_shift(tmp_mask);
            // shift tmp_mask to the next square in the ray direction
            // tmp_mask = if *dir_shift > 0 {
            //     tmp_mask << dir_shift
            // } else {
            //     tmp_mask >> dir_shift.abs()
            // };
            // Add the square to the result mask
            result |= tmp_mask;
            
            // In this case we have hit a blocker, nothing behind the blocker
            // matters so we break out of this loop. We are done in this direction.
            if tmp_mask & blocker_permutation > 0 {
                break; 
            }
        }
    }
    // println!("attacks = {}", hex_to_board(result));
    result
}

// Rook and Bishop attack masks for each square, with the edge squares zeroed out.
// As a blocker on the edge of the board makes no difference to the outcome
#[rustfmt::skip]
const ROOK_MASKS: [u64; 64] = [
	0x000101010101017E, 0x000202020202027C, 0x000404040404047A, 0x0008080808080876,
	0x001010101010106E, 0x002020202020205E, 0x004040404040403E, 0x008080808080807E,
	0x0001010101017E00, 0x0002020202027C00, 0x0004040404047A00, 0x0008080808087600,
	0x0010101010106E00, 0x0020202020205E00, 0x0040404040403E00, 0x0080808080807E00,
	0x00010101017E0100, 0x00020202027C0200, 0x00040404047A0400, 0x0008080808760800,
	0x00101010106E1000, 0x00202020205E2000, 0x00404040403E4000, 0x00808080807E8000,
	0x000101017E010100, 0x000202027C020200, 0x000404047A040400, 0x0008080876080800,
	0x001010106E101000, 0x002020205E202000, 0x004040403E404000, 0x008080807E808000,
	0x0001017E01010100, 0x0002027C02020200, 0x0004047A04040400, 0x0008087608080800,
	0x0010106E10101000, 0x0020205E20202000, 0x0040403E40404000, 0x0080807E80808000,
	0x00017E0101010100, 0x00027C0202020200, 0x00047A0404040400, 0x0008760808080800,
	0x00106E1010101000, 0x00205E2020202000, 0x00403E4040404000, 0x00807E8080808000,
	0x007E010101010100, 0x007C020202020200, 0x007A040404040400, 0x0076080808080800,
	0x006E101010101000, 0x005E202020202000, 0x003E404040404000, 0x007E808080808000,
	0x7E01010101010100, 0x7C02020202020200, 0x7A04040404040400, 0x7608080808080800,
	0x6E10101010101000, 0x5E20202020202000, 0x3E40404040404000, 0x7E80808080808000,
];

#[rustfmt::skip]
const BISHOP_MASKS: [u64; 64] = [
	0x0040201008040200, 0x0000402010080400, 0x0000004020100A00, 0x0000000040221400,
	0x0000000002442800, 0x0000000204085000, 0x0000020408102000, 0x0002040810204000,
	0x0020100804020000, 0x0040201008040000, 0x00004020100A0000, 0x0000004022140000,
	0x0000000244280000, 0x0000020408500000, 0x0002040810200000, 0x0004081020400000,
	0x0010080402000200, 0x0020100804000400, 0x004020100A000A00, 0x0000402214001400,
	0x0000024428002800, 0x0002040850005000, 0x0004081020002000, 0x0008102040004000,
	0x0008040200020400, 0x0010080400040800, 0x0020100A000A1000, 0x0040221400142200,
	0x0002442800284400, 0x0004085000500800, 0x0008102000201000, 0x0010204000402000,
	0x0004020002040800, 0x0008040004081000, 0x00100A000A102000, 0x0022140014224000,
	0x0044280028440200, 0x0008500050080400, 0x0010200020100800, 0x0020400040201000,
	0x0002000204081000, 0x0004000408102000, 0x000A000A10204000, 0x0014001422400000,
	0x0028002844020000, 0x0050005008040200, 0x0020002010080400, 0x0040004020100800,
	0x0000020408102000, 0x0000040810204000, 0x00000A1020400000, 0x0000142240000000,
	0x0000284402000000, 0x0000500804020000, 0x0000201008040200, 0x0000402010080400,
	0x0002040810204000, 0x0004081020400000, 0x000A102040000000, 0x0014224000000000,
	0x0028440200000000, 0x0050080402000000, 0x0020100804020000, 0x0040201008040200,
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::*;

    #[test]
    fn test_rook_magic() {
        let square_mask = 1u64 << (Square::B3 as u32);
        let magic = Magic::init_for_square(square_mask, Piece::Rook);
        test_bitboard_eq!(
            "Test rook magic bitboard for B3 with fen r5k1/pbpq1ppp/1pnp1n2/4r3/2P1p3/BRPPPNP1/P1Q2PBP/4R1K1 w q - 0 1 ",
            fen_to_hex("8/8/1R6/1R6/1R6/R1R5/1R6/1R6 w - - 0 1"),
            magic.get_attacks(fen_to_hex("r5k1/pbpq1ppp/1pnp1n2/4r3/2P1p3/BRPPPNP1/P1Q2PBP/4R1K1 w q - 0 1")),
        );

        let square_mask = 1u64 << (Square::E1 as u32);
        let magic = Magic::init_for_square(square_mask, Piece::Rook);
        test_bitboard_eq!(
            "Test rook magic bitboard for E1 with fen r5k1/pbpq1ppp/1pnp1n2/4r3/2P1p3/BRPPPNP1/P1Q2PBP/4R1K1 w q - 0 1 ",
            fen_to_hex("8/8/8/8/8/4R3/4R3/RRRR1RR1 w - - 0 1"),
            magic.get_attacks(fen_to_hex("r5k1/pbpq1ppp/1pnp1n2/4r3/2P1p3/BRPPPNP1/P1Q2PBP/4R1K1 w q - 0 1")),
        );

        let square_mask = 1u64 << (Square::A8 as u32);
        let magic = Magic::init_for_square(square_mask, Piece::Rook);
        test_bitboard_eq!(
            "Test rook magic bitboard for A1 with fen r5k1/pbpq1ppp/1pnp1n2/4r3/2P1p3/BRPPPNP1/P1Q2PBP/4R1K1 w q - 0 1 ",
            fen_to_hex("1rrrrrr1/r7/8/8/8/8/8/8 w - - 0 1"),
            magic.get_attacks(fen_to_hex("r5k1/pbpq1ppp/1pnp1n2/4r3/2P1p3/BRPPPNP1/P1Q2PBP/4R1K1 w q - 0 1")),
        );

        let square_mask = 1u64 << (Square::E5 as u32);
        let magic = Magic::init_for_square(square_mask, Piece::Rook);
        test_bitboard_eq!(
            "Test rook magic bitboard for E5 with fen r5k1/pbpq1ppp/1pnp1n2/4r3/2P1p3/BRPPPNP1/P1Q2PBP/4R1K1 w q - 0 1 ",
            fen_to_hex("4r3/4r3/4r3/rrrr1rrr/4r3/8/8/8 w - - 0 1"),
            magic.get_attacks(fen_to_hex("r5k1/pbpq1ppp/1pnp1n2/4r3/2P1p3/BRPPPNP1/P1Q2PBP/4R1K1 w q - 0 1")),
        )
    }
    
    #[test]
    fn test_bishop_magic() {
        let square_mask = 1u64 << (Square::B2 as u32);
        let magic = Magic::init_for_square(square_mask, Piece::Bishop);
        test_bitboard_eq!(
            "Test bishop magic bitboard for B2 with fen r1bq1rk1/ppp2ppp/1bn2n2/3p4/1P2p3/P3P1P1/1BPPNPBP/RN1Q1RK1 w Qq - 0 1",
            fen_to_hex("8/8/5B2/4B3/3B4/B1B5/8/B1B5 w - - 0 1"),
            magic.get_attacks(fen_to_hex("r1bq1rk1/ppp2ppp/1bn2n2/3p4/1P2p3/P3P1P1/1BPPNPBP/RN1Q1RK1 w Qq - 0 1"))
        );
            
        let square_mask = 1u64 << (Square::G2 as u32);
        let magic = Magic::init_for_square(square_mask, Piece::Bishop);
        test_bitboard_eq!(
            "Test bishop magic bitboard for G2 with fen r1bq1rk1/ppp2ppp/1bn2n2/3p4/1P2p3/P3P1P1/1BPPNPBP/RN1Q1RK1 w Qq - 0 1",
            fen_to_hex("8/8/8/8/4B3/5B1B/8/5B1B w - - 0 1"),
            magic.get_attacks(fen_to_hex("r1bq1rk1/ppp2ppp/1bn2n2/3p4/1P2p3/P3P1P1/1BPPNPBP/RN1Q1RK1 w Qq - 0 1")),
        );
        
        let square_mask = 1u64 << (Square::C8 as u32);
        let magic = Magic::init_for_square(square_mask, Piece::Bishop);
        test_bitboard_eq!(
            "Test bishop magic bitboard for C8 with fen r1bq1rk1/ppp2ppp/1bn2n2/3p4/1P2p3/P3P1P1/1BPPNPBP/RN1Q1RK1 w Qq - 0 1",
            fen_to_hex("8/1b1b4/4b3/5b2/6b1/7b/8/8 w - - 0 1"),
            magic.get_attacks(fen_to_hex("r1bq1rk1/ppp2ppp/1bn2n2/3p4/1P2p3/P3P1P1/1BPPNPBP/RN1Q1RK1 w Qq - 0 1")),
        );

        let square_mask = 1u64 << (Square::B6 as u32);
        let magic = Magic::init_for_square(square_mask, Piece::Bishop);
        test_bitboard_eq!(
            "Test bishop magic bitboard for C8 with fen r1bq1rk1/ppp2ppp/1bn2n2/3p4/1P2p3/P3P1P1/1BPPNPBP/RN1Q1RK1 w Qq - 0 1",
            fen_to_hex("8/b1b5/8/b1b5/3b4/4b3/8/8 w - - 0 1"),
            magic.get_attacks(fen_to_hex("r1bq1rk1/ppp2ppp/1bn2n2/3p4/1P2p3/P3P1P1/1BPPNPBP/RN1Q1RK1 w Qq - 0 1")),
        );
    }
}