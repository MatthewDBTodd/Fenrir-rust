use criterion::{criterion_group, criterion_main, Criterion, BatchSize};

use chess::attack_table::AttackTable;
use chess::board::Board;
use chess::shared_perft::*;


const TEST_CASES: &[(&str, u32, usize)] = &[
    // (fen, depth-to-search, expected value)
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 1, 20),
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 2, 400),
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 3, 8902),
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 6, 119_060_324),
    ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193_690_690),
];
fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("generate_legal_moves");

    // Initialize AttackTable outside the loop
    print!("Initialising attack table... ");
    let attack_table = AttackTable::init();  
    println!("done");

    for &(fen, depth, expected) in TEST_CASES {
        // so identical fen's don't cause duplicate IDs which criterion library does
        // not allow
        let bench_name = format!("{} depth {}", fen, depth);
        group.bench_function(&bench_name, |b| {
            b.iter_batched_ref(
                || Board::new(Some(fen)).unwrap(),
                |board| {
                    let actual_nodes = perft(board, &attack_table, depth);
                    assert_eq!(expected, actual_nodes);
                },
                BatchSize::SmallInput,
            );
        });
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
