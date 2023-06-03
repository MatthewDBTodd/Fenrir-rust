use std::time::Instant;

use criterion::{criterion_group, criterion_main, Criterion, BatchSize};

use chess::attack_table::AttackTable;
use chess::board::Board;
use chess::shared_perft::*;


const TEST_CASES: &[(&str, u32, usize)] = &[
    // (fen, depth-to-search, expected value)
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 6, 119_060_324),
    // ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 7, 3_195_901_860),
    ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193_690_690),
    // ("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", 8, 3_009_794_393),
    // ("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 6, 706_045_033),
    // ("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 5, 89_941_194),
    // ("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 5, 164_075_551),
];
fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("generate_legal_moves");
    group.sample_size(10);

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
                    let start = Instant::now();
                    let actual_nodes = perft(board, &attack_table, depth, false);
                    let duration = start.elapsed().as_secs_f64();
                    assert_eq!(expected, actual_nodes);
                    let nodes_per_second = actual_nodes as f64 / duration;
                    println!("{bench_name} finished in {duration} at {nodes_per_second}");
                },
                BatchSize::SmallInput,
            );
        });
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
