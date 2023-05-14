use criterion::{black_box, criterion_group, criterion_main, Criterion, BatchSize};
use chess::attack_table::AttackTable;  
use chess::board::Board;        
use chess::Move;         

const TEST_CASES: &[(&str, u32, usize)] = &[
    // (fen, depth-to-search, expected value)
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 6, 0 /*119_060_324*/),
    ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 0 /*193_690_690*/),
];

fn perft(board: &mut Board, attack_table: &AttackTable, depth: u32) -> usize {
    let move_list = [Move::default(); 256];

    let legal_moves = attack_table.generate_legal_moves(board, &move_list);

    if depth == 1 {
        return legal_moves;
    }

    let mut nodes: usize = 0;
    for i in 0..legal_moves {
        board.make_move(move_list[i]); 
        nodes += perft(board, attack_table, depth-1);
        board.undo_move();
    }
    nodes
}

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("generate_legal_moves");

    // Initialize AttackTable outside the loop
    print!("Initialising attack table... ");
    let attack_table = AttackTable::init();  
    println!("done");

    for &(fen, depth, expected) in TEST_CASES {
        group.bench_function(fen, |b| {
            b.iter_batched_ref(
                || Board::new(Some(fen)).unwrap(),
                |board| {
                    let actual_nodes = perft(board, &attack_table, depth);
                    assert_eq!(actual_nodes, expected);
                },
                BatchSize::SmallInput,
            );
        });
    }

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
