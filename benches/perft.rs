use criterion::{black_box, criterion_group, criterion_main, Criterion, BatchSize};
use chess::attack_table::AttackTable;  
use chess::board::Board;        
use chess::chess_move::Move;         

const TEST_CASES: &[(&str, u32, usize)] = &[
    // (fen, depth-to-search, expected value)
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 1, 20),
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 2, 400),
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 3, 8902),
    ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ", 6, 119_060_324),
    ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 5, 193_690_690),
];

fn perft(board: &mut Board, attack_table: &AttackTable, depth: u32) -> usize {
    assert!(depth > 0);
    let mut move_list = [Move::default(); 256];

    let legal_moves = attack_table.generate_legal_moves(board, &mut move_list);

    if depth == 1 {
        return legal_moves;
    }

    let mut total_nodes: usize = 0;
    for i in 0..legal_moves {
        board.make_move(move_list[i]);
        let nodes = _perft(board, attack_table, depth-1);
        total_nodes += nodes;
        println!("{} {}", move_string(&move_list[i]), nodes);
    }
    total_nodes
}

fn _perft(board: &mut Board, attack_table: &AttackTable, depth: u32) -> usize {

    let mut move_list = [Move::default(); 256];

    let legal_moves = attack_table.generate_legal_moves(board, &mut move_list);

    if depth == 1 {
        return legal_moves;
    }

    let mut nodes: usize = 0;
    for i in 0..legal_moves {
        board.make_move(move_list[i]); 
        nodes += _perft(board, attack_table, depth-1);
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

fn move_string(chess_move: &Move) -> String {
    let squares: [&str; 64] = [
        "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
        "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
        "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
        "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
        "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
        "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
        "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
        "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8",
    ];
    format!("{}{}", squares[chess_move.source_sq as usize], squares[chess_move.dest_sq as usize])
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
