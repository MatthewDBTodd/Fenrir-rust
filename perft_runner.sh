#!/usr/bin/bash

# ./target/release/perft_correctness "$@"
# ./target/release/perft_correctness -d "$1" -f "$2" "${@:3}" | tee perft_output.txt
./target/release/perft_correctness -d "$1" -f "$2" "${@:3}"
