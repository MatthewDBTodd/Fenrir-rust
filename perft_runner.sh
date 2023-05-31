#!/usr/bin/bash

# ./target/release/perft_correctness "$@"
./target/release/perft_correctness -d "$1" -f "$2" "${@:3}"
