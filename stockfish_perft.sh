#!/bin/bash

start=$(date +%s.%N)

output=$(echo -e "position startpos\ngo perft 7\nquit" | stockfish | tee /dev/stderr)

end=$(date +%s.%N)

runtime=$(python -c "print(${end} - ${start})")

echo "Time taken: $runtime seconds"

# Parse the output to find the line that starts with "Nodes searched:"
nodes_line=$(echo "$output" | grep "Nodes searched:")

# Extract the number of nodes from that line
nodes=${nodes_line#*: }

# Calculate nodes per second
nodes_per_second=$(python -c "print(int(${nodes} / ${runtime}))")

echo "Nodes per second: $nodes_per_second"

