#!/usr/bin/python
from dataclasses import dataclass
import subprocess
import sys
import select

@dataclass
class EngineStats:
    name: str
    wins: int = 0
    losses: int = 0
    draws: int = 0

WHITE_WIN_CODE = 1
BLACK_WIN_CODE = 2
STALEMATE_CODE = 3
THREEFOLD_CODE = 4
FIFTYMOVE_CODE = 5

def main():
    if len(sys.argv) != 4:
        print("Usage: fenrir_battleground.py engine1-path engin2-path ms-per-move")
        return
    
    
    engine1_path = sys.argv[1]
    engine2_path = sys.argv[2]
    ms = int(sys.argv[3])

    engine1 = subprocess.Popen(
        [engine1_path, "-q"],
        stdin=subprocess.PIPE, 
        stdout=subprocess.PIPE, 
        universal_newlines=True,
    )

    engine2 = subprocess.Popen(
        [engine2_path, "-q"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        universal_newlines=True,
    )

    engines = [engine1, engine2]

    white_idx, black_idx = 0, 1

    current_turn_idx, other_turn_idx = white_idx, black_idx




    engine1_version = engine1.stdout.readline().strip()
    engine2_version = engine2.stdout.readline().strip()
    print(f'Engine 1 is {engine1_version}')
    print(f'Engine 2 is {engine2_version}')

    scores = [EngineStats(name=engine1_version), EngineStats(name=engine2_version)]

    while True:
        engines[current_turn_idx].stdin.write(f"search time {ms}\n")
        engines[current_turn_idx].stdin.flush()
        move = engines[current_turn_idx].stdout.readline().strip()
        print(f"move = {move}")
        engines[other_turn_idx].stdin.write(f"{move}\n")
        engines[other_turn_idx].stdin.flush()

        reads, _, _ = select.select([engines[other_turn_idx].stdout], [], [], 0.2)

        if reads:
            # There's something to read, so read a line
            line = engines[other_turn_idx].stdout.readline()
            parts = line.split(":")
            print(parts)
            if len(parts) == 2 and parts[0] == "Game over":
                print("game is over")
                code = int(parts[1].strip())
                print(f"code = {code}")

                if code == WHITE_WIN_CODE:
                    scores[white_idx].wins += 1
                    scores[black_idx].losses += 1
                elif code == BLACK_WIN_CODE:
                    scores[black_idx].wins += 1
                    scores[white_idx].losses += 1
                elif code == STALEMATE_CODE or code == THREEFOLD_CODE or code == FIFTYMOVE_CODE:
                    scores[white_idx].draws += 1
                    scores[black_idx].draws += 1
            
            print("asdf")
            pgn = engines[other_turn_idx].stdout.readline()
            print("asdfsdaf")
            print(pgn)
            break

        current_turn_idx, other_turn_idx = other_turn_idx, current_turn_idx

    print(scores)

    # Clean up the subprocesses
    for engine in engines:
        engine.stdin.close()
        engine.terminate()
        engine.wait()


if __name__ == "__main__":
    main()
