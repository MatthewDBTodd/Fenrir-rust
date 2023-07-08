#!/usr/bin/python
from dataclasses import dataclass
import subprocess
import sys

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

    current_engine = 0


    engine1_version = engine1.stdout.readline().strip()
    engine2_version = engine2.stdout.readline().strip()
    print(f'Engine 1 is {engine1_version}')
    print(f'Engine 2 is {engine2_version}')

    scores = [EngineStats(name=engine1_version), EngineStats(name=engine2_version)]

    print(scores)


    # Clean up the subprocesses
    for engine in engines:
        engine.stdin.close()
        engine.terminate()
        engine.wait()


if __name__ == "__main__":
    main()
