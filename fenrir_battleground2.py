#!/usr/bin/python
from dataclasses import dataclass
import subprocess
import sys
from datetime import datetime
import fcntl
import os
import time

openings = {
    "English Opening" : ["c2c4"],
    "Benoni" : ["d2d4", "g8f6", "c2c4", "c7c5", "d4d5"],
    "Dutch Defence" : ["d2d4", "f7f5"],
    "Reti Opening" : ["g1f3", "d7d5", "c2c4"],
    "Benko Gambit" : ["d2d4", "g8f6", "c2c4", "c7c5", "d4d5", "b7b5"],
    "Old Indian Defence" : ["d2d4", "g8f6", "c2c4", "d7d6"],
    "Bird Opening" : ["f2f4"],
    "Sicilian" : ["e2e4", "c7c5"],
    "Caro-Kann" : ["e2e4", "c7c6"],
    "Pirc Defence" : ["e2e4", "d7d6", "d2d4", "g8f6", "b1c3", "g7g6"],
    "Alekhine's Defence" : ["e2e4", "g8f6"],
    "Modern Defence" : ["e2e4", "g7g6"],
    "Scandinavian Defence" : ["e2e4", "d7d5"],
    "Ruy Lopez" : ["e2e4", "e7e5", "g1f3", "b8c6", "f1b5"],
    "French Defence" : ["e2e4", "e7e6"],
    "Petrov's Defence" : ["e2e4", "e7e5", "g1f3", "g8f6"],
    "Vienna Game" : ["e2e4", "e7e5", "b1c3"],
    "Centre Game" : ["e2e4", "e7e5", "d2d4", "e5d4"],
    "King's Gambit" : ["e2e4", "e7e5", "f2f4"],
    "Philidor Defence" : ["e2e4", "e7e5", "g1f3", "d7d6"],
    "Italian Game" : ["e2e4", "e7e5", "g1f3", "b8c6", "f1c4"],
    "Scotch Game" : ["e2e4", "e7e5", "g1f3", "b8c6", "d2d4"],
    "Four Knights Game" : ["e2e4", "e7e5", "g1f3", "b8c6", "b1c3", "g8f6"],
    "Queen's Gambit Accepted" : ["d2d4", "d7d5", "c2c4", "d5c4"],
    "Queen's Gambit Declined" : ["d2d4", "d7d5", "c2c4", "e7e6"],
    "Grunfeld Defence" : ["d2d4", "g8f6", "c2c4", "g7g6", "b1c3", "d7d5"], 
    "Nimzo-Indian Defence" : ["d2d4", "g8f6", "c2c4", "e7e6", "b1c3", "f8b4"],
    "Queen's Indian Defence" : ["d2d4", "g8f6", "c2c4", "e7e6", "g1f3", "b7b6"],
    "King's Indian Defence" : ["d2d4", "g8f6", "c2c4", "g7g6"],
    "Catalan Opening" : ["d2d4", "g8f6", "c2c4", "e7e6", "g2g3"],
    "London System" : ["d2d4", "d7d5", "g1f3", "g8f6", "c1f4"],
}

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

def read_stdout(engine, timeout):
    fd = engine.stdout.fileno()
    fl = fcntl.fcntl(fd, fcntl.F_GETFL)
    fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)
    timeout_ms = timeout * 1000
    chunk = 10
    current = 0
    while current < timeout_ms:
        rv = engine.stdout.readline().strip()
        if rv:
            return rv 
        time.sleep(chunk / 1000)
        current += 10


def play_game_instance(engines, scores, white_idx, black_idx, opening_name, opening_moves, ms):
    print("Playing {} with white = {}, black = {}".format(opening_name, scores[white_idx].name, scores[black_idx].name))
    current_turn_idx, other_turn_idx = white_idx, black_idx
    for move in opening_moves:
        print(f"playing book move {move}")
        engines[current_turn_idx].stdin.write(f"{move}\n")
        engines[current_turn_idx].stdin.flush()
        engines[other_turn_idx].stdin.write(f"{move}\n")
        engines[other_turn_idx].stdin.flush()
        confirm1 = read_stdout(engines[current_turn_idx], 5.0)
        confirm2 = read_stdout(engines[other_turn_idx], 5.0)

        if not (confirm1 == "ok" and confirm2 == "ok"):
            print(f"Something has gone wrong. confirm1 = {confirm1}, confirm2 = {confirm2}")
            sys.exit(1)

        current_turn_idx, other_turn_idx = other_turn_idx, current_turn_idx
    while True:
        engines[current_turn_idx].stdin.write(f"search time {ms}\n")
        engines[current_turn_idx].stdin.flush()
        move = read_stdout(engines[current_turn_idx], 5.0)
        # print(f"{move}")
        engines[other_turn_idx].stdin.write(f"{move}\n")
        engines[other_turn_idx].stdin.flush()

        confirm2 = read_stdout(engines[other_turn_idx], 5.0)

        confirm1 = read_stdout(engines[current_turn_idx], 5.0)

        if confirm1 == "ok" and confirm2 == "ok":
            current_turn_idx, other_turn_idx = other_turn_idx, current_turn_idx
        else:
            parts = confirm1.split(":")
            parts2 = confirm2.split(":")
            if len(parts) == 2 and parts[0] == "Game over":
                code = int(parts[1].strip())

                if code == WHITE_WIN_CODE:
                    scores[white_idx].wins += 1
                    scores[black_idx].losses += 1
                elif code == BLACK_WIN_CODE:
                    scores[black_idx].wins += 1
                    scores[white_idx].losses += 1
                elif code == STALEMATE_CODE or code == THREEFOLD_CODE or code == FIFTYMOVE_CODE:
                    scores[white_idx].draws += 1
                    scores[black_idx].draws += 1
            else:
                print("Couldn't parse result: {} -- {}".format(parts, parts2))
                break
            pgn = read_stdout(engines[other_turn_idx], 5.0)
            _ = read_stdout(engines[current_turn_idx], 5.0)
            pgn_header = f"[Event \"Engine Battle with {opening_name}\"]\n"
            current_date = datetime.now()
            date_string = current_date.strftime("%Y.%m.%d")
            pgn_header += f"[Date \"{date_string}\"]\n"
            pgn_header += "[White \"{}\"]\n".format(scores[white_idx].name)
            pgn_header += "[Black \"{}\"]\n".format(scores[black_idx].name)
            if code == WHITE_WIN_CODE:
                pgn_header += "[Result \"1-0\"]\n"
            elif code == BLACK_WIN_CODE:
                pgn_header += "[Result \"0-1\"]\n"
            else:
                pgn_header += "[Result \"1/2-1/2\"]\n\n"
            
            pgn = pgn_header + pgn

            time_string = current_date.strftime("%Y.%m.%d-%H:%M:%S.pgn") 
            with open(time_string, "w") as f:
                f.write(pgn) 
            print(scores)
            break



def main():
    if len(sys.argv) != 5:
        print("Usage: fenrir_battleground.py engine1-path engine2-path games-per-opening ms-per-move")
        return
    
    
    engine1_path = sys.argv[1]
    engine2_path = sys.argv[2]
    iterations = int(sys.argv[3])
    ms = int(sys.argv[4])

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

    engine1_version = read_stdout(engine1, 5.0)
    engine2_version = read_stdout(engine2, 5.0)
    print(f'Engine 1 is {engine1_version}')
    print(f'Engine 2 is {engine2_version}')

    scores = [EngineStats(name=engine1_version), EngineStats(name=engine2_version)]

    for opening_name, opening_moves in openings.items():
        for _ in range(iterations):
            play_game_instance(engines, scores, white_idx, black_idx, opening_name, opening_moves, ms)
            white_idx, black_idx = black_idx, white_idx
            play_game_instance(engines, scores, white_idx, black_idx, opening_name, opening_moves, ms)
            white_idx, black_idx = black_idx, white_idx

    engine1_score = scores[0].wins + (scores[0].draws * 0.5)
    engine2_score = scores[1].wins + (scores[1].draws * 0.5)
    with open("summary.txt", "w") as f:
        f.write("{} {}-{} {}".format(scores[0].name, engine1_score, engine2_score, scores[1].name))
        f.write("\n\n")
        f.write("{} Summary:\n".format(scores[0].name))
        f.write("{} wins\n".format(scores[0].wins))
        f.write("{} draws\n".format(scores[0].draws))
        f.write("{} losses".format(scores[0].losses))
        f.write("\n\n")
        f.write("{} Summary:\n".format(scores[1].name))
        f.write("{} wins\n".format(scores[1].wins))
        f.write("{} draws\n".format(scores[1].draws))
        f.write("{} losses\n".format(scores[1].losses))
    print(scores)

    # Clean up the subprocesses
    for engine in engines:
        engine.stdin.close()
        engine.terminate()
        engine.wait()


if __name__ == "__main__":
    main()
