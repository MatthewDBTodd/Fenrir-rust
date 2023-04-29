#!/usr/bin/env python3
import pyperclip

def main():
    fen = pyperclip.paste()
    fen = fen.split(' ')
    fen = fen[0]
    ranks = fen.split('/')
    result = 0
    shift = 63
    for rank in ranks:
        rank = rank[::-1]
        for c in rank:
            if c.isnumeric():
                shift -= int(c)
            else:
                result |= (1 << shift)
                shift -= 1
    print(hex(result))
    pyperclip.copy(hex(result))
    

if __name__ == "__main__":
    main()
