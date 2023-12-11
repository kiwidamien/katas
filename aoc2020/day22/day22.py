from collections import deque


def play(deck1: list[int], deck2: list[int]):
    p1, d1 = deck1[0], deck1[1:]
    p2, d2 = deck2[0], deck2[1:]
    if p1 > p2:
        d1.extend([p1, p2])
    else:
        d2.extend([p2, p1])
    return (d1, d2)


def play_full_game(deck1: list[int], deck2: list[int]):
    while len(deck1) * len(deck2) > 0:
        deck1, deck2 = play(deck1, deck2)
    return deck1, deck2


def score_deck(deck: list[int]) -> int:
    reverse = deck[::-1]
    acc = 0
    for index, card in enumerate(reverse):
        acc += (index + 1) * card
    return acc


def parse(filename: str):
    with open(filename) as f:
        data = [line.strip() for line in f.readlines() if not line.startswith("Player")]
    decks = [[]]
    for card in data:
        if card == "":
            decks.append([])
            continue
        decks[-1].append(int(card))
    return (decks[0], decks[1])


def part1(deck1, deck2):
    deck1, deck2 = play_full_game(deck1, deck2)
    # The empty deck has a score of zero, so this is really
    # just a way of scoring the non-empty deck
    return score_deck(deck1) + score_deck(deck2)
