def parse_card(line: str) -> (int, list[int], list[int]):
    card_info, round_info = line.split(":")
    _, card_id = card_info.split()
    card_id = int(card_id)
    pre_win, pre_ours = round_info.split("|")
    winning_numbers = [int(n) for n in pre_win.split()]
    our_numbers = [int(n) for n in pre_ours.split()]
    return card_id, winning_numbers, our_numbers


def score(winning_numbers: list[int], our_numbers: list[int]) -> int:
    num_matches = len(set(winning_numbers) & set(our_numbers))
    if num_matches == 0:
        return 0
    return 2 ** (num_matches - 1)


def score_examples(lines: list[str]) -> dict[int, int]:
    results = {}
    for line in lines:
        card_id, winning, ours = parse_card(line)
        results[card_id] = score(winning, ours)
    return results


def part1(filename: str):
    with open(filename) as f:
        example = [line.strip() for line in f.readlines()]
    scores = score_examples(example)
    return sum(scores.values())
