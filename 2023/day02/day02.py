example = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"


def parse_round(round_str: str):
    colors = [r.strip() for r in round_str.strip().split(",")]
    counter = {}
    for c in colors:
        amt, name = c.split()
        counter[name] = int(amt)
    return counter


def parse_line(line: str):
    game, state = line.split(":")
    game_id = game.split()[1]
    rounds = state.split(";")
    return int(game_id), [parse_round(r) for r in rounds]


def is_legal_round(round_info: dict[str, int], world_info: dict[str, int]) -> bool:
    for name, number in round_info.items():
        if number > world_info.get(name, 0):
            return False
    return True


def round_power(rounds: list[dict[str, int]]):
    acc = 1
    for color in ["red", "green", "blue"]:
        acc *= max([r.get(color, 0) for r in rounds])
    return acc


def part_1(lines: list[str]):
    def is_legal_game(rounds: list[dict[str, int]]):
        return all(
            [
                is_legal_round(round, {"red": 12, "green": 13, "blue": 14})
                for round in rounds
            ]
        )

    legit_ids = []
    for line in lines:
        game_id, rounds = parse_line(line)
        if is_legal_game(rounds):
            legit_ids.append(game_id)
    return sum(legit_ids)


def part_2(lines: list[str]):
    powers = []
    for line in lines:
        _, rounds = parse_line(line)
        powers.append(round_power(rounds))
    return sum(powers)


def main(filename: str = "example.txt"):
    with open(filename) as f:
        lines = f.readlines()
    print(part_1(lines))
    print(part_2(lines))
