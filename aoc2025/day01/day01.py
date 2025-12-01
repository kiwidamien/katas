def parse_numbers(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f.readlines()]
    return [
        int(n[1:]) if n[0] == 'R' else -int(n[1:]) for n in lines
    ]

def make_stream(start: int, moves: [int]) -> [int]:
    states = [start]
    for move in moves:
        n = abs(move)
        direction = 1 if move > 0 else -1
        new = [num % 100 for num in range(states[-1] + direction, states[-1] + direction + move, direction)]
        states.extend(new)
    return states

def solve_part_2(filename):
    numbers = parse_numbers(filename)
    num_cross = len([n for n in make_stream(50, numbers) if n==0])
    print(f"Number of times crossing zero for {filename}: {num_cross}")


def main():
    solve_part_2("example.txt")
    solve_part_2("input.txt")


if __name__=='__main__':
    main()