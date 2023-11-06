import fire
from typing import List, Dict


def countJumps(adapters: List[int]) -> Dict[int, int]:
    sorted_adapters = sorted(adapters)
    differences = [next - this for this, next in zip(sorted_adapters, sorted_adapters[1:])]
    counts = {}
    for d in differences:
        counts[d] = counts.get(d, 0) + 1
    return counts

def scoreJumps(counts: Dict[int, int]) -> int:
    return counts.get(3, 0) * counts.get(1, 0)


def part1(adapters: List[int]) -> int:
    biggest = max(adapters)
    with_outlet = [0] + adapters + [biggest + 3]
    return scoreJumps(countJumps(with_outlet))


def main(filename: str):
    with open(filename) as f:
        adapters = [int(n) for n in f.readlines()]
    print(part1(adapters))

if __name__ == "__main__":
    fire.Fire(main)
