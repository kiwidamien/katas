from collections import Counter


def parse(buffer: str) -> (list[int], list[int]):
    lines = [words for words in buffer.split("\n")]
    list_of_pairs = [[int(w) for w in line.split()] for line in lines if len(line) > 0]
    return list(zip(*list_of_pairs))


def part01(filename: str):
    with open(filename) as f:
        buffer = f.read()
    left, right = parse(buffer)
    left = sorted(left)
    right = sorted(right)
    return sum([abs(l - r) for l, r in zip(left, right)])


def similarity_score(left: list[int], right: list[int]):
    left_count = Counter(left)
    right_count = Counter(right)
    components = [
        left_count[value] * value * right_count[value] for value in left_count
    ]
    return sum(components)


def part02(filename: str):
    with open(filename) as f:
        buffer = f.read()
    left, right = parse(buffer)
    return similarity_score(left, right)


def example():
    print(part01("example1.txt"))
    print(part02("example1.txt"))


def part01_our_input():
    print(part01("input.txt"))


def part02_our_input():
    print(part02("input.txt"))


if __name__ == "__main__":
    example()
    part01_our_input()
    part02_our_input()
