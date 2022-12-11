from typing import Tuple, List

Interval = Tuple[int, int]
Pair = Tuple[Interval, Interval]

def parse_line(line: str) -> Tuple[Interval, Interval]:
    first, second = line.split(',')
    first = [int(f) for f in first.split('-')]
    second = [int(f) for f in second.split('-')]
    return first,second


def parse_input() -> List[Pair]:
    with open('fourth_puzzle_input.txt', 'r') as f:
        intervals = [parse_line(line.strip()) for line in f.readlines()]
    return intervals


def is_overlap(pair: Pair) -> bool:
    min_value = min(pair[0][0], pair[1][0])
    max_value = max(pair[0][1], pair[1][1])
    interval = [min_value, max_value]
    return interval in pair


def day4_count_complete_overlaps():
    pairs = parse_input()
    return sum(is_overlap(pair) for pair in pairs)

def day4_count_overlap_at_all():
    pairs = [sorted(pair) for pair in parse_input()]
    total = 0
    for (_, early_shift_end), (late_shift_start, _) in pairs:
        if early_shift_end >= late_shift_start:
            total += 1
    return total
    

if __name__=='__main__':
    print(f"day4: number of complete overlaps: {day4_count_complete_overlaps()}")
    print(f"day4  number of days that overlap at all: {day4_count_overlap_at_all()}")
