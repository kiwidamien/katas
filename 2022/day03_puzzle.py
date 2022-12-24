from typing import List
from functools import reduce


def find_duplicate_in_rugsack(line):
    assert len(line) % 2 == 0, f'{line} is not even in length: {len(line)}'
    first = set(line[:len(line)//2])
    second = set(line[len(line)//2:])
    duplicated = first.intersection(second)
    assert len(duplicated) == 1, f'{line} does not have exactly one duplicate: {duplicated}'
    return duplicated.pop()


def find_priority_char(c: str) -> int:
    if ord(c) < ord('a'):
        return ord(c) - ord('A') + 1 + 26
    return ord(c) - ord('a') + 1


def find_priority_rugsack(line: str) -> int:
    char_duplicated = find_duplicate_in_rugsack(line)
    priority = find_priority_char(char_duplicated)
    return priority


def parse_rugsacks():
    with open('day03_input.txt','r') as f:
        lines = [line.strip() for line in f.readlines()]
    return lines

def day3_sum_priorities() -> int:
    rugsacks = parse_rugsacks()
    return sum(find_priority_rugsack(line) for line in rugsacks)


def find_items_common_to_all_rugsacks(*lines: List[str]):
    common = reduce(lambda acc, element: acc.intersection(set(element)), lines, set(lines[0]))
    return common


def day3_common_to_triplets() -> int:
    rugsacks = parse_rugsacks()
    badges = [
        find_items_common_to_all_rugsacks(rugsacks[index], rugsacks[index+1], rugsacks[index+2])
        for index in range(0, len(rugsacks), 3)
    ]
    priorities = [
        find_priority_char(badge.pop()) for badge in badges
    ]
    return sum(priorities)


if __name__=='__main__':
    print("Hello")
    print(find_duplicate_in_rugsack("vJrwpWtwJgWrhcsFMMfFFhFp"))
    print(find_priority_rugsack("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"))
    print(f"Day 3: sum priorities are {day3_sum_priorities()}")
    print(f"Day 3: find sum of badge priorities: {day3_common_to_triplets()}")
