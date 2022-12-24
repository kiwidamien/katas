from typing import List


def get_elves(file='day01_input.txt'):
    with open(file, 'r') as f:
        content = [[int(c) for c in elf.split()] for elf in f.read().split("\n\n")]
    return content


def total_calories(elves: List[List[int]]):
    return [sum(single_elf) for single_elf in elves]


def day1_most_calories():
    elves = get_elves()
    calories = total_calories(elves)
    return max(calories)


def day1_total_top_3_calories():
    elves = get_elves()
    calories = sorted(total_calories(elves), reverse=True)
    return sum(calories[:3])


if __name__ == '__main__':
    print(f"day 1 part 1: Total calories are {day1_most_calories()}")
    print(f"day 1 part 2: Sum of top three calories are {day1_total_top_3_calories()}")

