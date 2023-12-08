import string
from functools import reduce
from typing import Optional

def calibration_value(line: str) -> int:
    digits = [d for d in line if d in string.digits]
    return int(digits[0] + digits[-1])

def digit_word(s: str, index: int) -> Optional[int]:
    lookup = {
        'one': 1,
        'two': 2,
        'three': 3,
        'four': 4,
        'five': 5,
        'six': 6,
        'seven': 7,
        'eight': 8,
        'nine': 9,
    }
    for name, value in lookup.items():
        if s[index:index + len(name)] == name:
            return value
    return None

def advanced_calibration_value(line: str) -> int:
    digits = []
    index = 0
    while index < len(line):
        if line[index] in string.digits:
            digits.append(line[index])
            index += 1
            continue
        possible_spelling = digit_word(line, index)
        if possible_spelling:
            digits.append(str(possible_spelling))
        index += 1
    return int(digits[0] + digits[-1])


def product_of_calibration(lines: list[str]) -> int:
    values = [calibration_value(line) for line in lines]
    return reduce(lambda acc, new: acc * new, values, 1)

def sum_of_calibration(lines: list[str], func=calibration_value) -> int:
    values = [func(line) for line in lines]
    return reduce(lambda acc, new: acc + new, values, 0)

def part1(filename):
    with open(filename) as f:
        return sum_of_calibration(lines=f.readlines(), func=calibration_value)

def part2(filename):
    with open(filename) as f:
        return sum_of_calibration(lines=f.readlines(), func=advanced_calibration_value)

    
if __name__=='__main__':
    print(part1('input.txt'))
    print(part2('input.txt'))