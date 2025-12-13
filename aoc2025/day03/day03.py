def highest_line(line: str) -> int:
    """Given a series of digits, pick the biggest 2 digit number from the sequence.

    1. We pick two digits, in order, but not necessarly consecutive
    2. We concatinate them, to make another number

    Pick the largest such number that can be formed.

    Examples
    
    987654321111111  -> 98
    811111111111119  -> 89   (pick '8' then '9')
    234234234234278  -> 78
    818181911112111  -> 92
    """
    for first in '987654321':
        if first not in line:
            continue
        start = line.index(first)
        if start == len(line) - 1:
            continue
        second = max(line[start+1:])
        return int(first + second)


def _highest(line: str, num_to_take: int) -> str:
    if num_to_take == 1:
        return max(line)
    digit = max(line[:-(num_to_take - 1)])
    index = line.index(digit)
    return digit + _highest(line[index+1:], num_to_take-1) 




def solution_one(filename: str) -> int:
    with open(filename) as f:
        lines = f.readlines()
    highest = [highest_line(line.strip()) for line in lines]
    return sum(highest)

def solution_two(filename: str) -> int:
    with open(filename) as f:
        lines = f.readlines()
    highest = [int(_highest(line.strip(), 12)) for line in lines]
    return sum(highest)