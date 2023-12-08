import string

Location = (int, int)


def extract_number(
    lines: list[str], row: int, col: int
) -> (int | None, bool, int, int):
    if lines[row][col] not in string.digits:
        return (None, False, row, col)
    digits = []
    current = col
    while current < len(lines[row]):
        if lines[row][current] not in string.digits:
            break
        digits.append(lines[row][current])
        current += 1
    num = int("".join(digits))
    col_stop = current - 1
    validity = is_valid(lines, row, col, col_stop)
    return (num, validity, row, col_stop)


def is_valid(lines, row, col, col_stop):
    rows = [r for r in [row - 1, row, row + 1] if 0 <= r < len(lines)]
    cols = [c for c in range(col - 1, col_stop + 2) if 0 <= c < len(lines[row])]
    symbols = set(string.punctuation) - {"."}
    return any([lines[r][c] in symbols for r in rows for c in cols])


def num_scan(lines: list[str]):
    acc = locate_nums(lines)
    return [num for (num, _, _, _) in acc]


def locate_nums(lines: list[str]):
    acc = []
    for row, line in enumerate(lines):
        col = 0
        while col < len(line):
            col_start = col
            (num, valid, _, col) = extract_number(lines, row, col)
            if valid:
                acc.append((num, row, col_start, col))
            col = col+1
    return acc

def is_gear(row, col, number_list) -> bool:
    return extract_gear_nums(row, col, number_list) is not None

def extract_gear_nums(row, col, number_list: list[(int, int, int, int)]):
    """Returns the two numbers adjancent to (row, col) if there are exactly two, else returns None"""
    def is_adjacent(r, col_start, col_end):
        if r not in [row-1, row, row+1]:
            return False 
        if col_end < col - 1:
            return False
        if col_start > col + 1:
            return False
        return True 
    nums = [num for (num, r, col_start, col_end) in number_list if is_adjacent(r, col_start, col_end)]
    if len(nums) != 2:
        return None
    return tuple(nums)


def locate_gears(lines: list[str]):
    number_list = locate_nums(lines)
    acc = []
    for row, line in enumerate(lines):
        for col, char in enumerate(line):
            if char != '*':
                continue
            if is_gear(row, col, number_list):
                n1, n2 = extract_gear_nums(row, col, number_list)
                acc.append(n1*n2)
    return acc 


def part1(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f.readlines()]
    return sum(num_scan(lines))


def part2(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f.readlines()]
    return sum(locate_gears(lines))
