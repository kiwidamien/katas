from typing import List, Tuple


# Algorithm
def visible_line(nums: List[int]) -> List[int]:
    current_floor = -1
    visible = []
    for index, first in enumerate(nums):
        if first > current_floor:
            visible.append(index)
        current_floor = max(first, current_floor)
    return visible


def replace_lines(num: List[int]) -> List[int]:
    num = num[:]
    start_visible, end_visible = visible_line(num), visible_line(num[::-1])
    for i in start_visible:
        num[i] = -1
    for i in end_visible:
        num[len(num) - 1 -i] = -1
    return num


def determine_visible(forest: List[List[int]]) -> List[List[int]]:
    horizontals = [replace_lines(line) for line in forest]
    verticals = list(zip(*[replace_lines(list(line)) for line in zip(*forest)]))
    rows = len(horizontals)
    cols = len(horizontals[0])
    new_forest = [[min(horizontals[r][c], verticals[r][c]) for c in range(cols)] for r in range(rows)]
    return new_forest


def _count_visible_of_processed(processed_forest) -> int:
    rows = len(processed_forest)
    cols = len(processed_forest[0])
    counter = [1 if processed_forest[r][c]==-1 else 0 for r in range(rows) for c in range(cols)]
    return sum(counter)


def count_all_visible_from_outside(raw_forest: List[List[int]]) -> int:
    processed_forest = determine_visible(raw_forest)
    return _count_visible_of_processed(processed_forest)


# second problem
def count_trees_in_half_line(this_height: int, semi_axis: List[int]) -> int:
    count = 0
    if semi_axis == []:
        return 0
    for index, tree_height in enumerate(semi_axis):
        if tree_height >= this_height:
            break
    return index+1


def count_visible_in_line(line: List[int], position: int):
    this_height = line[position]
    before, after = line[:position][::-1], line[position + 1:]
    return (count_trees_in_half_line(this_height, before), count_trees_in_half_line(this_height, after))


def count_visible_from_tree(row: int, col: int, forest: List[List[int]]) -> Tuple[int, int, int, int]:
    the_row = forest[row]
    the_col = [forest[r][col] for r in range(len(forest))]
    return count_visible_in_line(the_row, col) + count_visible_in_line(the_col, row)

def score_tree(row: int, col: int, forest: List[List[int]]) -> int:
    up, down, left, right = count_visible_from_tree(row=row, col=col, forest=forest)
    return up*down*left*right


# Test helpers / debuggers
def test_forest() -> List[List[int]]:
    forest = [
        [3,0,3,7,3],
        [2,5,5,1,2],
        [6,5,3,3,2],
        [3,3,5,4,9],
        [3,5,3,9,0],
    ]
    return forest

def print_forest(forest):
    for row in forest:
        s = ''.join(f' {x:2d} ' for x in row)
        print(s)

# Parsing
def parse_file() -> List[List[int]]:
    with open('day07_input.txt', 'r') as f:
        contents = [[int(d) for d in line.strip()] for line in f.readlines()]
    return contents


# Driver
if __name__ == '__main__':
    actual_forest = parse_file()
    print(count_all_visible_from_outside(raw_forest=actual_forest))
    print(count_visible_from_tree(3, 2, test_forest()))
    tree_scores = [
        score_tree(row=row, col=col, forest=actual_forest)
        for row in range(len(actual_forest))
        for col in range(len(actual_forest[0]))
    ]
    print(max(tree_scores))
