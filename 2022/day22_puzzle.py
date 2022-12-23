from collections import namedtuple
import string
from typing import Dict, Tuple


State = namedtuple("State", "row col direction")
Floor = Dict[Tuple[int, int], str]


parse_cube = lambda s: [list(row) for row in s.split('\n') if row]

def parse_move_string_recurrsive(s: str):
    if s=="":   
        return []
    
    if s[0] in ["L", "R"]:
        return [s[0]] + parse_move_string(s[1:])

    for index, char in enumerate(s):
        if char not in string.digits:
            entry = int(s[0:index])
            return [entry] + parse_move_string(s[index:])
    return [int(s)]


def parse_move_string(move: str):
    accumulator = []
    is_digit = (move[0] in string.digits)
    start_index, current_index = 0, 0
    while current_index < len(move):
        if is_digit:
            if move[current_index] in string.digits:
                current_index += 1
                continue
            else:
                num = int(move[start_index:current_index])
                accumulator.append(num)
                start_index = current_index
                is_digit = False
                continue
        else:
            accumulator.append(move[current_index])
            current_index += 1
            start_index = current_index
            is_digit=True
    if is_digit:
        accumulator.append(int(move[start_index:]))
    else:
        accumulator.append(move[start_index:])
    return accumulator


def parse_floorplan(floor_string):
    m = {}
    for index, line in enumerate(floor_string.split('\n')):
        for col_index, char in enumerate(line):
            if char != ' ':
                m[(index + 1, col_index+1)] = char
    return m


def parse_contents(contents):
    floorplan, moves = contents.split('\n\n')
    return parse_floorplan(floorplan), parse_move_string(moves)


def starting_state(floorplan: Floor):
    keys = [loc for loc in sorted(floorplan) if floorplan[loc]=='.']
    loc = keys[0]
    return State(row=loc[0], col=loc[1], direction=0)


def turn_left(state: State):
    return State(
        row=state.row, col=state.col, direction=(state.direction - 1) % 4
    )


def turn_right(state: State) -> State:
    return State(
        row=state.row, col=state.col, direction=(state.direction + 1) % 4
    )


def _step(direction: int) -> Tuple[int, int]:
    return [(0, 1), (1, 0), (0, -1), (-1, 0)][direction]


def location_forward_one(state: State, floorplan: Floor) -> State:
    step = _step(state.direction)
    proposed = (state.row + step[0], state.col + step[1])
    if proposed in floorplan:
        return State(*proposed, state.direction)
    if step[0]==0:  # stay in same row
        cols = [c for r, c in floorplan if r==state.row]
        min_col, max_col = min(cols), max(cols)
        new_col = min_col + (state.col + step[1]-min_col) % (max_col-min_col + 1)
        return State(state.row, new_col, state.direction)
    if step[1]==0:
        rows = [r for r, c in floorplan if c==state.col]
        min_row, max_row = min(rows), max(rows)
        new_row = min_row + (state.row + step[0] - min_row) % (max_row - min_row + 1)
        return State(new_row, state.col, state.direction)
    raise ValueError('illegal move?')


def location_forward_one_cube(state: State, floorplan: Floor) -> Tuple[int, int]:
    step = _step(state.direction)
    proposed = (state.row + step[0], state.col + step[1])
    if proposed in floorplan:
        return State(*proposed, state.direction)
    # now do the cube wrapping
    ...



def walk(floorplan: Floor, moves, starting_loc: State):
    current = starting_loc
    for move in moves:
        if move == 'L':
            current = turn_left(current)
            continue
        if move == 'R':
            current = turn_right(current)
            continue
        for _ in range(move):
            proposed = location_forward_one(current, floorplan)
            if floorplan[(proposed[0], proposed[1])]=='#':
                break
            current = proposed
    return current

def parse_file():
    with open('day22_input.txt') as f:
        return parse_contents(f.read())


def problem1_find_coord():
    floor, moves = parse_file()
    start = starting_state(floor)
    end = walk(floor, moves, starting_loc=start)
    password = 1000*end.row + 4*end.col + end.direction
    return password


if __name__ == '__main__':
    print(problem1_find_coord())
