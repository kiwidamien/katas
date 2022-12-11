import copy
import re
import collections
from typing import List, Dict

Move = collections.namedtuple('Move', "amount source dest")
StackState = Dict[int, List[str]]

def read_line_stack(line, n_stacks=9):
    letter = 0
    stacks = {}
    for letter in range(n_stacks):
        try:
            char = line[4*letter]
        except IndexError:
            break
        if char != '[':
            continue
        payload = line[4*letter + 1]
        stacks[letter + 1] = payload
    return stacks


def read_stack(lines, n_stacks=9):
    stacks = {stack: [] for stack in range(1, n_stacks+1)}
    for line in lines[::-1]:
        for stack_num, letter in read_line_stack(line).items():
            stacks[stack_num].append(letter)
    return stacks


def parse_moves(lines) -> List[Move]:
    moves = [Move(*[int(n) for n in re.search(r"move (\d+) from (\d+) to (\d+)", line).groups()])
        for line in lines
    ]
    return moves


def parse_input():
    with open('fifth_puzzle_input.txt', 'r') as f:
        contents = [line.strip() for line in f.readlines()]
    index = contents.index("")
    return contents[:index-1], contents[index+1:]


def day5_move_stacks():
    stacks, moves = parse_input()
    stacks = read_stack(stacks)
    m = parse_moves(moves)

    for move in m:
        stacks = update_state_one_at_a_time(stacks, move)
    print_state(stacks)
    print_top_of_stacks(stacks)


def day5_move_grouped_stacks():
    stacks, moves = parse_input()
    stacks = read_stack(stacks)
    m = parse_moves(moves)
    for move in m:
        stacks = update_state_as_group(stacks, move)
    print_top_of_stacks(stacks)


def print_state(stacks):
    s = ""
    for key in sorted(stacks):
        s += f'{key}:  {"".join(stacks[key])}\n'
    print(s)


def print_top_of_stacks(stacks):
    s = ""
    for key in sorted(stacks):
        s += stacks[key][-1]
    print(s)


def update_state_one_at_a_time(state: StackState, move: Move) -> StackState:
    state = copy.deepcopy(state)
    state[move.source], moving = state[move.source][:-move.amount], state[move.source][-move.amount:]
    state[move.dest].extend(moving[::-1])
    return state


def update_state_as_group(state: StackState, move: Move) -> StackState:
    state = copy.deepcopy(state)
    state[move.source], moving = state[move.source][:-move.amount], state[move.source][-move.amount:]
    state[move.dest].extend(moving)
    return state


if __name__=="__main__":
   
    day5_move_stacks()
    day5_move_grouped_stacks()
