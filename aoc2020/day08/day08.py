from typing import Tuple, List, Set
from copy import copy


State = Tuple[int, int]


def process(cmd: str) -> State:
    c, val = cmd.split()
    val = int(val)
    if c == "nop":
        return (1, 0)
    if c == "jmp":
        return (val, 0)
    if c == "acc":
        return (1, val)


def process_state(state: State, cmd: str) -> State:
    loc, acc = state
    delta_loc, delta_acc = process(cmd)
    return (loc + delta_loc, acc + delta_acc)


def process_list(
    cmds: List[str], start: State = (0, 0), visited: Set = None
) -> Tuple[State, State]:
    if visited is None:
        visited = set()
    else:
        visited = copy(visited)
    curr_state = start
    while curr_state[0] not in visited:
        prev_state = curr_state
        visited.add(curr_state[0])
        try:
            curr_state = process_state(prev_state, cmds[prev_state[0]])
        except IndexError:
            break
    return prev_state, curr_state


def corrupted(cmds: List[str], start: State = (0, 0)):
    visited = set()
    curr_state = start
    while curr_state[0] not in visited:
        location = curr_state[0]
        if cmds[location].startswith("nop"):
            # does switching the operation help?
            new_cmds = cmds[:]
            new_cmds[location] = cmds[location].replace("nop", "jmp")
            ps, cs = process_list(new_cmds, curr_state, visited)
            if cs[0] == len(cmds):
                return cs
        if cmds[location].startswith("jmp"):
            new_cmds = cmds[:]
            new_cmds[location] = cmds[location].replace("jmp", "nop")
            ps, cs = process_list(new_cmds, curr_state, visited)
            if cs[0] == len(cmds):
                return cs
        visited.add(location)
        try:
            curr_state = process_state(curr_state, cmds[location])
        except IndexError:
            break
    return curr_state


def main():
    data = open("input.txt").readlines()
    example_data = open("example.txt").readlines()
    print(process_list(example_data))
    print(process_list(data))
    print(corrupted(example_data))
    print(corrupted(data))


if __name__ == "__main__":
    main()
