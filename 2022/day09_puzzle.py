from collections import namedtuple
from typing import Tuple, List

example_input = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

Position = Tuple[int, int]
DeltaPosition = Position
State = List[Position] 


def _parse_head_move(direction: str) -> DeltaPosition:
    return {
        'U': (-1,0), 'D': (1,0), 'L': (0, -1), 'R': (0, 1)
    }[direction]


def delta_position(end: Position, start: Position) -> DeltaPosition:
    return (end[0] - start[0], end[1] - start[1])


def sign(x) -> int:
    return 0 if x == 0 else (1 if x > 0 else -1)


def _new_tail_position(old_tail: Position, new_head: Position) -> Position:
    delta = delta_position(new_head, old_tail)
    # Are they overlapping? No move
    if delta == (0, 0):
        return old_tail

    # Are they touching? No move
    biggest_delta = max(abs(x) for x in delta)
    if biggest_delta == 1:
        return old_tail

    horiz_delta = sign(delta[1]) 
    vert_delta = sign(delta[0])
    return (old_tail[0] + vert_delta, old_tail[1] + horiz_delta)


def update_state(state: State, direction: str) -> State:
    delta = _parse_head_move(direction)
    new_head = (state[0][0] + delta[0], state[0][1] + delta[1])
    new_state = [new_head]
    for knot_position in state[1:]:
        new_head = new_state[-1]
        new_knot = _new_tail_position(new_head=new_head, old_tail=knot_position)
        new_state.append(new_knot)
    return new_state


def update_state_multimove(state: State, direction: str, amount: int) -> List[State]:
    states = [state]
    for _ in range(amount):
        states.append(update_state(states[-1], direction))
    return states[1:]


def process_states(initial_state: State, commands: List[str]) -> List[State]:
    states = [initial_state]
    for line in commands:
        direction, amount = line.strip().split()
        amount = int(amount)
        states = states + update_state_multimove(states[-1], direction, amount)
    return states


def count_tail_locations(state_history: List[State]) -> int:
    tails = [s[-1] for s in state_history]
    return len(set(tails))


def parse_file() -> str:
    with open('day09_input.txt', 'r') as f:
        contents = f.read()
    return contents


if __name__ == '__main__':
    commands = parse_file().split('\n')
    initial_state = [(0,0), (0,0)]
    state_history = process_states(initial_state=initial_state, commands=commands)
    print(count_tail_locations(state_history))

