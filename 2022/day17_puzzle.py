from typing import Tuple


_BAR = {(0,x) for x in range(4)}
_PLUS = {(0, 1), (1,0), (1,1), (1, 2), (2, 1)}
_CORNER = {(0,0), (0, 1), (0, 2), (1,2), (2,2)}
_I = {(x,0) for x in range(4)}
_SQUARE = {(x,y) for x in range(2) for y in range(2)}

ROCK_ORDER = [_BAR, _PLUS, _CORNER, _I, _SQUARE]


def new_state(width):
    return {(0, x) for x in range(width)}


def cycle(iterable):
    while True:
        for obj in iterable:
            yield obj 


def get_height(state) -> int:
    if len(state)==0:
        return 0 # height of floor
    return max([y for y, _ in state])


def map_string(state, width=7) -> str:
    max_height = get_height(state)
    lines = [
        '|' + ''.join('#' if (y,x) in state else '.' 
                      for x in range(width)) + '|'
        for y in range(max_height, 0, -1)
    ]
    last_line = '-'*(width+2)
    return '\n'.join(lines) + '\n' + last_line


def _gas_move(state, width, jet, current_rock_loc, current_rock_shape):
    proposed_shift = -1 if jet == "<" else 1
    proposed_shape_and_loc = {
        (y + current_rock_loc[0], x + current_rock_loc[1] + proposed_shift)
        for y, x in current_rock_shape
    }
    if proposed_shape_and_loc.intersection(state):
        return current_rock_loc
    for _, x in proposed_shape_and_loc:
        if x < 0 or x >=width:
            return current_rock_loc
    return (current_rock_loc[0], current_rock_loc[1] + proposed_shift)


def _move_down(state, current_rock_loc, current_rock_shape):
    proposed_loc = [
        (current_rock_loc[0] + y - 1, current_rock_loc[1] + x) 
        for y, x in current_rock_shape
    ]
    if set(proposed_loc).intersection(state):
        old_loc = [
            (current_rock_loc[0] + y, current_rock_loc[1] + x)
            for y, x in current_rock_shape
        ]
        new_state = set(old_loc).union(state)
        return (None, new_state)
    return ((current_rock_loc[0] - 1, current_rock_loc[1]), state)


def move_rock(state, width, jet, current_rock_loc, current_rock_shape):
    updated_rock_location = _gas_move(
        state=state, width=width, jet=jet, current_rock_loc=current_rock_loc,
        current_rock_shape=current_rock_shape
    )
    updated_rock_location, new_state = _move_down(
        state=state, current_rock_loc=updated_rock_location,
        current_rock_shape=current_rock_shape
    )
    return updated_rock_location, new_state


def next_resting_rock_spot(state, width, jet_iterator, current_rock_shape):
    current_rock_loc = (get_height(state)+4, 2)
    count = 0
    while True:
        jet = next(jet_iterator)
        current_rock_loc, state = move_rock(
            state=state, width=width, jet=jet, current_rock_loc=current_rock_loc,
            current_rock_shape=current_rock_shape
        )
        count += 1
        if current_rock_loc is None:
            return state, count


def drop_n_rocks(width, shape_order, jet_order, n_rocks):
    state = new_state(width)
    jet_iter = cycle(jet_order)
    for rock_num, rock in enumerate(cycle(shape_order)):
        state, count = next_resting_rock_spot(
            state=state, width=width, jet_iterator=jet_iter,
            current_rock_shape=rock
        )
        if rock_num + 1 == n_rocks:
            return state
    return state


def get_map_of_cave(jets: str, n_rocks: int, width: int=7):
    """Visualization / debugging tool only"""
    state = drop_n_rocks(width, shape_order=ROCK_ORDER, jet_order=jets, n_rocks=n_rocks)
    return map_string(state, width)


def height_of_tower(jets: str, n_rocks: int, width: int=7):
    """Utility function for wrapping the entire functionality"""
    state = drop_n_rocks(width, shape_order=ROCK_ORDER, jet_order=jets, n_rocks=n_rocks)
    return get_height(state)


def parse_file():
    with open('day17_input.txt') as f:
        return f.read()

def prob1_find_height_after_2022_rocks():
    jets = parse_file()
    return height_of_tower(jets=jets, n_rocks=2022, width=7)


def get_top_two_layers(state):
    height = get_height(state)
    top_layers = {
        (y - height + 1, x) for y, x in state
        if y>=height - 1
    }
    return top_layers


def _move_state_one_rock_cycle(jet_iter, state, width=7):
    total_count = 0
    for rock in ROCK_ORDER:
        state, count = next_resting_rock_spot(
            state=state, width=width, jet_iterator=jet_iter,
            current_rock_shape=rock)
        total_count += count
    return (state, total_count)


def _get_burnin_time(jet_iter, n_jets, width):
    state = new_state(width)
    seen_modulo = []
    total_count = 0
    n_rock_cycles_total = 0
    while True:
        state, count = _move_state_one_rock_cycle(jet_iter, state, width)
        total_count += count
        n_rock_cycles_total += 1
        mod_count = total_count % n_jets
        if mod_count in seen_modulo:
            break
        seen_modulo.append(mod_count)
    index = seen_modulo.index(mod_count)
    n_rock_cycles_to_repeat = len(seen_modulo) - index
    print(seen_modulo, mod_count)
    return (state, n_rock_cycles_to_repeat, n_rock_cycles_total)


def _estimate_period(jets, width=7):
    # lets do a burn in
    n_jets = len(jets)
    state = new_state(width)
    jet_iter = cycle(jets)
    # burn in
    state, n_rocks_cycles_to_repeat, n_rock_cycles_total = _get_burnin_time(jet_iter, n_jets, width)

    burnin_rocks = n_rock_cycles_total * len(ROCK_ORDER)
    burnin_height = get_height(state)

    # Now do cycle detection
    layer_states = get_top_two_layers(state)
    total_count = 0
    n_rock_cycles = 0
    while True:
        for _ in range(n_rocks_cycles_to_repeat):
            state, count = _move_state_one_rock_cycle(jet_iter, state, width)
            total_count += count
            n_rock_cycles += 1
        if layer_states == get_top_two_layers(state):
            break

    jet_elements_per_cycle = total_count
    rocks_per_cycle = n_rock_cycles*len(ROCK_ORDER)
    height_per_cycle = get_height(state) - burnin_height

    return {
        'burnin_rocks': burnin_rocks,
        'burnin_height': burnin_height,
        'jet_per_cycle': jet_elements_per_cycle,
        'rocks_per_cycle': rocks_per_cycle,
        'height_per_cycle': height_per_cycle,
    }


def height_of_tower_many(jets, n_rocks, width=7):
    cycle_data = _estimate_period(jets, width=width)
    leftover_rocks = n_rocks - cycle_data['burnin_rocks']
    num_cycles = leftover_rocks // cycle_data['rocks_per_cycle']
    rem_rocks = leftover_rocks % cycle_data['rocks_per_cycle']

    base_height = height_of_tower(jets=jets, n_rocks=cycle_data['burnin_rocks'] + rem_rocks, width=width)
    print(base_height)
    print(cycle_data)
    return base_height + num_cycles * cycle_data['height_per_cycle']


if __name__=='__main__':
    print(f'After 2022 rocks have dropped, the height is', prob1_find_height_after_2022_rocks())
    #many = 1000000000000
    many = 2022
    jets = parse_file()
    height_of_many = height_of_tower_many(jets=jets, n_rocks=many)
    print(f'After {many:,} rocks have dropped, the height is', height_of_many)
