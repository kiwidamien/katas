from typing import Dict, List, Tuple, Set


Location = Tuple[int, int]
Field = Set[Location]


_dirs = {
    'N': [(x, -1) for x in [-1, 0, 1]],
    'S': [(x, 1) for x in [-1, 0, 1]],
    'E': [(1, y) for y in [-1, 0, 1]],
    'W': [(-1,y) for y in [-1, 0, 1]]
}

_step = {
    'N': (0, -1), 'S': (0, 1), 'E': (1, 0), 'W': (-1, 0)
}


def can_propose_direction(field: Field, location: Location, checks: List[Location]) -> bool:
    return all([(location[0] + c[0], location[1]+c[1]) not in field for c in checks])


def can_move(field, location):
    dirs = [
        (x,y) for x in [-1, 0, 1] for y in [-1, 0, 1]
        if (x,y) != (0,0)
    ]
    return not can_propose_direction(field, location, checks=dirs)


def can_move_compass(field, location, compass):
    return can_propose_direction(field, location, checks=_dirs[compass])


def propose_move(field, location, order):
    if not can_move(field, location):
        return location
    for compass in order:
        if can_move_compass(field, location, compass):
            step = _step[compass]
            return (location[0] + step[0], location[1] + step[1])
    return location


def do_move(field: Field, order=List[str]):
    proposals = {}
    seen_locations = set() 
    collision_locations = set()
    for location in field:
        proposed = propose_move(field, location, order)
        if proposed in seen_locations:
            proposals[location] = location
            collision_locations.add(proposed)
            continue
        proposals[location] = proposed
        seen_locations.add(proposed)
    return {
        prop if prop not in collision_locations else location
        for location, prop in proposals.items()
    }


def mover(initial_field, initial_order):
    field = initial_field
    order = list(initial_order)
    while True:
        field = do_move(field, order)
        order = order[1:] + [order[0]]
        yield field


def empty_area(field: Field):
    x_values, y_values = zip(*field)
    width = max(x_values) - min(x_values) + 1
    height = max(y_values) - min(y_values) + 1
    return width * height - len(field)


def draw(field) -> str:
    x_values, y_values = zip(*field)
    x_min, x_max = min(x_values), max(x_values)
    y_min, y_max = min(y_values), max(y_values)
    matrix = [
        ''.join(['#' if (x,y) in field else '.' for x in range(x_min, x_max+1)])
        for y in range(y_min, y_max+1)
    ]
    return '\n'.join(matrix)


def parse_contents(contents):
    field = set()
    for y, line in enumerate(contents.split('\n')):
        for x, char in enumerate(line):
            if char=='#':
                field.add((x,y))
    return field


def parse_file():
    with open('day23_input.txt') as f:
        return parse_contents(f.read())


def problem1_empty_area_10_moves():
    field = parse_file()
    for step, f in zip(range(10), mover(field, 'NSWE')):
        pass
    return empty_area(f)


def find_steady_state(initial_field, initial_order):
    last_field  = initial_field
    for step, field in enumerate(mover(initial_field, initial_order)):
        if last_field == field:
            return step+1
        last_field=field


def problem2_find_steady_state():
    initial_field = parse_file()
    return find_steady_state(initial_field, 'NSWE')


if __name__ == '__main__':
    print(problem1_empty_area_10_moves())
    print(problem2_find_steady_state())

