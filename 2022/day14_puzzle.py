from collections import namedtuple
from typing import Dict, Tuple, List


Location = namedtuple("Location", "x y")
Terrain = Dict[Location, str]

def _update_terrain_for_line(line: str, mutable_terrain: Terrain):
    locations = []
    locs = [loc for loc in line.split(" -> ")]
    for loc in locs:
        x,y = [int(num) for num in loc.split(',')]
        locations.append((x,y))
    for start, end in zip(locations, locations[1:]):
        min_horiz = min(start[0], end[0])
        min_vert = min(start[1], end[1])
        for horiz_fill in range(abs(start[0] - end[0]) + 1):
            mutable_terrain[(min_horiz + horiz_fill, min_vert)] = '#'
        for vert_fill in range(abs(start[1] - end[1]) + 1):
            mutable_terrain[(min_horiz, min_vert + vert_fill)] = '#'


def parse_input_string(wall_notation: str) -> Terrain:
    lines = wall_notation.split("\n")
    terrain={}
    for line in lines:
        _update_terrain_for_line(line, terrain)
    return terrain


def lowest_row(terrain: Terrain) -> int:
    y_vals = [y for (_, y), v in terrain.items() if v=='#']
    return max(y_vals) + 1


def find_final_location(terrain: Terrain, start: Location) -> Location:
    def move(current: Location) -> Location:
        x, y = current
        if (x, y+1) not in terrain:
            return (x, y+1)
        if (x-1, y+1) not in terrain:
            return (x-1, y+1)
        if (x+1, y+1) not in terrain:
            return (x+1, y+1)
        return (x, y)
    current = start
    lowest = lowest_row(terrain)
    while current[1] != lowest:
        updated = move(current)
        if updated == current:
            return current
        current = updated
    return current


def drop_sand_grain(terrain: Terrain, start: Location) -> (Terrain, Location):
    final_location = find_final_location(terrain, start)
    new_terrain = terrain.copy()
    new_terrain[final_location] = 'o'
    return (new_terrain, final_location)


def number_of_grains_come_to_rest(terrain: Terrain, start: Location) -> int:
    counter = 0 
    stopping_point = lowest_row(terrain)
    while True:
        terrain, location = drop_sand_grain(terrain, start)
        if location[1]==stopping_point:
            return counter
        counter += 1


def print_terrain(min_col, max_col, terrain):
    for row in range(lowest_row(terrain)+1):
        s = ''.join(terrain.get((col, row), '.') for col in range(min_col, max_col+1))
        print(s)


def parse_file():
    with open('day14_input.txt', 'r') as f:
        return f.read()

def solve_problem_part_one():
    wall_text_from_file = parse_file()
    terrain = parse_input_string(wall_text_from_file)
    resting_grains = number_of_grains_come_to_rest(terrain, (500, 0))
    return resting_grains


def solve_problem_part_two(terrain, start):
    """When do we cover up the hole?"""
    counter = 0
    while True:
        terrain, location = drop_sand_grain(terrain, start)
        if location==start:
            return counter + 1
        counter += 1


if __name__ == '__main__':
    EXAMPLE_INPUT = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""
    terrain = parse_input_string(EXAMPLE_INPUT)
    wall_text = parse_file()
    real_terrain = parse_input_string(wall_text)

    print(solve_problem_part_one())
    print(solve_problem_part_two(real_terrain, (500, 0)))
