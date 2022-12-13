"""
Input: A multiline string of the form
   Sabqponm
   abcryxxl
   accszExk
   acctuvwj
   abdefghi

Here there are different levels of the ground (a=0, b=1, ..., z=25)
and 
  S: Starting location (at level 'a')
  E: Ending location (at leevl 'z')
You can move up/down/left/right, but can only make transitions
  - at most one level higher than your current position (e.g. b->c, m->n)
  - any levels below you (e.g. n -> b)
Transititions like a->c are forbidden (two levels)


Solutions from the description is the following path:
    v..v<<<<
    >v.vv<<^
    .>vv>E^^
    ..v>>>^^
    ..>>>>>^

We start at S (at level a) and end at E (at level z), The levels are
lexographically ordered (z > y > x > ... > a) and at each step you can
go up at most one level but down as many as you like.

Shown here is the shortest path from S to E that respects the constraint.
The length of this path is 31
"""

from typing import List, Tuple, Dict


Terrain = List[List[str]]
Location = Tuple[int, int]


def parse_input(contents: str) -> Terrain:
    return [list(s) for s in contents.split()]


def numeric_level(row, col, terrain) -> int:
    level = terrain[row][col]
    if level == 'S': return 0
    if level == 'E': return 25
    return ord(level) - ord('a')


def _possible_transitions(row: int, col: int, terrain: Terrain) -> List[Location]:
    level = numeric_level(row, col, terrain)
    moves = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    neighbors = [(row + r, col + c) for r,c in moves]
    valid = [(r, c) for r, c in neighbors 
             if r>=0 and r<len(terrain) and 
                c >= 0 and c<len(terrain[0]) and 
             numeric_level(r, c, terrain) <= level+1]
    return valid


def _scan_locations(terrain: Terrain):
    for row in range(len(terrain)):
        for col in range(len(terrain[row])):
            yield (row, col)


def _find(character: str, terrain: Terrain) -> Location:
    for location in _scan_locations(terrain):
        if terrain[location[0]][location[1]] == character:
            return location
    raise ValueError(f'No {character} on terrain')


def find_start(terrain: Terrain) -> Location:
    return _find('S', terrain)


def find_end(terrain: Terrain) -> Location:
    return _find('E', terrain)


def _reverse_transitions(terrain: Terrain) -> Dict[Location, List[Terrain]]:
    reverse = {}
    for loc in _scan_locations(terrain):
        neighbors = _possible_transitions(*loc, terrain)
        for n in neighbors:
            if n not in reverse:
                reverse[n] = [loc]
            else:
                reverse[n].append(loc)
    return reverse


def _shortest_path_length(dst: Location, terrain: Terrain):
    """Simple BFS"""
    starts = _reverse_transitions(terrain)
    queue = [(dst, 0)]
    visited = {}

    while queue:
        current, dist_so_far = queue.pop()
        if current in visited:
            continue
        visited[current] = dist_so_far
        queue = [(head, dist_so_far + 1) for head in starts.get(current, []) if head not in visited] + queue
    return visited


def shortest_path_length(terrain: Terrain) -> int:
    """Finds the shortest path between S and E"""
    src = find_start(terrain)
    dst = find_end(terrain)
    return _shortest_path_length(dst=dst, terrain=terrain)[src]


def shortest_path_from_any_a_level_point(terrain: Terrain) -> int:
    """Finds the shorest path between and lowest level (S or 'a') and E"""
    dst = find_end(terrain)
    distances = _shortest_path_length(dst=dst, terrain=terrain)
    low_levels = [
        distance for loc, distance in distances.items()
        if terrain[loc[0]][loc[1]]=='a' or terrain[loc[0]][loc[1]]=='S'
    ]
    return min(low_levels)


# Read in the puzzle for the day
def parse_file():
    with open('day12_input.txt', 'r') as f:
        contents = f.read()
    return contents


if __name__ == '__main__':
    terrain = parse_input(parse_file())
    print(shortest_path_length(terrain))
    print(shortest_path_from_any_a_level_point(terrain))
