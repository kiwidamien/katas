from pprint import pprint as print

Loc = (int, int)
State = (Loc, str, int)


def neighbors(grid: list[list[int]], loc: Loc, curr_dir: str) -> list[(Loc, str)]:
    y, x = loc 
    results = [
        ((y-1, x), 'N'), ((y, x+1), 'E'), ((y, x-1), 'W'), (((y+1, x), 'S'))
    ]
    opp = {
        'N': 'S', 'E':'W', 'S': 'N', 'W':'E'
    }
    return list(filter(lambda state: (0 <= state[0][0] < len(grid)) and (0 <= state[0][1] < len(grid[0])) and state[1] != opp[curr_dir], results))

def update(candidate_weight: int, state: State, to_visit: list[(int, State)]):
    for index, (w,s) in enumerate(to_visit):
        if s == state:
            to_visit[index] = (min(w, candidate_weight), s)
            return
    to_visit.append((candidate_weight, state))

def shortest_path(grid: list[list[int]], start: Loc, end: Loc, min_straight: int=0, max_straight: int=3) -> int:
    to_visit = [(0, (start, d, 0)) for d in 'NEWS']
    finalized = {}
    
    while to_visit:
        (c_dist, current) = to_visit.pop()
        (c_loc, c_dir, c_times) = current
        finalized[current] = c_dist
        for (loc, d) in neighbors(grid, c_loc, c_dir):
            if c_times < min_straight and (c_dir != d):
                continue
            visit_num = c_times + 1 if c_dir == d else 1
            if visit_num > (max_straight):
                continue
            state = (loc, d, visit_num)
            if state in finalized:
                continue
            update(c_dist + grid[loc[0]][loc[1]], state, to_visit)
        to_visit = sorted(to_visit, reverse=True)
    
    states = [v for k, v in finalized.items() if (k[0] == end) and (k[2] >= min_straight)]
    return min(states)

def crossing_path(grid: list[list[int]], min_straight: int=0, max_straight: int=3) -> int:
    last_pos = (len(grid) - 1, len(grid[0]) - 1)
    return shortest_path(grid, (0,0), last_pos, min_straight=min_straight, max_straight=max_straight)

def read_grid(filename: str) -> [[int]]:
    with open(filename) as f:
        contents = f.read()
    return [[int(c) for c in line] for line in contents.split()]


def part1(filename):
    return crossing_path(read_grid(filename))

def part2(filename):
    return crossing_path(read_grid(filename), min_straight=4, max_straight=10)
