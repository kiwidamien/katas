def paper_locations(grid: list[list[str]]) -> set((int, int)):
    locations = {
        (x, y) 
        for y, line in enumerate(grid)
        for x, char in enumerate(line)
        if char == '@'
    }
    return locations


def count_neighbors(loc_grid: set((int, int))) -> dict[(int,int), int]:
    neighbors = dict()
    for (x, y) in loc_grid:
        neighbors[(x,y)] = 0
        for deltaX in [-1, 0, 1]:
            for deltaY in [-1, 0, 1]:
                if (deltaX, deltaY) == (0, 0):
                    continue
                if (x + deltaX, y+deltaY) in loc_grid:
                    neighbors[(x,y)] += 1
    return neighbors


def get_fewer_than_n(neighbor_count: dict[(int, int), int], amount: int):
    return {
        k: v
        for k, v in neighbor_count.items()
        if v < amount
    }


def iterate(loc_grid: set((int, int))) -> set((int, int)):
    neighbors = count_neighbors(loc_grid=loc_grid)
    filtered = {
        k: v
        for k, v in neighbors.items()
        if v >= 4
    }
    return set(filtered.keys())


def fixed_point(loc_grid: set((int, int))) -> set((int, int)):
    new_loc_grid = iterate(loc_grid)
    if new_loc_grid==loc_grid:
        return loc_grid
    return fixed_point(new_loc_grid)


def part_one(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f.readlines()]
    loc = paper_locations(lines)
    counter = count_neighbors(loc)
    filtered = get_fewer_than_n(counter, amount=4)
    return len(filtered)


def part_two(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f.readlines()]
    loc = paper_locations(lines)
    final = fixed_point(loc)
    return len(loc) - len(final)
