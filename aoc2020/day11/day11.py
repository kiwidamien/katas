from typing import List, Dict, Tuple
import fire


def count_neighbors(grid: List[List[str]], row, col) -> int:
    total = 0
    for r in [row - 1, row, row + 1]:
        for c in [col - 1, col, col + 1]:
            if row == r and col == c:
                continue
            if r < 0 or r >= len(grid):
                continue
            if c < 0 or c >= len(grid[0]):
                continue
            total += grid[r][c] == "#"
    return total


def update(grid: List[List[str]]):
    def update_loc(g, r, c):
        if g[r][c] == ".":
            return "."
        count = count_neighbors(g, r, c)
        if g[r][c] == "L" and count == 0:
            return "#"
        if g[r][c] == "#" and count >= 4:
            return "L"
        return g[r][c]

    return [
        [update_loc(grid, row, col) for col in range(len(grid[0]))]
        for row in range(len(grid))
    ]


def fixed_point(grid: List[List[str]], update_rule):
    old_grid = None
    while old_grid != grid:
        old_grid = [row[:] for row in grid]
        grid = update_rule(grid)
    return grid


def render(g: List[List[str]]) -> str:
    return "\n".join(["".join(line) for line in g])


def count_seats(g: List[List[str]]) -> int:
    return sum([c == "#" for c in render(g)])


def main(filename: str = "input.txt"):
    with open(filename) as f:
        g = [line.strip() for line in f.readlines()]
    terminal = fixed_point(g, update)
    print(count_seats(terminal))

    terminal_part2 = fixed_point2(g)
    print(count_seats(terminal_part2))


## Part 2


def build_adjacency(g: List[List[str]]) -> Dict[Tuple[int, int], List[Tuple[int, int]]]:
    neighbor = {}

    def find_non_floor(loc, step, grid=g):
        r, c = loc
        dy, dx = step
        for num in range(1, len(grid) + 1):
            nr = r + num * dy
            nc = c + num * dx
            if nr < 0 or nr >= len(g):
                return None
            if nc < 0 or nc >= len(g[0]):
                return None
            if g[nr][nc] != ".":
                return (nr, nc)

    for r, row in enumerate(g):
        for c, char in enumerate(row):
            for step in [
                (-1, -1),
                (-1, 0),
                (-1, 1),
                (0, -1),
                (0, 1),
                (1, -1),
                (1, 0),
                (1, 1),
            ]:
                maybe_adj = find_non_floor((r, c), step)
                if maybe_adj is not None:
                    neighbor[(r, c)] = neighbor.get((r, c), []) + [maybe_adj]
    return neighbor


def update_part2(
    g: List[List[str]], adj: Dict[Tuple[int, int], List[Tuple[int, int]]] = None
) -> List[List[str]]:
    if adj is None:
        adj = build_adjacency(g)

    def update_loc(g, r, c):
        if g[r][c] == ".":
            return "."
        count = 0
        for ny, nx in adj.get((r, c), []):
            count += g[ny][nx] == "#"
        if g[r][c] == "L" and count == 0:
            return "#"
        if g[r][c] == "#" and count >= 5:
            return "L"
        return g[r][c]

    return [
        [
            update_loc(
                g,
                r,
                c,
            )
            for c in range(len(g[0]))
        ]
        for r in range(len(g))
    ]


def fixed_point2(g: List[List[str]]) -> List[List[str]]:
    adj = build_adjacency(g)
    old = None
    while g != old:
        old = [row[:] for row in g]
        g = update_part2(g, adj)
    return g


if __name__ == "__main__":
    fire.Fire(main)
