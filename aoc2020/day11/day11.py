from typing import List, Dict
import fire 


def count_neighbors(grid: List[List[str]], row, col) -> int:
    total = 0
    for r in [row - 1, row, row+1]:
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

def fixed_point(grid: List[List[str]]):
    old_grid = None
    while old_grid != grid:
        old_grid = [row[:] for row in grid]
        grid = update(grid)
    return grid


def render(g: List[List[str]]) -> str:
    return "\n".join([''.join(line) for line in g])


def main(filename: str="input.txt"):
    with open(filename) as f:
        g = [line.strip() for line in f.readlines()]
    terminal = fixed_point(g)
    final_map = render(terminal)
    print(final_map)
    print(sum([c == '#' for c in final_map]))

## Part 2

def build_adjacency(g: List[List[str]]) -> Dict[Tuple[int, int], List[Tuple[int, int]]]:
    for r, row in enumerate(g):
        for c, char in enumerate(row):
            if c == '.':
                continue
            for offset_minus_one, nc in enumerate(row[c+1:]):
                if nc != '.':
                    neighbor[(r, c)] = neighbor.get((r, c), []).append((r, c+1+offset_minus_one))
                    break
            for index, nc in enumerate(row[:c]):
                if nc != '.':
                    neighbor[(r, c)] = neighbor.get((r,c), []).append((r, index))

if __name__=='__main__':
    fire.Fire(main)
