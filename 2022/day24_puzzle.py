from collections import namedtuple, deque
from dataclasses import dataclass

from typing import List, Tuple


Expedition = namedtuple("Expendition", "row col")
Blizzard = namedtuple("Blizzard", "row col direction")

_move_lookup = {
    '>': (0, 1),
    'v': (1, 0),
    '<': (0, -1),
    '^': (-1, 0)
}

@dataclass
class BoardState:
    expedition: Expedition
    blizzards: List[Blizzard]
    goal: Tuple[int, int]
    n_rows: int
    n_cols: int

    def at_loc(self, row, col) -> str:
        if self.expedition == (row, col):
            return 'E'
        if row == -1 and col == 0:
            return '.'
        if (row, col) == self.goal:
            return '.'
        if row < 0 or row >= self.n_rows:
            return '#'
        if col < 0 or col >= self.n_cols:
            return '#'
        blizzards = [b for b in self.blizzards if (b.row, b.col)==(row, col)]
        if len(blizzards)==0:
            return '.'
        if len(blizzards)>1:
            return str(len(blizzards))
        return blizzards[0].direction

    def __str__(self) -> str:
        grid = [
            ''.join([self.at_loc(row, col) for col in range(-1, self.n_cols+1)])
            for row in range(-1, self.n_rows+1)
        ]
        return '\n'.join(grid)


def parse_contents(contents: str):
    lines = contents.split('\n')
    n_cols = len(lines[0]) - 2
    n_rows = len(lines) - 2
    blizzards = []
    for row_num, row in enumerate(lines[1:-1]):
        for col_num, occ in enumerate(row[1:-1]):
            if occ == '.':
                continue
            assert occ in _move_lookup
            blizzards.append(Blizzard(row=row_num, col=col_num, direction=occ))
    expedition = (-1, 0)
    goal = (n_rows, n_cols-1)
    return BoardState(
        expedition=expedition,
        blizzards=blizzards,
        goal=goal,
        n_rows=n_rows,
        n_cols=n_cols
    )



def min_moves_to_exit(state) -> int:
    return 0


def move(blizzard: Blizzard, n_rows: int, n_cols: int) -> Blizzard:
    delta_y, delta_x = _move_lookup[blizzard.direction]
    new_row = (blizzard.row + delta_y) % n_rows
    new_col = (blizzard.col + delta_x) % n_cols
    return Blizzard(row=new_row, col=new_col, direction=blizzard.direction)


def update_all_blizzards(
    blizzards: List[Blizzard], n_rows: int, n_cols: int
) -> List[Blizzard]:
    return [move(blizzard, n_rows, n_cols) for blizzard in blizzards]


def new_board(board):
    return BoardState(
        expedition=board.expedition,
        blizzards = update_all_blizzards(board.blizzards, board.n_rows, board.n_cols),
        goal=board.goal,
        n_rows=board.n_rows,
        n_cols=board.n_cols
    )

def _find_path(
    ini_blizzards: List[Blizzard],
    ini_loc: Expedition,
    n_rows: int,
    n_cols: int,
    goal: Tuple[int, int]
) -> int:
    # Looking for shortest path, use BFS
    def legal_moves(loc, next_blizzards):
        poss_delta = [(0,0), (1,0), (-1, 0), (0, 1), (0, -1)]
        poss_locs = [(loc[0] + d[0], loc[1] + d[1]) for d in poss_delta]
        blizzard_locs = {(b.row, b.col) for b in next_blizzards}
        poss_locs = [pl for pl in poss_locs if pl not in blizzard_locs]
        if goal in poss_locs:
            return [goal]
        poss_locs = [
            pl for pl in poss_locs if (pl[0] >= 0 and pl[0] < n_rows
            and pl[1] >= 0 and pl[1] < n_cols) or pl==ini_loc
        ]
        return poss_locs

    def get_blizzard_step(step_num):
        if step_num not in blizzard_step:
            blizzard_step[step_num] = update_all_blizzards(
                blizzards=get_blizzard_step(step_num - 1),
                n_rows=n_rows, n_cols=n_cols,
            )
        return blizzard_step[step_num]

    queue = deque(([ini_loc, 0],))
    blizzard_step = {
        0: ini_blizzards,
    }
    while (queue):
        curr_loc, curr_step = queue.popleft()
        if curr_loc==goal:
            return curr_step
        these_blizzards = get_blizzard_step(curr_step+1)
        possible_locs = [
            (loc, curr_step+1) 
            for loc in legal_moves(curr_loc, these_blizzards)
            if (loc, curr_step+1) not in queue
        ]
        assert all(len(pl)==2 for pl in possible_locs)
        assert all(len(pl[0])==2 for pl in possible_locs)
        
        queue.extend(possible_locs)
    raise ValueError('Was trapped, impossible to find path')


def min_moves_to_exit(board):
    return _find_path(
        ini_blizzards= board.blizzards,
        ini_loc=board.expedition,
        n_rows=board.n_rows,
        n_cols=board.n_cols,
        goal=board.goal
    )

def parse_file():
    with open('day24_input.txt') as f:
        return parse_contents(f.read())

def problem1_min_steps_to_exit():
    board = parse_file()
    return min_moves_to_exit(board)

def multi_trip(board):
    moves_there = min_moves_to_exit(board)

    updated_blizzards = board.blizzards
    for _ in range(moves_there):
        updated_blizzards = update_all_blizzards(
            updated_blizzards,
            board.n_rows,
            board.n_cols
        )

    new_board = BoardState(
        expedition=board.goal,
        blizzards=updated_blizzards,
        goal=board.expedition,
        n_rows=board.n_rows,
        n_cols=board.n_cols
    )
    moves_back = min_moves_to_exit(new_board)

    updated_blizzards = new_board.blizzards
    for _ in range(moves_back):
        updated_blizzards = update_all_blizzards(
            updated_blizzards,
            board.n_rows,
            board.n_cols
        )

    final_board = BoardState(
        expedition=board.expedition,
        goal=board.goal,
        blizzards = updated_blizzards,
        n_rows=board.n_rows,
        n_cols=board.n_cols
    )

    final_moves = min_moves_to_exit(final_board)
    return [moves_there, moves_back, final_moves]


def problem2_there_and_back_again():
    board = parse_file()
    move_counts = multi_trip(board)
    print("Segments: ", move_counts)
    return sum(move_counts) 



if __name__ == '__main__':
    #print(f'It takes {problem1_min_steps_to_exit()} to get to the exit')
    print(f'And now for the back and forth ....')
    print(problem2_there_and_back_again())
