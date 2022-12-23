import pytest
import sys
sys.path.append('..')

import day22_puzzle as d22


EXAMPLE="""        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"""


@pytest.fixture()
def floorplan():
    fp, _ = d22.parse_contents(EXAMPLE)
    return fp


@pytest.fixture()
def moves():
    _, moves = d22.parse_contents(EXAMPLE)
    return moves


def test_moves(moves):
    assert moves == [10, 'R', 5, 'L', 5, 'R', 10, 'L', 4, 'R', 5, 'L', 5]


def test_starting_location(floorplan):
    state = d22.starting_state(floorplan)
    assert state.row == 1
    assert state.col == 9
    assert state.direction == 0


def test_final_location_example(floorplan, moves):
    expected_row, expected_col, expected_direction = 6, 8, 0
    state = d22.walk(floorplan, moves, starting_loc=d22.State(row=1, col=9, direction=0))
    assert expected_row == state.row
    assert expected_col == state.col
    assert expected_direction == state.direction

