import pytest
import sys
sys.path.append("..")
import day12_puzzle as day12


EXAMPLE = """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""

"""Solutions from the description is the following path:
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


def test_length_of_shortest_path_in_example():
    terrain = day12.parse_input(EXAMPLE)
    assert day12.shortest_path_length(terrain)==31


def test_length_of_shortest_path_from_any_low_point():
    terrain = day12.parse_input(EXAMPLE)
    assert day12.shortest_path_from_any_a_level_point(terrain)==29


def test_trivial_shortest_length():
    terrain = day12.parse_input("SbcdefghijklmnopqrstuvwxyE")
    assert day12.shortest_path_length(terrain)==25


@pytest.mark.parametrize("row,col,expected",
        [
            [0, 0, [(0, 1), (1, 0)]],
            [2, 1, [(2, 0), (3, 1),  (2,2), (1, 1)]],
            [3, 0, [(2, 0), (4, 0)]],
            [3, 4, [(3, 5), (4, 4), (3, 3)]]
        ]
)
def test_possible_transitions_from_corner(row, col, expected):
    terrain = day12.parse_input(EXAMPLE)
    assert set(day12._possible_transitions(row, col, terrain))==set(expected)
