import pytest
import sys
sys.path.append('..')

import day23_puzzle as d23

EXAMPLE1 = """.....
..##.
..#..
.....
..##.
....."""

EXAMPLE2 = """..............
..............
.......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......
..............
..............
.............."""

@pytest.fixture()
def small_input():
    return d23.parse_contents(EXAMPLE1)

@pytest.fixture()
def large_input():
    return d23.parse_contents(EXAMPLE2)

def test_small_example_reaches_steady_state_on_turn_3(small_input):
    mover = d23.mover(small_input, 'NSWE')
    fields = [field for index, field in zip(range(4), mover)]
    assert fields[0] != fields[1]
    assert fields[1] != fields[2]
    assert fields[2] == fields[3]


def test_area_claim(large_input):
    for step, f in zip(range(10), d23.mover(large_input, 'NSWE')):
        pass
    assert d23.empty_area(f) == 110


def test_steady_state_claim(large_input):
    assert d23.find_steady_state(large_input, 'NSWE') == 20


