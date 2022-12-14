import pytest
import sys
sys.path.append('..')

import day14_puzzle as day14


EXAMPLE_INPUT = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""


@pytest.fixture()
def example_terrain():
    return day14.parse_input_string(EXAMPLE_INPUT)


def test_lowest_row(example_terrain):
    assert day14.lowest_row(example_terrain) == 10


def test_final_location_of_first_grain(example_terrain):
    assert day14.find_final_location(example_terrain, (500, 0)) == (500, 8)


def test_number_of_grains_come_to_rest(example_terrain):
    assert day14.number_of_grains_come_to_rest(example_terrain, (500,0))==24


def test_example_of_blocking_hole(example_terrain):
    assert day14.solve_problem_part_two(example_terrain, (500, 0))==93
