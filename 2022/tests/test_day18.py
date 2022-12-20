import pytest
import sys
sys.path.append('..')

import day18_puzzle as d18

EXAMPLE_INPUT="""2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5""".split('\n')


@pytest.fixture()
def example_points():
    return [[int(coord) for coord in line.split(',')] for line in EXAMPLE_INPUT]


def test_single_cube():
    area = d18.get_area([[1,1,1]])
    assert area == 6


@pytest.mark.parametrize(
    "cube1,cube2",[
        [[1,1,1], [2,1,1]],
        [[1,1,1], [0,1,1]],
        [[1,1,1], [1,2,1]],
        [[1,1,1], [1,1,2]]
    ])
def test_two_touching_cubes(cube1, cube2):
    area = d18.get_area([cube1, cube2])
    assert area == 10


def test_two_nontouching_cubes():
    area = d18.get_area([[1,1,1], [1,2,2]])
    assert area == 12


def test_example_area(example_points):
    area = d18.get_area(example_points)
    assert area==64


def test_example_exterior_area(example_points):
    exterior_area = d18.get_exterior_area(example_points)
    assert exterior_area == 58


def test_example_num_interior_points(example_points):
    interior_points = d18.find_interior_points(example_points)
    assert len(interior_points)==1


def test_example_num_interior_points_single_cube():
    interior_points = d18.find_interior_points([[1,1,1]])
    assert len(interior_points) == 0


