import pytest
import sys
sys.path.append('..')

import day17_puzzle as d17

jets = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"


EXAMPLE_MAP = """|....@..|
|....@..|
|..@@@..|
|.......|
|.......|
|.......|
|...#...|
|..###..|
|...#...|
|..####.|
---------"""

def test_map():
    map_of_cave = d17.get_map_of_cave(jets=jets, n_rocks=2)
    assert map_of_cave=='\n'.join(EXAMPLE_MAP.split('\n')[-5:])


def test_height_of_falls_after_two_rocks():
    height_of_tower = d17.height_of_tower(jets=jets, n_rocks=2)
    assert height_of_tower==4


@pytest.mark.slow
def test_height_of_tower_after_2022_rocks():
    height_of_tower = d17.height_of_tower(jets=jets, n_rocks=2022)
    assert height_of_tower==3068


@pytest.mark.slow
def test_height_of_tower_after_2022_rocks_using_many():
    height_of_tower = d17.height_of_tower(jets=jets, n_rocks=2022)
    assert height_of_tower==3068


def test_height_of_tower_after_1000000000000_rocks():
    n_rocks = 1000000000000 
    height_of_tower = d17.height_of_tower_many(jets=jets, n_rocks=n_rocks)
    assert height_of_tower==1_514_285_714_288
