import pytest
import sys
sys.path.append('..')

import day19_puzzle as d19


EXAMPLE = (
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n"
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
)


def test_parsing_line1():
    expected = d19.Blueprint(id=1, ore=(4,0,0), clay=(2,0,0), obsidian=(3,14,0), geode=(2,0,7))
    assert expected == d19.parse_line(EXAMPLE.split('\n')[0])


def test_parsing_line2():
    expected = d19.Blueprint(id=2, ore=(2,0,0), clay=(3,0,0), obsidian=(3,8,0), geode=(3,0,12))
    assert expected == d19.parse_line(EXAMPLE.split('\n')[1])


@pytest.mark.slow
def test_largest_numer_of_geodes():
    expected = 9
    blueprint = d19.parse_line(EXAMPLE.split('\n')[0])
    assert expected == d19.search(blueprint=blueprint, time=24)


@pytest.mark.slow
def test_largest_number_of_geodes_two():
    expected = 12
    blueprint = d19.parse_line(EXAMPLE.split('\n')[1])
    assert expected == d19.search(blueprint=blueprint, time=24)


@pytest.mark.slow
def test_largest_number_of_geodes_long():
    expected = 56
    blueprint = d19.parse_line(EXAMPLE.split('\n')[0])
    assert expected == d19.search(blueprint=blueprint, time=32)


@pytest.mark.slow
def test_largest_number_of_geodes_long_2():
    blueprint = d19.parse_line(EXAMPLE.split('\n')[1])
    assert 62 == d19.search(blueprint=blueprint, time=32)
