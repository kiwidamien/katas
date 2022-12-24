import pytest
import sys
sys.path.append('..')

import day24_puzzle as d24


SIMPLE = """#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#"""


EXAMPLE = """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"""


def test_expected_min_moves():
    board = d24.parse_contents(SIMPLE)
    assert 10 == d24.min_moves_to_exit(board)

def test_expected_min_moves_on_documentation_example():
    board = d24.parse_contents(EXAMPLE)
    assert 18 == d24.min_moves_to_exit(board)

def test_multi_trip():
    board = d24.parse_contents(EXAMPLE)
    assert [18, 23, 13] == d24.multi_trip(board)
