import pytest
import sys
sys.path.append('..')

import day20_puzzle as d20


EXAMPLE = """1
2
-3
3
-2
0
4"""


@pytest.fixture()
def example():
    return d20.process_string(EXAMPLE)


def test_example_initial(example):
    assert example==[1,2,-3,3,-2,0,4]


def test_mixing_example(example):
    expected = [0, 3, -2, 1, 2, -3, 4]
    assert expected==d20.mix_from_zero(example)

def test_get_coords(example):
    expected = [4, -3, 2]
    assert expected == d20.get_coords(example)
