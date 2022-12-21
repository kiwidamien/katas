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


def test_wrap_around():
    example = [0, 2, 1]
    # Step 1: [0, 2, 1]
    # Step 2: [0, 2, 1]
    # Step 3: [0, 1, 2] --> [0, 1, 2]
    assert [0, 1, 2] == d20.mix_from_zero(example)


def test_decryption_with_mode(example):
    expected = [0, -2434767459, 1623178306, 3246356612, -1623178306, 2434767459, 811589153]
    assert expected == d20.coord_with_decryption(example)
