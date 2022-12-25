import pytest
import sys

sys.path.append('..')

import day25_puzzle as d25

cases = [ 
    [1, "1"],
    [2, "2"],
    [3, "1="],
    [4, "1-"],
    [5, "10"],
    [6, "11"],
    [7, "12"],
    [8, "2="],
    [9, "2-"],
    [10, "20"],
    [15, "1=0"],
    [20, "1-0"],
    [2022, "1=11-2"],
    [12345, "1-0---0"],
    [314159265, "1121-1110-1=0"]
]

EXAMPLE="""1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"""


@pytest.mark.parametrize("decimal,snafu",cases)
def test_decimal_to_snafu(decimal, snafu):
    assert d25.decimal_to_snafu(decimal)==snafu


@pytest.mark.parametrize("decimal,snafu", cases)
def test_snafu_to_decimal(decimal, snafu):
    assert d25.snafu_to_decimal(snafu)==decimal


def test_example_part_1():
    lines = EXAMPLE.split('\n')
    total_fuel = sum([d25.snafu_to_decimal(n) for n in lines])
    assert total_fuel == 4890
    assert d25.decimal_to_snafu(total_fuel) == '2=-1=0'
