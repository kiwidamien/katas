import pytest 
import day05 as d


def test_example_part1():
    assert 35 == d.part1("example.txt")

def test_example_part2():
    assert 46 == d.part2("example.txt")

def test_input_part1():
    assert 261668924 == d.part1("input.txt")

def test_part_1_alternative():
    assert d.part1("example.txt") == d.part1_alt("example.txt")
    assert d.part1("input.txt") == d.part1_alt("input.txt")