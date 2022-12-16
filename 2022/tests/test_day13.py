import pytest
import sys
sys.path.append('..')
import day13_puzzle as day13


EXAMPLE = """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""


@pytest.mark.parametrize(
    "left,right,is_ordered",[
        [[1,1,3,1,1], [1,1,5,1,1], True],
        [[[1],[2,3,4]],[[1],4], True],
        [[9], [8,7,6], False],
        [[[4,4],4,4], [[4,4],4,4,4], True],
        [[7,7,7,7], [7,7,7], False],
        [[], [3], True],
        [[[[]]],[[]], False],
        [[1,[2,[3,[4,[5,6,7]]]],8,9], [1,[2,[3,[4,[5,6,0]]]],8,9], False]
    ])
def test_is_ordered(left, right, is_ordered):
    assert day13.is_ordered(left, right)==is_ordered


def test_sum_indices():
    expected = 13
    pairs = day13.string_to_pairs(EXAMPLE)
    assert day13.sum_the_indicies(pairs)==expected


def test_product_of_breakers():
    expected = 140
    pairs = day13.string_to_pairs(EXAMPLE)
    flattened = day13.order_all_signals_and_add_breakers(pairs)
    assert day13.product_of_breaker_locations(flattened)==expected


def test_print_sort():
    pairs = day13.string_to_pairs(EXAMPLE)
    flattened = day13.order_all_signals_and_add_breakers(pairs)
    visualization = "\n".join(str(x) for x in flattened)
    assert visualization != ""

