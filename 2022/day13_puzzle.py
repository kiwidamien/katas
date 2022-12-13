from typing import List, Optional, Union, Tuple
import functools


Signal = Union[int, List]


def _cmp_integers(left: int, right: int) -> Optional[bool]:
    if left==right:
        return None
    return (left < right)


def _cmp_lists(left: List, right: List) -> Optional[bool]:
    default = _cmp_integers(len(left), len(right))
    for l_elem, r_elem in zip(left, right):
        result = _cmp(l_elem, r_elem)
        if result is not None:
            return result
    return default


def _cmp_exactly_one_list(left, right) -> Optional[bool]:
    left_is_list = isinstance(left, list)
    right_is_list = isinstance(right, list)
    if left_is_list == right_is_list:
        raise ValueError("Exactly one of these should be a list!")
    if left_is_list:
        return _cmp_lists(left, [right])
    return _cmp_lists([left], right)


def _cmp(left: Union[int, List], right: Union[int, List]) -> Optional[bool]:
    if isinstance(left, int) and isinstance(right, int):
        return _cmp_integers(left, right)
    if isinstance(left, list) and isinstance(right, list):
        return _cmp_lists(left, right)
    if isinstance(left, list) or isinstance(right, list):
        return _cmp_exactly_one_list(left, right)
    raise ValueError("Don't know how to do comparison")


def is_ordered(left, right) -> bool:
    result = _cmp(left, right)
    if result is None:
        return True
    return result

# Do comparison
def parse_file():
    with open('day13_input.txt', 'r') as f:
        contents = f.read()
    return string_to_pairs(contents)


def string_to_pairs(contents: str):
    contents = [eval(line) for line in contents.split("\n") if line.strip()]
    pairs = []
    for index in range(0, len(contents), 2):
        pairs.append((contents[index], contents[index+1]))
    return pairs

def sum_the_indicies(pairs: List[Tuple[Signal, Signal]]) -> int:
    """Return the sum of the (1-based) indicies of the pair of signals that appear in order.

    Solution to part 1.
    """
    success_indices = [1+index for index, pair in enumerate(pairs)
                       if is_ordered(*pair)]
    return sum(success_indices)


def order_all_indices(signals: List[Signal]):
    def cmp_func(a,b):
        return 2*is_ordered(a,b) - 1

    return sorted(signals, key=functools.cmp_to_key(cmp_func), reverse=True)


def order_all_signals_and_add_breakers(pairs: List[Tuple[Signal, Signal]]) -> List[Signal]:
    """As given in the problem, sorts all signals (ignoring the pairing) as well as two signal breakers [[2]] and [[6]]

    Returns an ordered list of signals
    """
    flattened = sum([list(pair) for pair in pairs], []) + [[[2]], [[6]]]
    return order_all_indices(flattened)


def product_of_breaker_locations(signals: List[Signal]):
    """Returns the product of the (1-based) indicies of the breakers, [[2]] and [[6]]"""
    index1 = 1 + signals.index([[2]])
    index2 = 1 + signals.index([[6]])
    return index1 * index2


if __name__=='__main__':
    pairs = parse_file()
    print(sum_the_indicies(pairs))
    ordered_with_breakers = order_all_signals_and_add_breakers(pairs)
    #print("\n".join([str(p) for p in ordered_with_breakers]))
    print(product_of_breaker_locations(ordered_with_breakers))
