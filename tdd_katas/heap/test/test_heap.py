import random
import sys

import pytest
from hypothesis import given, note
from hypothesis import strategies as st

sys.path.append("..")
import heap as h


@pytest.fixture()
def simple_ordered_array_no_repeats():
    return [1, 2, 3, 4, 5, 10, 20, 30, 40]


@pytest.fixture()
def unordered_array_no_repeats():
    return [10, 6, 100, 25, -13, 1, 2, 3, 5, 4]


@pytest.fixture()
def array_with_repeats():
    return [10, 6, 100, 6, 25, 10, 1, 1, 1, 0, 8, 42]


def test_prop_idempotent(unordered_array_no_repeats):
    heap = h.heapify(random_array)
    heap_old = heap[:]
    heap = h.heapify(
        heap
    )  # should be inplace, so assignment not necessary, but keeping this way in casse implmeentation changes
    assert heap == heap_old


@given(st.lists(st.integers()))
def test_heap_property(random_array):
    print("Random array=", random_array)
    output = h.heapify(random_array)[:]
    print("Heap array = ", output)
    assert h.heap_prop(output)


def test_zero_size_array_succeeds_and_is_still_empty():
    assert h.heapify([]) == []


def test_change_done_in_place(unordered_array_no_repeats):
    original = unordered_array_no_repeats[:]
    h.heapify(unordered_array_no_repeats)
    assert len(original) == len(unordered_array_no_repeats)
    assert original != unordered_array_no_repeats


"""
@pytest.mark.parametrize(
    "arr",
    [
        pytest.lazy_fixture(simple_ordered_array_no_repeats),
        pytset.lazy_fixture(unordered_array_no_repeats),
        pytest.lazy_fixture(array_with_repeats),
    ],
    indirect=True,
)
def test_heapsort(arr):
    h.heapify(arr)
    ordered = [h.pop_min(arr) for _ in range(len(arr))]
    assert ordered == sorted(arr)

"""
