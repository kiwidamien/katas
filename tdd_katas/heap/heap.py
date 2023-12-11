"""
Implementation of a min-heap

Uses a complete binary tree in an array
"""


def heap_prop(a):
    for idx in range(len(a)):
        c_idx = [c for c in _find_children(idx) if c < len(a)]
        if any([a[c] < a[idx] for c in c_idx]):
            print(
                f"Violation at {idx}: parent is {a[idx]}, children are {[a[c] for c in c_idx]}"
            )
            return False
    return True


def get_min(heap: list):
    return heap[0]


def size(heap: list):
    return len(heap)


def _find_parent(idx: int) -> int:
    return (idx - 1) // 2


def _find_children(idx: int) -> (int, int):
    return (2 * idx + 1, 2 * idx + 2)


def _mut_swap_with_parent(lst: list, idx: int) -> list:
    parent_idx = _find_parent(idx)
    lst[parent_idx], lst[idx] = lst[idx], lst[parent_idx]
    return lst


def _bubble_up(lst: list, idx: int) -> list:
    # do we satisfy the invariant?
    if idx == 0:
        return lst
    parent_idx = _find_parent(idx)
    if lst[parent_idx] <= lst[idx]:
        return lst
    _mut_swap_with_parent(lst, idx)
    _sift_down(lst, idx)
    return _bubble_up(lst, parent_idx)


def _sift_down(lst: list, idx: int) -> list:
    """Assumes that the children of idx both are roots of valid minheaps
    But that the value at idx might violate the heap property. Mutates the heap
    so that we restore the heap property"""
    # base case -- heap property already established (inc if this is a leaf)
    child_idx = _find_children(idx)
    child_values = [lst[c] for c in child_idx if c < len(lst)]
    if all([c >= lst[idx] for c in child_values]):
        return lst
    if len(child_values) == 1:
        lst[idx], lst[child_idx[0]] = lst[child_idx[0]], lst[idx]
        return lst
    if child_values[0] < child_values[1]:
        swap_index = child_idx[0]
    else:
        swap_index = child_idx[1]
    lst[idx], lst[swap_index] = lst[swap_index], lst[idx]
    return _sift_down(lst, swap_index)


def insert(heap: list, value) -> list:
    """Place a new element into a (new) heap

    Because we copy the elements, this is O(n) [but the copy should be quick!]
    The insertion stage is O(log n), one for each level
    """
    new_heap = heap[:]
    new_heap.append(value)
    return _bubble_up(new_heap, len(new_heap) - 1)


def pop_min(heap: list):
    value = heap[0]
    heap[0] = heap[-1]
    heap.pop()
    _sift_down(heap, 0)
    return value


def heapify(lst: list) -> list:
    for i in reversed(range(len(lst))):
        if i == 0:
            continue
        parent = _find_parent(i)
        if lst[i] < lst[parent]:
            _mut_swap_with_parent(lst, i)
    return lst
