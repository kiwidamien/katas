"""
Generate a sequence

1. Seed with an initial sequence (input)
2. After seed, consider the last number in sequence.
   - If it is the first time the number appeared, generate a 0
   - If the number has appeared more than once, generate the difference in indices
     for that number

    Example:
    1. Seed [0, 3, 6]
    2. Last number was a 6 (first time), generate 0
       Sequence now [0, 3, 6, 0]
    3. Last number was a 0 (second time). Was at index 0 and index 3. Generate 3-0=3
       Sequence now [0, 3, 6, 0, 3]
    4. Last number was a 3. Was at index 1 and index 4. Generate 4-1=3
       Sequence now [0, 3, 6, 0, 3, 3]
    5. Last number was a 3, at index 5 and 4. Generate a 1
       Sequence now [0, 3, 6, 0, 3, 3, 1]
"""
from typing import List


def seq(seed: List[int]):
    def add_num_to_tracker(idx, n):
        if n not in tracker:
            tracker[n] = [idx, None]
            return
        tracker[n][1] = tracker[n][0]
        tracker[n][0] = idx
        return

    tracker = {}
    for index, num in enumerate(seed):
        add_num_to_tracker(index, num)
        yield num
    while True:
        index += 1
        previous = tracker[num]
        num = 0 if previous[-1] is None else previous[0] - previous[1]
        add_num_to_tracker(index, num)
        yield num


def extract_index(seed: List[int], index: int) -> int:
    s = seq(seed)
    for _, n in zip(range(index), s):
        pass
    return n


if __name__ == "__main__":
    seed = [2, 20, 0, 4, 1, 17]
    lst = [s for s, _ in zip(seq(seed), range(2020))]
    print(lst[-1])
