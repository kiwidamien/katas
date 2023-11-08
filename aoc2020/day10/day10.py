import fire
from typing import List, Dict


def countJumps(adapters: List[int]) -> Dict[int, int]:
    sorted_adapters = sorted(adapters)
    differences = [
        next - this for this, next in zip(sorted_adapters, sorted_adapters[1:])
    ]
    counts = {}
    for d in differences:
        counts[d] = counts.get(d, 0) + 1
    return counts


def scoreJumps(counts: Dict[int, int]) -> int:
    return counts.get(3, 0) * counts.get(1, 0)


def part1(adapters: List[int]) -> int:
    biggest = max(adapters)
    with_outlet = [0] + adapters + [biggest + 3]
    return scoreJumps(countJumps(with_outlet))


def _countTheWays(adapters: List[int], target: int) -> Dict[int, Dict[int, int]]:
    """Intermediate step for counting the # ways we the adapters combine to make target

    Uses the constraint that we can only chain adapters that are at most 3
    apart (e.g. a 1 -> 4 -> 5 -> 6 is a valid chain, but 1 -> 6 is not, because
    the gap from 1 to 6 is too large)

    The table is a representation
      res[Adapter] = {total: num_ways_constrained}
    Here num_ways_constrained is
      * The number of ways of reaching total
      * Where you MUST use `Adapter`
      * You CAN use any adapter a, where a < Adapter
    This means that different rows can be summed, because they MUST have the largest adapter
    and the previous rows won't have it. Note this assumes all adapter values are distinct.
    """

    cnt_partition = {}
    sort_adapters = sorted(adapters)
    for adapter_to_include in sort_adapters:
        tmp = {adapter_to_include: 1}
        for total in range(adapter_to_include + 1, target + 1):
            delta = 0
            for other_adapters in cnt_partition:
                if not (other_adapters < adapter_to_include <= other_adapters + 3):
                    continue
                delta += cnt_partition[other_adapters].get(
                    total - adapter_to_include, 0
                )
            tmp[total] = delta
        cnt_partition[adapter_to_include] = tmp
    return cnt_partition


def countTheWays(adapters: List[int], target: int = None) -> int:
    if target is None:
        target = max(adapters) + 3
    tbl = _countTheWays(adapters=adapters, target=target)
    total = 0
    for largest in tbl:
        total += tbl[largest].get(target, 0)
    return total


def count_configurations(adapters: List[int]) -> int:
    target = max(adapters)
    cnt = {0: 1}
    for adapter in sorted(adapters):
        cnt[adapter] = sum(cnt.get(adapter - j, 0) for j in [1, 2, 3])
    return cnt.get(target, 0)


def main(filename: str):
    with open(filename) as f:
        adapters = [int(n) for n in f.readlines()]
    print(part1(adapters))
    print(countTheWays(adapters, target=max(adapters) + 3))
    print(count_configurations(adapters))


if __name__ == "__main__":
    fire.Fire(main)
