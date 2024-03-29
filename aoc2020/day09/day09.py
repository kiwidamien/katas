from typing import List
from collections import deque


def find_first_invalid(stream: List[int], buffer_size:int) -> int:
    buffer = deque(stream[:buffer_size])
    for num in stream[buffer_size:]:
        buffered = set(buffer)
        if not any([num - elem in buffered for elem in buffered]):
            return num
        buffer.popleft()
        buffer.append(num)

def stream(filename: str):
    with open(filename) as f:
        stream = [int(num) for num in f.readlines()]
    return stream

def process(filename: str, buffer_size: int):
    my_stream = stream(filename)
    invalid_num = find_first_invalid(my_stream, buffer_size)
    large_sum_list = find_contiguous(my_stream, target=invalid_num)
    small,large = min(large_sum_list), max(large_sum_list)
    return (invalid_num, small+large)


def find_contiguous(stream: List[str], target: int) -> List[int]:
    """Find a continuous set of integers that sum to the target"""
    cumsum = [stream[0]]
    for num in stream[1:]:
        cumsum.append(cumsum[-1] + num)
    prev_seen = set()
    for index, num in enumerate(cumsum):
        if num - target in prev_seen:
            # Can do one slow traversal of the list
            index_start = cumsum.index(num-target)
            return stream[index_start+1:index+1]
        prev_seen.add(num)


def main():
    (example_ans, sum_pieces) = process("example.txt", 5)
    print("In example, first invalid number is", example_ans)
    print("            sum of smallest and largest in ctgs array that sum to the target is ", sum_pieces)
    (input_ans, sum_pieces) = process("input.txt", 25)
    print("In input, first invalid number is", input_ans)
    print("            sum of smallest and largest in ctgs array that sum to the target is ", sum_pieces)


if __name__ == '__main__':
    main()

