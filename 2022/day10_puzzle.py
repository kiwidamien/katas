from typing import List

stack = []

def noop(stack: List[int]) -> List[int]:
    if len(stack) == 0: return [1]
    return stack + [stack[-1]]


def addx(stack: List[int], value: int) -> List[int]:
    if len(stack) ==0: return [1, 1+value]
    return stack + [stack[-1], stack[-1] + value]


def update(stack, line: str)->List[int]:
    line = line.strip()
    if line == 'noop':
        return noop(stack)
    if line.startswith('addx'):
        return addx(stack, int(line[5:]))
    raise ValueError('Unknown command type')


def score_stack_sequence(stack: List[int], start, gap) -> int:
    return sum((step + 2)*stack[step] for step in range(start-2, len(stack), gap))


def draw_stack(stack: List[int]) -> str:
    s = ""
    for position, stack_state in enumerate([1] + stack[:-1]):
        char = "#" if abs(stack_state - position % 40) <= 1 else "."
        s += char
    return "\n".join([s[start:start+40] for start in range(0, len(s), 40)])


def parse_file() -> List[str]:
    with open('day10_input.txt', 'r') as f:
        instructions = f.readlines()
    return instructions


if __name__ == '__main__':
    instructions = parse_file()
    for instruction in instructions:
        stack = update(stack, instruction)
    print(score_stack_sequence(stack, 20, 40))
    print(draw_stack(stack))
