closed_to_open = {')': '(', ']':'[', '}':'{', '>': '<'}
closed_chars, open_chars = [
    set(char_tup) for char_tup in zip(*closed_to_open.items())
]


def reduce_stack(s: str) -> str:
    stack = []
    for char in s:
        if char in open_chars:
            stack.append(char)
        if char in closed_to_open:
            if len(stack)==0:
                return char
            if stack[-1]!=closed_to_open[char]:
                stack.append(char)
                break
            stack.pop()
    return ''.join(stack)


def first_illegal_char(s: str) -> str:
    reduced = reduce_stack(s)
    return reduced[-1] if reduced else ''


def return_completion(s: str) -> str:
    reduced = reduce_stack(s)
    if any(char in closed_chars for char in reduced):
        return ''  # could also raise an Error, as this cannot be completed
    open_to_closed = {v: k for k, v in closed_to_open.items()}
    return ''.join(open_to_closed[char] for char in reduced[::-1])

