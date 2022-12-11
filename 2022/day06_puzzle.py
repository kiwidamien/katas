def parse_input() -> str:
    with open('sixth_puzzle_input.txt', 'r') as f:
        contents = f.read()
    return contents


def find_beginner_marker(message: str, length: int) -> int:
    for index in range(len(message) - length):
        if len(set(message[index:index+length]))==length:
            return index+length


if __name__=='__main__':
    print(find_beginner_marker(parse_input(), length=4))
    print(find_beginner_marker(parse_input(), length=14))

