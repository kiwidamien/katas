from typing import List


class Node:
    def __init__(self, num):
        self.num = num
        self.next = None
        self.prev = None

    def move_forward(self, places):
        ptr = self.next
        self._cut()
        for _ in range(places-1):
            ptr = ptr.next
        self._place_between(ptr, ptr.next)

    def move_back(self, places):
        ptr = self.prev
        self._cut()
        for _ in range(places-1):
            ptr = ptr.prev
        self._place_between(ptr.prev, ptr)

    def _cut(self):
        self.prev.next = self.next
        self.next.prev = self.prev

    def _place_between(self, before, after):
        before.next = self
        self.prev = before
        after.prev = self
        self.next = after

    def move(self, places):
        if places==0:
            return
        if places > 0:
            self.move_forward(places)
        else:
            self.move_back(-places)


class CircularLinkedList:
    def __init__(self, nums):
        self.length = len(nums)
        self.nodes = [Node(n) for n in nums]
        for here, next_node in zip(self.nodes, self.nodes[1:]):
            here.next = next_node
            next_node.prev = here
        self.nodes[0].prev = self.nodes[-1]
        self.nodes[-1].next = self.nodes[0]

    def get_value(self, n):
        for check in self.nodes:
            if check.num==n:
                return check
        raise ValueError(f'{n} not found')

    def _flatten(self, start: Node) -> List[int]:
        output = []
        current = start
        for _ in range(self.length):
            output.append(current.num)
            current = current.next
        return output

    def flatten_from_zero(self) -> List[int]:
        zero = self.get_value(0)
        return self._flatten(zero)

    def flatten(self) -> List[int]:
        return self._flatten(self.nodes[0])


def process_string(s: str) -> List[int]:
    return [int(num) for num in s.split('\n')]


def mix_from_zero(orig: List[int]) -> List[int]:
    linked = CircularLinkedList(orig)
    for node in linked.nodes:
        node.move(node.num)
    return linked.flatten_from_zero()


def get_coords(orig: List[int]) -> List[int]:
    mixed = mix_from_zero(orig)
    return [
        mixed[index%len(orig)]
        for index in (1000, 2000, 3000)
    ]


def prob1_get_sum_coord():
    orig = parse_file()
    assert orig.count(0)==1, "Must be a unique zero element"
    print('Length:', len(orig))
    print('unique length:', len(set(orig)))
    coords = get_coords(orig)
    print(coords)
    return sum(coords)


def coord_with_decryption(orig):
    key = 811589153
    decrypt = [key*num for num in orig]
    linked = CircularLinkedList(decrypt)
    for _ in range(10):
        for node in linked.nodes:
            node.move(node.num % (linked.length - 1))
    flat = linked.flatten_from_zero()
    return flat


def sum_coord_with_decryption(orig):
    flat = coord_with_decryption(orig)
    return sum([flat[index % len(orig)] for index in (1000, 2000, 3000)])


def prob2_sum_with_decryption():
    orig = parse_file()
    return sum_coord_with_decryption(orig)


def parse_file():
    with open('day20_input.txt') as f:
        return process_string(f.read())

if __name__=='__main__':
    print(f'Sum of cooordinates:', prob1_get_sum_coord())
    print('Decrypted sum:', prob2_sum_with_decryption())

