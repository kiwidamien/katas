from typing import List


class Node:
    def __init__(self, num):
        self.num = num
        self.next = None
        self.prev = None

    def move_forward(self, places):
        ptr = self
        for _ in range(places):
            ptr = ptr.next
        self.prev.next = self.next
        self.next.prev = self.prev
        self._place_between(ptr, ptr.next)

    def move_back(self, places):
        ptr = self
        for _ in range(places):
            ptr = ptr.prev
        self.prev.next = self.next
        self.next.prev = self.prev
        self._place_between(ptr.prev, ptr)

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


def process_string(s: str) -> List[int]:
    return [int(num) for num in s.split('\n')]


def mix_from_zero(orig: List[int]) -> List[int]:
    orig_order = orig[:]
    linked = CircularLinkedList(orig)
    for node in linked.nodes:
        node.move(node.num)

    output = []
    current = linked.get_value(0)
    for _ in range(len(orig_order)):
        output.append(current.num)
        current = current.next
    return output

def get_coords(orig: List[int]) -> List[int]:
    mixed = mix_from_zero(orig)
    return [
        mixed[index%len(orig)]
        for index in (1000, 2000, 3000)
    ]


def prob1_get_sum_coord():
    orig = parse_file()
    print('Length:', len(orig))
    print('unique length:', len(set(orig)))
    coords = get_coords(orig)
    print(coords)
    return sum(coords)


def parse_file():
    with open('day20_input.txt') as f:
        return process_string(f.read())

if __name__=='__main__':
    print(f'Sum of cooordinates:', prob1_get_sum_coord())
    
