from __future__ import annotations
from dataclasses import dataclass

from typing import List, Dict, Optional

@dataclass
class Node:
    name: str
    parent: Node
    size: int
    is_file: bool
    children: Dict[str, Node]


def go_to_parent(current: Node) -> Node:
    return current.parent


def go_to_root(current: Optional[Node]=None) -> Node:
    if current is None:
        return Node(name='/', parent=None, size=0, is_file=False, children={})
    while current.parent is not None:
      current = current.parent
    if current.name != '/':
        return Node(name='/', parent=None, size=0, is_file=False, children={})
    return current


def change_directory(current: Node, name) -> Node:
    if name not in current.children:
        raise KeyError(f'We do not know of a subdiretory called {name}')
    if current.children[name].is_file:
        raise ValueError(f'{name} is a file, not a directory')
    return current.children[name]


def parse_listing(current: Node, listing_output: List[str]) -> Node:
    for line_item in listing_output:
        info, name = line_item.split()
        if name in current.children:
            continue
        if info == 'dir':
            current.children[name] = Node(name=name, parent=current, size=0, is_file=False, children={})
        else:
            current.children[name] = Node(name=name, parent=current, size=int(info), is_file=True, children={})
    return current


def parse_command(command_stack: List[str], current: Node) -> Tuple[List[str], Node]:
    """Processes exactly one command from the stack, may remove output as well.

    Returns new command stack and the new current node"""
    top_command, new_stack = command_stack[0].strip(), command_stack[1:]
    if top_command[0] != '$':
        raise ValueError(f"{top_command} is not a command!")
    if top_command=="$ cd /":
        return new_stack, go_to_root(current)
    if top_command=="$ cd ..":
        return new_stack, go_to_parent(current)
    if top_command.startswith("$ cd"):
        new_dir = top_command.split()[-1].strip()
        return new_stack, change_directory(current, new_dir)
    if top_command=="$ ls":
        listing = []
        while new_stack and new_stack[0][0] != '$':
            line, new_stack = new_stack[0], new_stack[1:]
            listing.append(line)
        return new_stack, parse_listing(current, listing_output=listing)
    raise ValueError(f'Unknown command: {top_command}')


def parse_all_commands(command_stack: List[str], current:Node) -> Node:
    current_stack = command_stack
    while current_stack:
        current_stack, current = parse_command(command_stack=current_stack, current=current)
    return current


def _sizes(current: Node, accumulator: Dict[str, int]) -> int:
    if current.is_file:
        return current.size
    qualified_name = [current.name]
    parent = current.parent
    while parent:
        qualified_name.append(parent.name)
        parent = parent.parent
    qualified_name = '/'.join(qualified_name[::-1])

    if qualified_name in accumulator:
        return accumulator[qualified_name]
    accumulator[qualified_name] = sum(_sizes(c, accumulator) for c in current.children.values()) + current.size
    return accumulator[qualified_name]


def find_subtree_sizes(current) -> Dict[str, int]:
    accumulator = {}
    _sizes(current, accumulator)
    return accumulator


def find_filtered_sum(current: Node, max_size: int) -> int:
    subtree_sizes = find_subtree_sizes(current)
    return sum(size for size in subtree_sizes.values() if size <= max_size)


def parse_file() -> List[str]:
    with open('day08_input.txt', 'r') as f:
        contents = f.readlines()
    return contents


def find_minimum_small_enough(root: Node, total_space: int, space_needed: int) -> int:
    find_size = find_subtree_sizes(root)
    free_space = total_space - find_size['/']
    size_to_free = space_needed - free_space
    sizes = sorted(s for s in find_size.values() if s >= size_to_free)
    return sizes[0]


if __name__=='__main__':
    root = go_to_root()
    commands = parse_file()
    current = parse_all_commands(command_stack=commands, current=root)
    root = go_to_root(current)
    print(find_filtered_sum(root, max_size=100_000))
    print(find_minimum_small_enough(root, total_space=70_000_000, space_needed=30_000_000))


