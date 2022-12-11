import sys
sys.path.append('..')
import day08_puzzle as day08
import pytest


@pytest.fixture()
def example1():
    return """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k""".split('\n')


@pytest.fixture()
def tree_from_example(example1):
    root = day08.go_to_root()
    current = day08.parse_all_commands(example1, root)
    root = day08.go_to_root(current)
    return root


def test_subdirectory_sizes_description(tree_from_example):
    answer = {'//a': 94_853, '//d': 24_933_642, '//a/e': 584, '/': 48_381_165}
    assert day08.find_subtree_sizes(tree_from_example)==answer


def test_example_in_description(tree_from_example):
    answer = 95_437
    assert day08.find_filtered_sum(tree_from_example, max_size=100_000)==answer

def test_smallest_dir_to_delete(tree_from_example):
    answer=24_933_642
    assert day08.find_minimum_small_enough(tree_from_example, total_space=70_000_000, space_needed=30_000_000)==answer


