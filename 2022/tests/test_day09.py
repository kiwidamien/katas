import sys
sys.path.append('..')
import day09_puzzle as day09


example_input = """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"""

def test_example_in_description_short_rope_test_positions_part_1():
    commands = example_input.split("\n")
    initial_state = [(0,0), (0,0)]
    state_history = day09.process_states(initial_state, commands=commands)
    count = day09.count_tail_locations(state_history)
    assert count == 13

