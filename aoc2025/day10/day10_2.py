import functools
import numpy as np


line = "[#...##] (0,1,3,4,5) (0,4,5) (1,2,3,4) (0,1,2) {132,30,23,13,121,115}"
simple_line = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
other = "[.#..#.#.] (3,4,5) (0,1,2,5,6,7) (0,4,7) (1,2,3,4,5) (0,3,6) (0,1,4,5,7) (0,1,2,3,4,5) (1,2,3) {59,74,65,52,52,60,25,41}"

def parse(line: str):
    words = line.split()
    target_str = words[0] 
    button_str = words[1:-1]
    joltage_str = words[-1]
    target = [0 if c == '.' else 1 for c in target_str[1:-1]]
    joltage = [int(x) for x in joltage_str[1:-1].split(',')]
    buttons = [[int(x) for x in b[1:-1].split(',')] for b in button_str]
    return (target, buttons, joltage)


def press(light_state: list[int], button: list[int]) -> list[int]:
    return [1-state if index in button else state for (index,state) in enumerate(light_state)]


def press_sequence(light_state: list[int], buttons: list[list[int]]) -> list[int]:
    new_state = light_state[:]
    for button in buttons:
        new_state = press(new_state, button)
    return new_state


def powerset(a):
    if len(a)==0:
        return [[]]
    reduced = powerset(a[1:])
    return reduced + [[a[0]] + r for r in reduced]

def find_minimal_presses(target_toggles: list[int], buttons: list[list[int]]):
    """Find the minimum number of button presses, so that light[i] toggles target_toggles[i] times.
    
    For example, if buttons = [[3], [1, 3], [2], [2,3], [0,2], [0,1]]
    this means that button 0 toggles light 3
                    button 1 toggles light 1 and 3
                    button 2 toggles light 2
                    button 3 toggles lights 2 and 3
                    button 3 toggles lights 0 and 2
                    button 4 toggles lights 0 and 1

    If we want to find [3, 5, 4, 7], we are looking for the minimum number of 
    button presses that toggle 
        light 0 3 times
        light 1 5 times
        light 2 4 times
    and light 3 7 times.

    We can do this with 10 button presses
        * Button 0 1x
        * Button 1 3x
        * Button 2 0x
        * Button 3 3x
        * Button 4 1x
        * Button 5 2x

    so find_minimal_presses([3, 5, 4, 7], [[3], [1, 3], [2], [2,3], [0,2], [0,1]]) = 10
    """
    # Setup
    num_lights = len(target_toggles)
    ini_map = [0 for _ in range(num_lights)]
    powerset_buttons = powerset(buttons)
    _state_map = [
        (tuple(press_sequence(ini_map, buttons)), buttons) for buttons in powerset_buttons
    ]
    state_map = {}
    for s, b in _state_map:
        state_map[s] = state_map.get(s, []) + [b]
    
    # Recursion
    def inner(target_toggles: tuple[int]):
        if all([a == 0 for a in target_toggles]):
            return 0
        if any([a < 0 for a in target_toggles]):
            return float('inf')
        if all([a % 2 == 0 for a in target_toggles]):
            new_tuple = tuple([a//2 for a in target_toggles])
            return 2*inner(new_tuple)
        
        toggle_mod = tuple([a%2 for a in target_toggles])
        if toggle_mod not in state_map:
            return float('inf')
        
        choices = []
        for button_presses in state_map[toggle_mod]:
            num_presses = len(button_presses)
            light_toggles = ini_map[:]
            for button in button_presses:
                for light in button:
                    light_toggles[light] += 1
            new_tuple = tuple([t - toggles for t, toggles in zip(target_toggles, light_toggles)])
            choices.append(num_presses + inner(new_tuple))
        return min(choices)
    return inner(tuple(target_toggles))


def beginning_to_end(line: str):
    (_, buttons, joltage) = parse(line)
    return find_minimal_presses(joltage, buttons)


(_, buttons, joltage) = parse(other)

from collections import Counter 

example_presses = [2, 20, 12, 16, 5, 9, 13, 16]
#example_presses = [2, 11, 12, 16, 5, 9, 13, 16]
_toggle_count = Counter(sum(sum([[b] * p for b, p in zip(buttons, example_presses)], []), []))
toggle_count = [_toggle_count.get(v, 0) for v in range(len(buttons))]



def update_joltage(original_joltage:tuple[int], button_seq: list[list[int]]) -> tuple[int]:
    light_toggles = [0 for _ in original_joltage]
    for button in button_seq:
        for light in button:
            light_toggles[light] += 1
    new_tuple = tuple([t -toggles for t, toggles in zip(original_joltage, light_toggles)])
    return new_tuple


def my_func():
    for a in range(16):
        for b in range(16):
            for c in range(16):
                for d in range(16):
                    for e in range(16):
                        for f in range(16):
                            for g in range(16):
                                for h in range(16):
                                    example = [a,b,c,d,e,f,g,h]
                                    _tc = Counter(sum(sum([[bb] * p for bb, p in zip(buttons, example)], []), []))
                                    tc = [_tc.get(v, 0) for v in range(len(buttons))]
                                    sol = find_minimal_presses(tc, buttons)
                                    if sol != sum(example):
                                        return example, tc

#counter_sol, counter_target = my_func()

A = np.array([[0, 1, 1, 0, 1, 1, 1, 0],
 [0, 1, 0, 1, 0, 1, 1, 1],
 [0, 1, 0, 1, 0, 0, 1, 1],
 [1, 0, 0, 1, 1, 0, 1, 1],
 [1, 0, 1, 1, 0, 1, 1, 0],
 [1, 1, 0, 1, 0, 1, 1, 0],
 [0, 1, 0, 0, 1, 0, 0, 0],
 [0, 1, 1, 0, 0, 1, 0, 0]])


def find_presses(jolt):
    return np.round(np.linalg.inv(A) @ np.array(jolt), decimals=2)