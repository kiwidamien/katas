"""
We have values that have a flow rate and connections described like
  Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
  Valve BB has flow rate=13; tunnels lead to valves CC, AA
  Valve CC has flow rate=2; tunnels lead to valves DD, BB
  Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
  .....

We start at AA.
Each minute we can either undo a value (and then it will release pressure
at rate 'flow_rate' for the remaining minutes) or walk to another connected
tunnel.

We want to optimize the total flow over 30 minutes.

Every minute, we have two options: open or move.
Let's say our movements were
  0: Start at AA
  1: (M, AA->DD)
  2: (O, DD)
  3: (M, DD->CC)
  4: (M, CC->BB)
  5: (O, BB)
  6: (M, AA)
  .....

The flow calculations is
0: Nothing open
1: Nothing open (we are moving)
2: Nothing open (we are opening DD, but it isn't open yet)
3: Flow 20 (DD=20 open; we are moving to CC)
4: Flow 20 (DD=20 open; we are moving to BB)
5: Flow 20 (DD=20 open; we are opening BB)
6: Flow 33 (DD=20 B=13 open; we are moving to AA)
"""

from __future__ import annotations

from collections import deque
import re
from dataclasses import dataclass

from typing import Dict, List


underground = {}

## Parsing
#
@dataclass
class Station:
    name: str
    flow_rate: int
    connections: List[Station]


def parse_line(line, underground) -> Station:
    pattern = r'Valve (.*) has flow rate=(\d+); \w+ \w+ to \w+ (.*)'
    name, rate, connections = re.match(pattern, line).groups()
    rate = int(rate)
    connections = [c.strip() for c in connections.split(',')]
    station = Station(name=name, flow_rate=rate, connections=connections)
    underground[name]=station
    return station


def file_contents_to_underground(contents: str) -> Dict[str, Station]:
    underground = {}
    for line in contents.split('\n'):
        parse_line(line, underground)
    return underground


def parse_file():
    with open('day16_input.txt', 'r') as f:
        return file_contents_to_underground(f.read())


def _distance_from_source(underground: Dict[str, Station], source: Station) -> Dict[str, int]:
    """Returns a mapping from station name to distance from source to station"""
    queue = deque([(source, 0)])
    distances = {}

    while queue:
        current, steps = queue.popleft()
        if current.name not in distances:
            distances[current.name] = steps
        enqueue = [(underground[next_station], steps+1) 
                   for next_station in current.connections 
                   if next_station not in distances]
        queue.extend(enqueue)
    return distances


def distance_list(underground: Dict[str, Station]) -> Dict[str, Dict[str, int]]:
    return {
        source.name: _distance_from_source(underground, source)
        for source in underground.values()
    }


def _evaluate(underground: Dict[str, Station],
              distances: Dict[str, Dict[str, int]],
              opening_order: List[str],
              start_loc: str="AA",
              time_limit: int=30) -> Tuple[int, int]:
    """Returns total flow from moves so far, and an upper bound from this partial path"""
    current = start_loc
    time_remaining = time_limit
    flow = 0
    for next_loc in opening_order:
        # Time to move AND then one turn for opening valve
        time_remaining = time_remaining - distances[current][next_loc] - 1
        if time_remaining < 0:
            raise ValueError("Do not have time to do this")
        flow += time_remaining * underground[next_loc].flow_rate
        current = next_loc
    return (flow, flow)


def _best_in_subtree(
    underground: Dict[str, Station], 
    distances: Dict[str, Dict[str, int]], 
    closed_valves: Set[str], 
    current: str,
    time_remaining:int,
    current_flow: int,
) -> int:
    """Evaluates the best remaining flow in the subtree.

    Some values are already opened, so we are only searching amount the closed_valves.
    Only returns the flow from the subtree."""
    if len(closed_valves) == 0:
        return current_flow
    if time_remaining < 0:
        return -float('inf')
    candidates = [current_flow]
    for next_station in closed_valves:
        new_time = time_remaining - distances[current][next_station] - 1
        new_closed = closed_valves - {next_station}
        new_flow = new_time * underground[next_station].flow_rate
        subtree = _best_in_subtree(
            underground, distances, new_closed, next_station, new_time, current_flow + new_flow
        )
        candidates.append(subtree)
    return max(candidates)


def find_greatest_flow(
    underground: Dict[str, Station],
    start_loc: str="AA",
    time_limit: int=30
) -> int:
    # Try all opening orders where flow > 0
    non_trivial = [name for name, station in underground.items() if station.flow_rate > 0]
    distances = distance_list(underground)
    result = _best_in_subtree(underground, distances, set(non_trivial), start_loc, time_limit, current_flow=0)
    return result


def problem_1_find_max_flow():
    underground = parse_file()
    max_flow = find_greatest_flow(underground, 'AA', 30)
    return max_flow


if __name__=='__main__':
    print("Problem 1: max flow is ", problem_1_find_max_flow())
