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

import re
from dataclasses import dataclass

from typing import Dict, List


underground = {}


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


if __name__=='__main__':
    line='Valve GG has flow rate=0; tunnel leads to valves FF, HH'
    print(parse_line(line, underground))
    print(parse_file())
