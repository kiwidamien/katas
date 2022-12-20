"""
* Each robot can collect 1 of its resource type per min
* You start with 1 ore robot
* It takes 1 minute to make a robot
* Need geode cracking robots

Example:
    Ore robots costs 4 ore, 0 clay, 0 obsidian
    Clay robots costs 2 ore, 0 clay, 0 obsidian
    Obsidian robots costs 3 ore, 14 clay, 0 obsidian
    Geode robots costs 2 ore and 7 obsidian

    Initial state: Robots = {1 ore}, Inventory = {}
    1. 1 ore robot collects 1 ore; you now have 1 ore.
       Robots = {1 ore}, Inventory = {1 ore}
    2. 1 ore robot collects 1 ore; you now have 2 ore.
       Robots = {1 ore}, Inventory = {2 ore}
    3. Spend 2 ore to build a clay robot (Inventory -> {})
       Collect resources: 1 ore-robot collects one ore (Inventory -> {1 ore})
       Build a clay robot
       Robots = {1 ore, 1 clay}

etc.

For a minute, the order of operations are(
  a. pay for a robot (if you want)
  b. collect resources
  c. get robot
"""

from collections import namedtuple
from dataclasses import dataclass
import re


from typing import Optional

MAGIC_BUILD = [
        None,
        None,
        None,
        None,
        0,
        None,
        1,
        1,
        1,
        1, # Min 10
        1,
        1,
        1,
        2,
        None,
        2,
        2,
        None,
        2,
        3, # Min 20
        2,
        3,
        3,
        3,
        None,
        3,
        3,
        None,
        3,
        3, # Min 30
        3,
        None
    ]



Costs = namedtuple("Costs", "ore clay obsidian")
Resources = namedtuple("Resources", "ore clay obsidian geode")


@dataclass
class Blueprint:
    id: int
    ore: Costs
    clay: Costs
    obsidian: Costs
    geode: Costs

    def get_cost(self, resource: int):
        if resource==0:
            return self.ore
        if resource==1:
            return self.clay
        if resource==2:
            return self.obsidian
        if resource==3:
            return self.geode
        raise ValueError('Unknown resource')

    def most_expensive_robot(self, resource: int) -> int:
        """Returns the highest cost you have to pay in RESOURCE for a single robot

        e.g. If most_expensive_robot(0) returns 4, this means that every robot costs
        4 or fewer ore. (Therefore there is no use in having more than 4 ore robots)
        """
        candidates = [r[resource] for r in (self.ore, self.clay, self.obsidian, self.geode)]
        return max(candidates)

    def can_afford(self, inventory):
        def _afford(costs):
            return all(i >= c for i, c in zip(inventory, costs))
        return {
            index for index in range(4)
            if _afford(self.get_cost(index))
        }


@dataclass
class State:
    inventory: Resources
    robots: Resources


def new_state():
    return State(inventory=(0,0,0,0), robots=(1,0,0,0))


def _pay_for_robots(
    state: State, blueprint: Blueprint, build: Optional[int]
) -> Resources:
    if build is None:
        return state.inventory
    pay = list(blueprint.get_cost(build)) + [0]
    new_balance = [
        r - cost for r, cost in zip(state.inventory, pay)
    ]
    if any(bal < 0 for bal in new_balance):
        raise ValueError('New balance is ', new_balance,' which requires a loan!')
    return Resources(*new_balance)


def _collection_phase(inventory: Resources, robots: Resources) -> Resources:
    new_inventory = [old + robots for old, robots in zip(inventory, robots)]
    return Resources(*new_inventory)


def _robot_finished_phase(state: State, build: Optional[int]) -> Resources:
    if build is None:
        return state.robots
    new_robots = [
        old_robot + 1 if build==index else old_robot
        for index, old_robot in enumerate(state.robots)
    ]
    return Resources(*new_robots)


def turn(state: State, blueprint: Blueprint, build: Optional[int]):
    new_balance = _pay_for_robots(state, blueprint, build)
    new_inventory = _collection_phase(new_balance, state.robots)
    new_robots = _robot_finished_phase(state, build)
    return State(inventory=new_inventory, robots=new_robots)


def _search_space(state: State, blueprint: Blueprint, time: int, best_so_far, allowed):
    if time == 0:
        return state.inventory[-1]
    # what if we create an geode building machine every minute?
    upper_bound = state.inventory[-1] + time*(time+1)/2 + time*state.robots[-1]
    if upper_bound < best_so_far['best']:
        return upper_bound
    if state.inventory[-1] > best_so_far['best']:
        best_so_far['best'] = state.inventory[-1]

    num_geodes = []
    can_afford = blueprint.can_afford(state.inventory)

    if 3 in can_afford:
        new_state = turn(state, blueprint, build=3)
        geode = _search_space(new_state, blueprint, time-1, best_so_far, allowed={0,1,2,3})
        return geode

    for option in [2, 1, 0]:
        if option not in can_afford:
            continue
        if option not in allowed:
            continue
        most_needed = blueprint.most_expensive_robot(option)
        if state.robots[option] >= most_needed:
            continue
        new_state = turn(state, blueprint, build=option)
        geode = _search_space(new_state, blueprint, time-1, best_so_far, allowed={0,1,2,3})
        num_geodes.append(geode)

    #( if we are saving up, do not build a robot we could have afforded this turn but skipped
    # it would be optimal to buy this turn unless we were saving for something else
    allowed = allowed - can_afford
    new_state = turn(state, blueprint, build=None)
    geode = _search_space(new_state, blueprint, time-1, best_so_far, allowed)
    num_geodes.append(geode)

    return max(num_geodes)


def search(blueprint, time):
    state = new_state()
    return _search_space(state, blueprint, time, {'best': -1}, allowed={0,1,2,3})


def parse_line(line) -> Blueprint:
    pattern = r"Blueprint (\d+):\D*(\d+)\D*(\d+)\D*(\d+)\D*(\d+)\D*(\d+)\D*(\d+)"
    nums = [int(n) for n in re.match(pattern, line).groups()]
    bp = Blueprint(
        id=nums[0],
        ore=(nums[1], 0, 0),
        clay=(nums[2], 0, 0),
        obsidian=(nums[3], nums[4], 0),
        geode=(nums[5], 0, nums[6])
    )
    return bp


def parse_file():
    with open('day19_input.txt') as f:
        bps = [parse_line(line) for line in f.readlines()]
    return bps


def problem_1_magic_number():
    blueprints = parse_file()
    quality_level = 0
    for blueprint in blueprints:
        num_geodes = search(blueprint, time=24)
        quality_level += blueprint.id * num_geodes
    return quality_level 


def prob2_product_of_three_blueprints():
    blueprints = parse_file()
    geodes = [search(blueprint, time=32) for blueprint in blueprints[:3]]
    return geodes[0]*geodes[1]*geodes[2]


if __name__=='__main__':
    state = new_state()
    bp = Blueprint(id=1, ore=(4,0,0), clay=(2,0,0), obsidian=(3,14,0), geode=(2,0,7))

    print(problem_1_magic_number())
    print(search(bp, time=32))
    print(prob2_product_of_three_blueprints())


