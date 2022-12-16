import re
from collections import namedtuple 
from typing import Tuple, Set, List

Loc = Tuple[int, int]
PairDevice = namedtuple("PairDevice", "beacon sensor")


"""
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
"""

def parse_line(line: str) -> PairDevice:
    pattern = r"Sensor at x=(\-?\d+), y=(\-?\d+): closest beacon is at x=(\-?\d+), y=(\-?\d+)"
    x1, y1, x2, y2 = [int(num) for num in re.match(pattern, line).groups()]
    return PairDevice(beacon=(x2, y2), sensor=(x1, y1))


def distance(p1: Loc, p2: Loc) -> int:
    return sum(abs(x1 - x2) for x1, x2 in zip(p1, p2))


def collection_no_known_sensors_one_line(beacon_sensors: List[PairDevice], y_line, x_min=-float('inf'), x_max=float('inf')) -> Set[int]:
    collection = set()
    for beacon, sensor in beacon_sensors:
        collection = collection.union(known_no_sensors_on_line(beacon, sensor, y_line, x_min=x_min, x_max=x_max))
    return collection


def eliminate_beacon_locations_on_line(beacon_sensors: List[PairDevice], y_line, x_min, x_max) -> Set[int]:
    collection = set(range(x_min, x_max+1))
    for beacon, sensor in beacon_sensors:
        collection = collection - known_no_sensors_on_line(beacon, sensor, y_line, x_min=x_min, x_max=x_max)
        if len(collection) == 0:
            return {}
    return collection


def parse_file() -> List[PairDevice]:
    with open('day15_input.txt', 'r') as f:
        return [parse_line(line) for line in f.readlines()]


def part_1_how_many_locations_cannot_have_sensors():
    beacons_and_sensors = parse_file()
    known_no_sensors = collection_no_known_sensors_one_line(beacons_and_sensors, 2_000_000)
    return len(known_no_sensors)


def _intervals_beacons_forbidden(pair: PairDevice, y_line: int, x_min=-float('inf'), x_max=float('inf')) -> List[Tuple[int, int]]:
    beacon, sensor = pair.beacon, pair.sensor
    d = distance(beacon, sensor)
    vert_distance=abs(y_line - sensor[1])
    if vert_distance > d:
        return []
    horiz_bound=d-vert_distance
    start = min(max(sensor[0] - horiz_bound, x_min), x_max)
    end = max(min(sensor[0] + horiz_bound, x_max), x_min)
    return [(start, end)]


def _merge_intervals(intervals: List[Tuple[int, int]]) -> List[Tuple[int, int]]:
    ordered = sorted([i for i in intervals if i])
    if len(ordered) == 0: return []
    merged = [ordered[0]]
    for interval in ordered:
        if interval[0] - 1 > merged[-1][1]:
            merged.append(interval)
            continue
        if interval[1] <= merged[-1][1]:
            continue
        merged[-1] = (merged[-1][0], interval[1])
    return merged


def _length_of_merged_intervals(merged_intervals: List[Tuple[int, int]]) -> int:
       return sum( interval[1] - interval[0] + 1 for interval in merged_intervals)


def _get_number_forbidden_beacon_locations(pairs: List[PairDevice], y_line: int, x_min=-float('inf'), x_max=float('inf')) -> int:
    intervals = sum([_intervals_beacons_forbidden(pair, y_line, x_min, x_max) for pair in pairs if pair], [])
    merged_intervals = _merge_intervals(intervals)
    return _length_of_merged_intervals(merged_intervals)


def known_no_sensors_on_line(beacon: Loc, closest_sensor: Loc, y_line: int, x_min=-float('inf'), x_max=float('inf')) -> Set[int]:
    d = distance(beacon, closest_sensor)
    vert_distance = abs(y_line - closest_sensor[1])
    if vert_distance > d:
        return set()
    horiz_bound = d-vert_distance
    start = max(closest_sensor[0] - horiz_bound, x_min)
    end = min(closest_sensor[0] + horiz_bound, x_max)
    x = {
        h for h in range(start, end+1) if (h, y_line) != beacon
    }
    return x


def part_2_how_many_locations_cannot_have_beacon_in_grid():
    beacons_and_sensors = parse_file()
    #possible_locations = eliminate_beacon_locations_on_line(beacons_and_sensors, y_line=2_000_000, x_min=0, x_max=4_000_000)
    #print(possible_locations)
    #print(_get_number_forbidden_beacon_locations(beacons_and_sensors, y_line=2_000_000, x_min=0, x_max=4_000_000))
    #return len(possible_locations)
    possible_locations = unpack_intervals(intervals(beacons_and_sensors, 4_000_000))
    assert len(possible_locations) == 1
    assert len(possible_locations[0]) == 2
    # The x,y coordinate of the unique possible unscouted location in the (0,0) x (4_000_000, 4_000_000) box
    return possible_locations[0]


#################################################################
def intervals(pairs, row_max):
    stuff = []
    for y_line in range(row_max):
        intervals = sum([_intervals_beacons_forbidden(pair, y_line, 0, row_max) for pair in pairs if pair], [])
        merged = _merge_intervals(intervals)
        if merged != [(0, row_max)]:
            stuff.append((y_line, _merge_intervals(intervals)))
    return stuff


def unpack_intervals(output_from_intervals):
    # Format is [row, [(start_forbidden, end_forbidden), (start_forbidden, end_forbidden), .... ]]
    # Want a list of POSSIBLE locations
    possible_locations = []
    for row in output_from_intervals:
        row_num, block_data = row
        for (_, block1_end), (block2_start, _) in zip(block_data, block_data[1:]):
            possible_locations.extend([(col, row[0]) for col in range(block1_end + 1, block2_start)])
    return possible_locations


def part_two_again(row_max):
    pairs = parse_file()
    print(intervals(pairs, row_max))


if __name__=='__main__':
    print(part_1_how_many_locations_cannot_have_sensors())
    print("Warning: it will take ~2 mins to get the next part :( ")
    x,y = part_2_how_many_locations_cannot_have_beacon_in_grid()
    print(f"Only unscouted location in (0,0) -- (4_000_000, 4_000_000) is {(x,y)}")
    multiplier = 4_000_000
    # this happens to be the index in a flattened array :)!!
    frequency = multiplier * x + y
    print(f'{frequency=}')
