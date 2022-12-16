import pytest
import sys
sys.path.append('..')

import day15_puzzle as day15


EXAMPLE="""Sensor at x=2, y=18: closest beacon is at x=-2, y=15
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
Sensor at x=20, y=1: closest beacon is at x=15, y=3""".split("\n")


@pytest.mark.parametrize("line,expected_sensor,expected_beacon",[
    [EXAMPLE[1], (9, 16), (10, 16)], 
    [EXAMPLE[3], (12,14), (10, 16)],
    [EXAMPLE[4], (10,20), (10, 16)]
])
def test_parse_line(line, expected_sensor, expected_beacon):
    beacon, sensor = day15.parse_line(line)
    assert sensor == expected_sensor
    assert beacon == expected_beacon


def test_beacon_sensor_on_a_line():
    """Have a beacon at (7, 3), and a sensor at (4, 5)

        0123456789 etc
      -2.......#.....
      -1......###....
       0.....#####...
       1....#######..
       2...#########.
       3..#####S#####
       4...#########.
       5....B######..
       6.....#####...
       7......###....
       8.......#.....
       9.............

    So on line 4 we should have x values {3,4,5,6,7,8,9,10,11}
    """
    beacon, sensor = (4,5), (7,3)
    expected = {3,4,5,6,7,8,9,10,11}
    result = day15.known_no_sensors_on_line(beacon, sensor, 4)
    assert len(result)==len(expected)
    assert result==expected


def test_beacon_sensor_on_a_line_with_sensor():
    beacon, sensor = (4, 5), (7, 3)
    expected = {5,6,7,8,9,10}
    result = day15.known_no_sensors_on_line(beacon, sensor, 5)
    assert len(result)==len(expected)
    assert result==expected


def test_collection_method_single_sensor():
    beacon_sensor = day15.PairDevice(beacon=(4,5), sensor=(7,3))
    result = day15.collection_no_known_sensors_one_line([beacon_sensor], 5)
    expected = {5,6,7,8,9,10}
    assert result == expected
    result = day15.collection_no_known_sensors_one_line([beacon_sensor]*2, 5)
    assert result == expected


def test_row_10_of_example():
    beacons_and_sensors = [day15.parse_line(line) for line in EXAMPLE]
    known_no_sensors = day15.collection_no_known_sensors_one_line(beacons_and_sensors, 10)
    assert len(known_no_sensors)==26


def test_what_locations_are_okay():
    pairs = [day15.parse_line(line) for line in EXAMPLE]
    intervals = sum([
        day15._intervals_beacons_forbidden(pair, y_line, x_min=0, x_max=20)
        for pair in pairs if pair for y_line in range(20)
    ], [])
    intervals = set(intervals)
    assert (14, 11) not in intervals


