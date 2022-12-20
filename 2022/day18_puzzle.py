from typing import List


def get_area(points: List[List[int]]) -> int:
    pt_set = set(tuple(vec) for vec in points)
    count_area = 0
    for pt in pt_set:
        x,y,z = pt
        for coord in [0,1,2]:
            for direction in [-1, 1]:
                new_pt = [x,y,z]
                new_pt[coord] += direction
                if tuple(new_pt) not in pt_set:
                    count_area += 1
    return count_area


def get_bounding_box(points: List[List[int]]) -> int:
    mins = [float("inf")] * 3
    maxs = [-float("inf")] * 3
    for point in points:
        mins = [min(m, point[index]) for index, m in enumerate(mins)]
        maxs = [max(m, point[index]) for index, m in enumerate(maxs)]
    return (mins, maxs)


def neighbors(point):
    for coord in [0,1,2]:
        for direction in [1, -1]:
            new_point = list(point)
            new_point[coord] += direction
            yield tuple(new_point)


def inside_bounding_box(point, bound_box):
    mins, maxs = bound_box
    for coord in range(len(point)):
        if point[coord] < mins[coord]:
            return False
        if point[coord] > maxs[coord]:
            return False
    return True


def _interior_point_finder(pt_set, bound_box, seed):
    stack = [seed]
    visited = set()
    is_interior = True
    while stack:
        current = stack.pop()
        if current in visited:
            continue
        for touching in neighbors(current):
            if touching not in pt_set and touching not in visited:
                if inside_bounding_box(touching, bound_box):
                    stack.append(touching)
                else:
                    is_interior = False
        visited.add(current)
    return (visited, is_interior)


def _determine_interiors(
    pt_set, candidates
):
    bound_box = get_bounding_box(pt_set)
    candidates = set(candidates)
    interior = set()
    while candidates:
        seed = candidates.pop()
        visited, is_interior = _interior_point_finder(pt_set, bound_box, seed)
        if is_interior:
            interior = interior.union(visited)
        candidates = candidates - visited
    return interior


def find_interior_points(points) -> int:
    candidate_points = set()
    pt_set = set(tuple(vec) for vec in points)
    for point in points:
        x,y,z = point
        for coord in [0,1,2]:
            for direction in [-1, 1]:
                new_pt = [x,y,z]
                new_pt[coord]+=direction
                if tuple(new_pt) not in pt_set:
                    candidate_points.add(tuple(new_pt))
    return _determine_interiors(pt_set, candidate_points) 


def get_exterior_area(points) -> int:
    interior_points = find_interior_points(points)
    filled_points = set(tuple(p) for p in points).union(interior_points)
    return get_area(list(filled_points))


def parse_file():
    with open('day18_input.txt') as f:
        return [[int(coord) for coord in line.split(',')] for line in f.readlines()]


def problem1_count_area():
    points = parse_file()
    return get_area(points)


def problem2_exterior_area():
    points = parse_file()
    return get_exterior_area(points)


if __name__=='__main__':
    area = problem1_count_area()
    print(f"problem 1: area is ", area)
    exterior_area = problem2_exterior_area()
    print(f"problem 2: exterior area is ", exterior_area)

