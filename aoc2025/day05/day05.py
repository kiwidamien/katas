def parse(raw_lines: str):
    def _parse_range(lines) -> [[int]]:
        return [[int(x) for x in line.split('-')] for line in lines]

    def _parse_ingredients(lines: [str]) -> [int]:
        return [int(ingredient_id) for ingredient_id in lines]

    parts = [line.strip() for line in raw_lines.split('\n\n')]
    
    ranges = _parse_range(parts[0].split('\n'))
    ingredients = _parse_ingredients(parts[1].split('\n'))
    return ranges, ingredients

    
def ingredient_in(ranges, ingredient_id):
    for (start, stop) in ranges:
        if start <= ingredient_id <= stop:
            return True
    return False


def count_fresh(ranges, ingredient_ids):
    return sum([ingredient_in(ranges, ingredient_id) for ingredient_id in ingredient_ids])


def merge_intervals(
        intervals
):
    sorted_intervals = sorted(
        intervals,
        key=lambda x: (x[0], -x[1])
    )
    new_intervals = [sorted_intervals[0]]
    for (new_start, new_stop) in sorted_intervals[1:]:
        old_start, old_stop = new_intervals[-1]
        if new_start > old_stop+1:
            new_intervals.append((new_start, new_stop))
            continue
        if new_stop <= old_stop:
            continue
        new_intervals[-1] = (old_start, max(new_stop, old_stop))
    return new_intervals


def total_fresh(merged_intervals):
    return sum([stop - start + 1 for start, stop in merged_intervals])


def part_one(filename):
    with open(filename) as f:
        contents = f.read()
    ranges, ingredients = parse(contents)
    print(count_fresh(ranges, ingredients))


def part_two(filename):
    with open(filename) as f:
        contents = f.read()
    ranges, _ = parse(contents)
    print(total_fresh(merge_intervals(ranges)))