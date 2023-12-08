class Interval:
    def __init__(self, dest_start, source_start, length):
        self.dest_start = dest_start
        self.dest_end = dest_start + length - 1
        self.source_start = source_start
        self.source_end = source_start + length - 1
    
    def __contains__(self, x: int) -> bool:
        return self.source_start <= x <= self.source_end
    
    def translate(self, x: int) -> int | None:
        if x not in self:
            return None
        offset = x - self.source_start 
        return self.dest_start + offset 
    
    def __repr__(self):
        return f'{self.__class__.__name__}: ({self.source_start} -> {self.dest_start}, ..., {self.source_end} -> {self.dest_end})'
    
        
class Mapping:
    def __init__(self, dest_source_length: list[(int, int, int)]):
        self.intervals = [
          Interval(dest_start, source_start, length) 
          for dest_start, source_start, length in dest_source_length
        ]

    def __getitem__(self, x) -> int:
        for interval in self.intervals:
            translation = interval.translate(x)
            if translation is not None:
                return translation 
        return x
    
    def map_intervals(self, lst_of_intervals: list[(int, int)]) -> list[(int, int)]:
        new_intervals = []
        for low, high in lst_of_intervals:
            # need to partition (low, high) into 
            # (low, x0) : Mapped by interval A0
            # (x1, x2): Mapped by interval A1
            # ...
            # (xn, high): Mapped by interval An
            current = low
            while current <= high:
                current_interval = [i for i in self.intervals if current in i]
                if len(current_interval) == 0:
                    # We have an identity transformation, how far does this extend?
                    ranked_intervals = [
                        i for i in sorted(self.intervals, key=lambda x: x.source_start)
                        if i.source_start > current
                    ]
                    if len(ranked_intervals) == 0:
                        current_high = float("inf")
                    else:
                        current_high = ranked_intervals[0].source_start - 1
                else:
                    current_high = current_interval[0].source_end
                current_high = min(high, current_high)
                new_intervals.append((current, current_high))
                current = current_high+1
        dest_intervals = [
            (self[current_low], self[current_high]) for current_low, current_high in new_intervals
        ]
        return sorted(dest_intervals)
    
    def __repr__(self) -> str:
        return ', '.join([f'<{i}>' for i in self.intervals])



def parse_map(name_and_data: str) -> Mapping:
    # discard the name
    data = name_and_data.split(":")[1].strip()
    lines = data.split("\n")
    dest_source_length = [tuple([int(x) for x in line.split()]) for line in lines]
    return Mapping(dest_source_length)


def eval_map_chain(in_value: int, map_chain: list[Mapping]) -> int:
    current = in_value
    for this_map in map_chain:
        current = this_map[current]
    return current

def chained_map_intervals(input_ranges: list[(int, int)], map_chain: list[Mapping]) -> list[(int, int)]:
    current = input_ranges 
    for this_map in map_chain:
        current = this_map.map_intervals(current)
    return current


def parse_file(filename):
    with open(filename) as f:
        contents = f.read()
    seeds, *map_list = contents.split("\n\n")
    seed_values = [int(s) for s in seeds.split(":")[1].strip().split()]
    map_chain = [parse_map(m_repr) for m_repr in map_list]
    return seed_values, map_chain


def find_smallest_location(seeds, chain: list[Mapping]) -> int:
    return min([eval_map_chain(s, chain) for s in seeds])


def part1(filename):
    seeds, chain = parse_file(filename)
    return find_smallest_location(seeds, chain)

def part1_alt(filename):
    seeds, chain = parse_file(filename)
    return min(chained_map_intervals([(s,s) for s in seeds], chain))[0]

def part2(filename):
    """Too slow!!"""
    numeric_seed_values, chain = parse_file(filename)
    seeds = []
    for i in range(0, len(numeric_seed_values), 2):
        seeds.extend(
            [numeric_seed_values[i] + c for c in range(numeric_seed_values[i + 1])]
        )
    return find_smallest_location(seeds, chain)

def part2_alt(filename):
    seed_and_range, chain = parse_file(filename)
    seeds = []
    for i in range(0, len(seed_and_range), 2):
        current, length = seed_and_range[i], seed_and_range[i+1]
        seeds.append((current, current+length))
    return min(chained_map_intervals(seeds, chain))[0]
