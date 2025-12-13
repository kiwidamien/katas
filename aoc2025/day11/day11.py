import functools


def parse(contents: str) -> dict[str, list[str]]:
    adj_list = {'out': []}
    lines = contents.split('\n')
    for line in lines:
        name, pre_edge = line.split(':')
        edges = tuple([e.strip() for e in pre_edge.split()])
        adj_list[name] = edges
    return adj_list


def paths_to_out(start, adj_list, must_go_through=None):
    must_go_through = tuple() if must_go_through is None else must_go_through
    @functools.lru_cache(maxsize=800)
    def _paths_to_out(start: str, must_go_through) -> int:
        if start == "out":
            if len(must_go_through) == 0:
                return 1
            else:
                return 0
        
        total = 0
        if start in must_go_through:
            must_go_through = tuple([c for c in must_go_through if c != start])
        
        for child in adj_list.get(start, []):
            total += _paths_to_out(child, must_go_through=must_go_through)
        return total
    return _paths_to_out(start=start, must_go_through=must_go_through)