def _topo_order_helper(adj_list: list[list[int]], current: int, stack: list[int], visited: set):
    if current in visited:
        return 
    visited.add(current)
    for neighbor in adj_list[current]:
        _topo_order_helper(adj_list, neighbor, stack, visited)
    stack.append(current)

def topological_order(adj_list: list[list[int]]) -> list[int]:
    stack = []
    visited = set()
    for i, neighbors in enumerate(adj_list):
        _topo_order_helper(adj_list, i, stack, visited)
    return stack[::-1]