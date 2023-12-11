from functools import reduce
from typing import Set

graph = [
    ("a", "b", 10),
    ("a", "c", 20),
    ("a", "d", 30),
    ("a", "e", 40),
    ("b", "c", 5),
    ("b", "d", 8),
    ("c", "d", 12),
    ("d", "e", 15),
]


def universe(graph) -> Set[str]:
    return reduce(lambda acc, new: acc.union({new[0], new[1]}), graph, set())


def prim(graph):
    """Returns a minimal spanning tree"""

    def edge_contains(edge, v):
        return v in {edge[0], edge[1]}

    def edge_crossed(edge, visited):
        return (edge[0] in visited) ^ (edge[1] in visited)

    if graph == []:
        return []
    all_nodes = universe(graph)
    num_nodes = len(all_nodes)
    current = all_nodes.pop()
    visited = {current}
    tree = []
    sorted_edges = sorted(graph, key=lambda e: e[2])

    while len(visited) < num_nodes:
        sorted_edges = [
            edge
            for edge in sorted_edges
            if (edge[0] not in visited) or (edge[1] not in visited)
        ]
        for current in sorted_edges:
            if edge_crossed(current, visited):
                break
        if not edge_crossed(current, visited):
            raise ValueError("cannot make tree")
        visited.update([current[0], current[1]])
        tree.append(current)
    return tree


def prim2(graph):
    if graph == []:
        return []
    all_nodes = universe(graph)
    num_nodes = len(all_nodes)

    adj_list = {n: [] for n in all_nodes}
    current = all_nodes.pop()
    visited = set()
    for e in graph:
        adj_list[e[0]].append((e[2], e[1]))
        adj_list[e[1]].append((e[2], e[0]))
    candidate_edges = []
    tree = []
    while len(visited) < num_nodes:
        visited.add(current)
        new_edges = [
            (dist, current, b) for (dist, b) in adj_list[current] if b not in visited
        ]
        candidate_edges = sorted(
            [
                (d, a, b)
                for (d, a, b) in candidate_edges
                if (a != current) and (b != current)
            ]
            + new_edges
        )
        if len(candidate_edges) == 0:
            return tree
        (d, a, b) = candidate_edges[0]
        tree.append((a, b, d))
        current = a if current == b else b
    return tree


def large_graph(size: int):
    import random

    graph = []
    p = 0.3
    for n in range(size - 1):
        graph.append((n, n + 1, random.randint(1, 100)))
        for n2 in range(n, size - 1):
            if random.random() < p:
                graph.append((n, n2, random.randint(1, 100)))
    return graph


def prim3(graph):
    if len(graph) < 1:
        return []
    all_nodes = universe(graph)
    score = {n: float("inf") for n in all_nodes}
    edges = {}
    num_nodes = len(all_nodes)

    current = all_nodes.pop()
    visited = {current}
    current_edge_set = []

    while len(visited) < num_nodes:
        current_edge_set.extend(
            [e for e in graph if (e[0] in visited) ^ (e[1] in visited)]
        )
        current_edge_set = sorted(current_edge_set, key=lambda e: e[2])
        new_edge = current_edge_set[0]
        current_edge_set = current_edge_set[1:]

        current = new_edge[0] if new_edge[0] not in visited else new_edge[1]
        visited.add(current)
        edges[current] = new_edge
    return edges


def prim4(graph):
    """Returns a minimal spanning tree"""

    def edge_contains(edge, v):
        return v in {edge[0], edge[1]}

    def edge_crossed(edge, visited):
        return (edge[0] in visited) ^ (edge[1] in visited)

    if graph == []:
        return []
    all_nodes = universe(graph)
    num_nodes = len(all_nodes)
    current = all_nodes.pop()
    visited = {current}
    tree = []
    sorted_edges = sorted(graph, key=lambda e: e[2])

    while len(visited) < num_nodes:
        sorted_edges = [
            edge
            for edge in sorted_edges
            if not ((edge[0] in visited) and (edge[1] in visited))
            # and not edge_contains(edge, visited)
        ]
        for current in sorted_edges:
            if edge_crossed(current, visited):
                break
        if not edge_crossed(current, visited):
            raise ValueError("cannot make tree")
        visited.update([current[0], current[1]])
        tree.append(current)
    return tree


if __name__ == "__main__":
    import time

    graph = large_graph(400)
    print(len(graph))
    start = time.time()
    for _ in range(10):
        prim(graph)
    end = time.time()
    print(f"Prim: {(end-start)/10}")

    start = time.time()
    for _ in range(10):
        prim2(graph)
    end = time.time()
    print(f"Prim: {(end-start)/10}")

    start = time.time()
    for _ in range(10):
        prim4(graph)
    end = time.time()
    print(f"Prim: {(end-start)/10}")
    print(prim2(graph))
