import random 
from functools import reduce


def parse(contents: str):
    lines = contents.split("\n")
    edges = {}
    for line in lines:
        vertex, rest = line.split(':')
        edges[vertex] = rest.split()

    vertex_set = set(edges.keys()) | set(sum(edges.values(), []))
    for v in vertex_set:
        if v not in edges:
            edges[v] = []
    return edges


def contract(multi_graph: dict[str, list[str]]):
    edge_list = sum([[(key, v) for v in values] for key, values in multi_graph.items()], [])
    (src,dest) = random.choice(edge_list)
    new_vertex = f"{src}%{dest}"

    multi_graph = multi_graph.copy()

    multi_graph[new_vertex] = (
        [node for node in multi_graph[src] if node != dest] + 
        [node for node in multi_graph[dest] if node != src]
    )
    del multi_graph[src]
    del multi_graph[dest]

    for n, lst_vertex in multi_graph.items():
        if n in [src, dest]:
            continue
        multi_graph[n] = [v if v not in [src, dest] else new_vertex for v in lst_vertex]
    return multi_graph


def karger_cuts(multi_graph, t:int = 2):
    if t < 2:
        raise ValueError("Must have at least two components at end")
    multi_graph = multi_graph.copy()
    while len(multi_graph) > t:
        multi_graph = contract(multi_graph)
    return multi_graph

def get_3_cut(multi_graph, t=2):
    while True:
        new_graph = karger_cuts(multi_graph, t=t)
        num_edges = sum([len(v) for v in new_graph.values()])
        print(num_edges)
        if num_edges == 3:
            break
    cmp_sizes = [len(node_list.split('%')) for node_list in new_graph]
    return new_graph, cmp_sizes

def part1(filename: str):
    graph = parse(open(filename).read())
    _, cmp_sizes = get_3_cut(graph)
    print(cmp_sizes)
    ans = reduce(lambda a,n :a*n, cmp_sizes, 1)
    print(ans)


exInput = "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr"
