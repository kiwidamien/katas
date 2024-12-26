from functools import lru_cache


tokens = ("r", "wr", "b", "g", "bwu", "rb", "gb", "br")
examples = [
    "brwrr",
    "bggr",
    "gbbr",
    "rrbgbr",
    "ubwu",
    "bwurrg",
    "brgr",
    "bbrgwb"]

@lru_cache()
def countWays(tokens: tuple[str], message: str) -> int:
    if message=="":
        return 1
    ways = 0
    for t in tokens:
        if message.startswith(t):
            ways = ways + countWays(tokens, message[len(t):])
    return ways


def countWaysExplicit(tokens: tuple[str], message: str, cache={"":1}) -> int:
    if message in cache:
        return cache[message]
    ways = 0
    for t in tokens:
        if message.startswith(t):
            ways = ways + countWaysExplicit(tokens, message[len(t):], cache)
    cache[message] = ways
    return cache[message]


def parse(s: str) -> (tuple[str], list[str]):
    lines = s.split("\n")
    tokens = tuple([word.strip() for word in lines[0].split(",")])
    rest = [line for line in lines[1:] if len(line) > 0]
    return (tokens, rest)
