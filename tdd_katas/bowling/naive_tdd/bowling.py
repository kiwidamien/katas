from typing import Tuple, Union, Optional, List

Frame = Union[Tuple[int], Tuple[int, int], Tuple[int, int]]
Game = List[Frame]


def _flatten(frames: List[Frame]):
    return sum([list(frame) for frame in frames], [])


def score_top_frame(start_frame: List[Frame]) -> int:
    top = start_frame[0]
    if top[0] == 10:
        return sum(_flatten(start_frame)[:3])
    if top[0] + top[1] == 10:
        return sum(_flatten(start_frame)[:3])
    return sum(top)


def score_game(game: List[Frame]) -> int:
    score = [score_top_frame(game[index:]) for index in range(len(game))]
    return sum(score)
