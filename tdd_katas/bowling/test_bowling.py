"""
From the rules:
    * We a ROLL (each time the player sends the ball down  the XXX)
    * A turn or FRAME consists of 1 roll (a stike), 2 rolls (standard), or 3 rolls (final frame with a strike or spare)
    * A non-final turn:
        - If the person knocks a 10, on first roll
          - the turn ends          - the score is 10 + the number of pins knocked down on the next two rolls
        - If the person knocks down 10 pins over two rolls
          - the turn ends
          - the score is 10 + the number of pins knocked down on the next roll
        - If the person knocks down less than 10 pins on two rolls
          - the turn ends
          - the score is the number of pins knowkced down
    * A final turn:
        - If ten knocked down on first turn, bowl two more times. The score is the number of pins on all three rolls (i.e. max 30)
        - OTHERWISE if ten knocked down by second turn, bowl one more time. The score is the total number of pins on all three rolls (max 29)
        - OTHERWISE there are two rolls

Note that the article excludes determining if we have a well formatted game from the spec. This means some tests I would write are not present.
We would probably include
    - raise an error if any roll scores more than 10
    - raise an error if any frame (except the last) knocks down more than 10 in any frame
    - raise an error if any list of frames is longer than 10 (10 should be the entire game)
    - raise an error if any of the values are negative
    - raise an error if any frame is empty
    - raise an error if the length of a frame that is not the final frame is not 1 or 2
    - raise an error if the length of a final frame is not 2 or 3
"""
import bowling
import pytest

from typing import List


@pytest.mark.parametrize(
    "start_frame,expected",[
        [[(1, 2), (3, 4)], 3],
        [[(10, ), (2, 7)], 19],
        [[(10, ), (10, ), (10, )], 30],
        [[(10, ), (7, 2)], 19],
        [[(5, 5), (2, 7)], 12],
        [[(5, 5), (7, 2)], 17],
        [[(0, 0), (9, 1)], 0],
        [[(0, 0), (10, )], 0],
        [[(10, ), (0, 3)], 13],
    ]
)
def test_non_final_frames(start_frame: List[bowling.Frame], expected: int):
    assert bowling.score_top_frame(start_frame=start_frame) == expected

@pytest.mark.parametrize(
    "this_frame,expected",[
      [(10, 3, 2), 15],
      [(5, 5, 3), 13],
      [(1, 3), 4]
    ]
)
def test_final_frames(this_frame: bowling.Frame, expected: int):
    assert bowling.score_top_frame(start_frame=[this_frame]) == expected


def test_score_perfect_game():
    game = [(10,) for _ in range(9)]
    game.append((10, 10, 10))
    assert bowling.score_game(game=game)==300
