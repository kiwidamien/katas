second_player_win_lose_draw = {
  ('A', 'X'): 'D',
  ('A', 'Y'): 'W',
  ('A', 'Z'): 'L',
  ('B', 'X'): 'L',
  ('B', 'Y'): 'D',
  ('B', 'Z'): 'W',
  ('C', 'X'): 'W',
  ('C', 'Y'): 'L',
  ('C', 'Z'): 'D'
}
  

def score_round(opponent: str, you: str) -> int:
    shapes = {key: value for key, value in zip('XYZ', [1,2,3])}
    result = second_player_win_lose_draw[(opponent, you)]
    return shapes[you] + {'W': 6, 'D': 3, 'L':0}[result]


def read_strategy_guide():
    with open("day02_input.txt", "r") as f:
        moves = [moves.split() for moves in f.readlines()]
    return moves


def perfect_strategy_guide():
    moves = read_strategy_guide()
    scores = [score_round(opp, me) for opp, me in moves]
    return sum(scores)

second_player_what_to_play = {
  (first, result): second for (first, second), result in second_player_win_lose_draw.items()
}

def day4_strategy_guide():
    lookup_result = {'X': 'L', 'Y': 'D', 'Z': 'W'}
    moves = [(first, lookup_result[second]) for first, second in read_strategy_guide()]
    scores = [
        score_round(opp, second_player_what_to_play[opp, result])
        for opp, result in moves
    ]
    return sum(scores)


if __name__=='__main__':
    print(f"day3: {perfect_strategy_guide()}")
    print(f"day4: {day4_strategy_guide()}")

