# The Bowling Game Kata

Ian Cooper in his talk ["TDD revisited"](https://www.youtube.com/watch?v=vOO3hulIcsY) mentioned the Ron Jefferies ["Bowling Game" kata](https://ronjeffries.com/xprog/articles/miningbowling/)
was a good example to get involved with for real TDD.

I am going to try this, reading as little of Ron's code / solution as possible initially. 
My goal is to just try scoring Bowling with TDD development in my own naive way.

Once I have done that, I will go and revisit Ron's blog, and rewrite it with a more informed and structured approach.
This will allow me to compare how much difference there is between my naive approach and the structured TDD approach.

## The rules

We are going to start with the rules of Bowling and how to score it. Again from [Ron](https://ronjeffries.com/xprog/articles/acsbowling/)

1. Each game, or "line" of bowling, includes 10 turns or "frames"
2. In each frame, the bowler gets up to two tries to knock down the pins.
3. If in two tries, the player fails to knock them down, the player's score is the total number of pins have knocked down
4. If the player knocks down all pins on the first try, this is a strike. The turn is over, and the score for this turn is 10 + the simple total of pins knocked down in th enext gframe
5. If the player knocks down all pints on the second try, this is a spare. The turn is over, and the score for this turn is 10 + the number of pins knocked down on the next roll
6. If the player gets a spare or strike in the last (10th) frame, the bowler gets to throw one or two more bonus balls, respectively. These bonus throws are taken as part of the same turn. If the bonus throws knock down all the pins, the process does not repeat. The bonus throws are only used to calculate the score of the final frame.
7. The game score is the total of all frame scores.

## Attempts

- `naive_tdd`: Started 2022-12-11

