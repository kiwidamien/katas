# Advent Of Code 2022

For this year's Advent of Code, I focused on developing two skills:

* Test Driven Development in Python
* Learning Haskell

Since this is my first foray into Haskell, some of the problems that require a lot of 
heavy IO processing to get started are going to be more frustrating than enlightening.
I am making a table of problems that includes a list of whether the problem is suitable
for a beginning Haskeller, and whether I have done that particular problem.

## Testing

### Comments on TDD

I have used pytest quite a bit at work. One of the challenges of TDD (in particular, strict
adherence to the Red-Green-Refactor methods) is that your test suite can be quite brittle.

That is, the tests can be tightly coupled to the implementation, because (despite what Robert Martin
says) the Red-Green-Refactor method means that you test all the code you write, so helper functions also 
have tests. If you eliminate these helper functions, you also have to re-jigger the tests.

Bob Martin's approach to this is "of course you have to refactor", but if every major change is leading
to a refactor (including of the tests) then you end up losing the confidence that your tests are passing
because you are not changing behavior.

[Ian Cooper](https://www.youtube.com/watch?v=vOO3hulIcsY) has a great talk on the brittleness of tests.
I am using the AoC to test the idea of writing interfaces (e.g. high level functions or "API") that I want
for my problem, and I think will be the right one regardless of how requirements evolve.

AoC is really nice for this because
* We have explicit test cases in the examples (example driven tests)
* Sometimes going from part 1 to part 2 will only require performance improvements (e.g. problem space gets a olt bigger), and if done right I won't have tests coupled to the specific data structure
* Other times, the requirements will drastically change (e.g. going from a DAG solver to an algebraic solver). This is a good stress test

I will admit that in some of the intermediate steps, where I have encountered problems, it has been difficult to debug using the tests without doing coupling to my private API. Sometimes I have dropped into a REPL to experiment, sometimes I have created a test and deleted it once I have debugged.

### Testing extensions

There are a couple of problems that have looked like they would be a good tes bed for using property-based testing and hypothesis.

The circular linked list problem on day 21 is a good example. There are properties such as "move forward n places and then back n places should leave the list invariant" or "move forward by the length of the list should leave the list invariant" (it is actually the length of the list - 1, as we don't jump the element being moved).

By the time I have seen a problem and identified hypothesis as an interesting experiment, I have already solved the problem. It might be interesting to go back through and add these property-based tests in as a way of building that muscle.

## Haskell

Keeping a collection of resources here that I find useful for getting started with the AoC style problems.

* [Some tips and tricks for doing Advent of Code with Haskell](https://wjwh.eu/posts/2022-11-30-haskell-aoc-tricks.html), updated 2022-11-30
* [Learning Haskell via Advent of Code](https://ndreynolds.com/posts/2019-02-05-learning-haskell-via-advent-of-code.html), updated 2019-02-05


## Table of problems

| Day | Name | Problem Type | Easyish for Haskell | Haskell done |
|-----|------|--------------|---------------------|--------------|
| 01  | Calorie Counting | Simple sums  | Yes | Yes |
| 02  | Rock Paper Scissors | Rock-Paper-Scissors | Yes, particularly for maps | No |
| 03  | Rugsack Reorg | Search for duplicates | Yes | Yes |
| 04  | Camp Cleanup | Overlaps | Yes | Yes |
| 05  | Supply Stacks | Stack manipulation | No | No |
| 06  | Tuning Trouble | Search for duplicates | Yes | Yes |
| 07  | No space on device | Tree | No | No |
| 08  | Treetop Tower | Grid Search | Yes | No |
| 09  | Rope Bridge | State Machine | Yes | No |
| 10  | Cathode-Ray Tubes  | State Machine | Yes | No |
| 11  | Monkey in the Middle | State Machine | No  | No |
| 12  | Hill Climbing | BFS | Yes | No |
| 13  | Distress Signal | Parser | Yes | No |
| 14  | Regolith Reservoir | State Machine | ?? | No |
| 15  | Beacon Exclusion Zone | Geometry | ?? | No |   
| 16  | Proboscidea Volcanium | BFS (implemented as DFS!) | ?? | No |
| 17  | Pyroclastic Flow (Tetris) | State Machine | ?? | No |
| 18  | Boiling Boulders | Geometry, flood fill | Yes | No |
| 19  | Not Enough Minerals | BFS | Yes | No |
| 20  | Grove Positioning System | Circular linked list | Yes | No |
| 21  | Monkey Math | DAGs | Yes | No |
| 22  | Monkey Map  | Geometry and State Machine | ?? | No |
| 23  | Unstable Diffusion | State Machine | Yes | No |
| 24  | Blizzard Basin | Shortest Path + State Machine | ?? | No |
| 25  | ?? | ?? | ?? | No |

