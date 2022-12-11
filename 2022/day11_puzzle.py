import re
from dataclasses import dataclass
from functools import reduce
from typing import Callable, List, Tuple


#Monkey = collections.namedtuple("Monkey", "items operation div true_monkey false_monkey")

@dataclass
class Monkey:
    items: List[int]
    operation: Callable[[int], int]
    div: int
    true_monkey: int
    false_monkey: int

def parse_operation(operation: str) -> Callable[[int], int]:
    return lambda old: eval(operation)


def parse_monkey(monkey: str):
    pattern = (
        r"Monkey (\d+):\n\s*Starting items: (.*)\s+Operation: new = (.*)\n\s+Test: divisible by (\d+)\n"
        r"\s+If true: throw to monkey (\d+)\n\s+If false: throw to monkey (\d+)"
    )
    items, operation, div, true_monkey, false_monkey = re.search(pattern, monkey).groups()[1:]
    items = [int(num.strip()) for num in items.split(',')]
    div = int(div)
    true_monkey, false_monkey = int(true_monkey), int(false_monkey)
    return Monkey(items=items, operation=parse_operation(operation), div=div, true_monkey=true_monkey, false_monkey=false_monkey)

    
def monkey_inspection_with_relief(monkey: Monkey, product) -> List[Tuple[int, int]]:
    transfers = []
    for item in monkey.items:
        new_worry_level = monkey.operation(item) // 3
        recieve_monkey = monkey.true_monkey if (new_worry_level % monkey.div == 0) else monkey.false_monkey
        transfers.append((recieve_monkey, new_worry_level))
    return transfers


def monkey_inspection_no_relief(monkey: Monkey, product) -> List[Tuple[int, int]]:
    transfers = []
    for item in monkey.items:
        new_worry_level = monkey.operation(item) % product
        recieve_monkey = monkey.true_monkey if (new_worry_level % monkey.div == 0) else monkey.false_monkey
        transfers.append((recieve_monkey, new_worry_level))
    return transfers


def do_round(monkeys: List[Monkey], inspections: List[int], product: int,  
    monkey_transfer = monkey_inspection_with_relief) -> Tuple[List[Monkey], List[int]]:
    for source_monkey, monkey in enumerate(monkeys):
        transfers = monkey_transfer(monkey, product=product)
        inspections[source_monkey] += len(monkey.items)
        monkey.items = []
        for transfer in transfers:
            new_monkey, new_worry = transfer
            monkeys[new_monkey].items.append(new_worry)
    return monkeys, inspections


def do_many_rounds(monkeys: List[Monkey], inspections: List[int], n_rounds, monkey_transfer, product) -> List[int]:
    for _ in range(n_rounds):
        monkeys, inspections = do_round(monkeys, inspections, monkey_transfer=monkey_transfer, product=product)
    return inspections


def monkey_business(inspections: List[int]) -> int:
    biggest, second = sorted(inspections, reverse=True)[:2]
    return biggest * second


lines = """
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1""".split("\n\n")


def parse_file():
    with open('day11_input.txt', 'r') as f:
        contents = [parse_monkey(monkey) for monkey in f.read().split("\n\n")]
    return contents


monkeys = parse_file() #[parse_monkey(monkey) for monkey in lines]
product = reduce(lambda a, b: a*b, [m.div for m in monkeys], 1)

inspections = [0]*len(monkeys)
updated_inspections = do_many_rounds(monkeys, inspections, n_rounds=20, monkey_transfer=monkey_inspection_with_relief, product=product)
print(monkey_business(updated_inspections))

monkeys = parse_file() #[parse_monkey(monkey) for monkey in lines]
inspections = [0]*len(monkeys)
updated_inspections = do_many_rounds(monkeys, inspections, n_rounds=10_000, monkey_transfer=monkey_inspection_no_relief, product=product)
print(monkey_business(updated_inspections))