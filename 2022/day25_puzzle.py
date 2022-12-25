def decimal_to_snafu(decimal: int) -> str:
    def num_to_digit_and_carry(num: int):
        digit = {
            5: '0',
            4: '-',
            3: '=',
            2: '2',
            1: '1',
            0: '0',
        }[num]
        return digit, (num >=3)

    # convert to base 5
    lowest_power = 1
    while 5**lowest_power <= decimal:
        lowest_power += 1
    lowest_power -= 1

    accumulator = []
    for power in range(lowest_power, -1, -1):
        number = decimal // (5**power) 
        decimal = decimal - number * (5**power)
        accumulator.append(number)

    carry = 0
    snafu_digits = []
    for num in accumulator[::-1]:
        num = num + carry
        digit, carry = num_to_digit_and_carry(num)
        snafu_digits.append(digit)
    if carry == 1:
        snafu_digits.append('1')

    return ''.join(snafu_digits[::-1]) 


def snafu_to_decimal(snafu: str) -> int:
    char_map = {
        '0': 0, '1': 1, '2': 2, '=': -2, '-': -1
    }
    num_power = [char_map[char]*(5**index) for index, char in enumerate(snafu[::-1])]
    return sum(num_power)


def parse_file():
    with open('day25_input.txt') as f:
        return [line.strip() for line in f.readlines()]


def problem1():
    lines = parse_file()
    total_fuel = sum(snafu_to_decimal(n) for n in lines)
    snafu = decimal_to_snafu(total_fuel)
    print(f"Total fuel needed is {total_fuel}, which is represented as '{snafu}' in SNAFU")


if __name__ == '__main__':
    problem1()
