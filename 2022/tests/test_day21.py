import pytest
import sys

sys.path.append('..')

import day21_puzzle as d21


EXAMPLE = """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""


@pytest.fixture()
def example():
    return d21.parse_string(EXAMPLE)


def test_simple_parser():
    simple_contents = "answer: pwer + flex\npwer: 1\nflex: 5\nabba: 6"
    assert d21.evaluate(simple_contents, 'answer') == 6

def test_root_example():
    assert d21.evaluate(EXAMPLE,'root')==152

def test_extended_dag_example():
    ex_dag = d21.parse_string_extended(EXAMPLE)
    num1, num2 = ex_dag.evaluate('root')
    H = 301
    assert num1.real + num1.human * H == num2

