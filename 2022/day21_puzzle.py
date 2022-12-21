from dataclasses import dataclass


class DAG:
    def __init__(self, unprocessable=[]):
        self.nodes = {}
        self.eval_types = {int, float}

    def add_node(self, node_name, expression):
        self.nodes[node_name] = expression

    def evaluate(self, node_name):
        if isinstance(node_name, int) or isinstance(node_name, float):
            return node_name

        if node_name not in self.nodes:
            raise KeyError(f'Unknown node: {node_name}')
        expression = self.nodes[node_name]
        if isinstance(expression, int) or isinstance(expression, float):
            return expression
        try:
            expression = int(expression)
            self.nodes[node_name] = expression
            return expression
        except ValueError:
            pass

        try:
            expression = float(expression)
            self.nodes[node_name] = expression
            return expression
        except ValueError:
            pass

        lhs, op, rhs = expression.strip().split(' ')
        lhs_value = self.evaluate(lhs)
        rhs_value = self.evaluate(rhs)
        operation = {
            '+': lambda a, b: a+b,
            '*': lambda a, b: a*b,
            '-': lambda a, b: a-b,
            '/': lambda a, b: a/b
        }[op]
        value = operation(lhs_value, rhs_value)
        self.nodes[node_name] = value
        return self.nodes[node_name]


class ExtendedNumber:
    def __init__(self, real, human):
        self.value = (real, human)
    
    @property
    def real(self):
        return self.value[0]

    @property
    def human(self):
        return self.value[1]

    @classmethod
    def from_plain(cls, other):
        if isinstance(other, cls):
            return other
        return cls(other, 0)

    def __mul__(self, other_real):
        if isinstance(other_real, ExtendedNumber):
            raise ValueError('Have not learned how to deal with fields')
        return ExtendedNumber(other_real*self.real, other_real*self.human)
    
    def __rmul__(self, other_real):
        return self.__mul__(other_real)

    def __add__(self, other):
        if isinstance(other, ExtendedNumber):
            return ExtendedNumber(
                self.real + other.real, self.human + other.human
            )
        return ExtendedNumber(self.real + other, self.human)

    def __radd__(self, other):
        return self.__add__(other)

    def __sub__(self, other):
        if isinstance(other, ExtendedNumber):
            return ExtendedNumber(
                self.real - other.real, self.human - other.human
            )
        return ExtendedNumber(self.real - other, self.human)

    def __rsub__(self, other):
        if isinstance(other, ExtendedNumber):
            return other - self
        return ExtendedNumber(other - self.real, -self.human)

    def __truediv__(self, other):
        if isinstance(other, ExtendedNumber):
            raise ValueError('Have not learned how to deal with fields')
        return ExtendedNumber(self.real / other, self.human / other)
    
    def __repr__(self):
        return f'({self.real} + {self.human} * H)'


class ExtendedDAG(DAG):
    def evaluate(self, node_name):
        if not isinstance(node_name, str):
            return node_name
        if node_name == 'humn':
            return ExtendedNumber(0, 1)
        if node_name not in self.nodes:
            raise KeyError(f'Unknown node: {node_name}')
        expression = self.nodes[node_name]
        if not isinstance(expression, str):
            return expression
        try:
            expression = int(expression)
            self.nodes[node_name] = expression
            return expression
        except ValueError:
            pass

        try:
            expression = float(expression)
            self.nodes[node_name] = expression
            return expression
        except ValueError:
            pass

        lhs, op, rhs = expression.strip().split(' ')
        lhs_value = self.evaluate(lhs)
        rhs_value = self.evaluate(rhs)
        operation = {
            '+': lambda a, b: a+b,
            '*': lambda a, b: a*b,
            '-': lambda a, b: a-b,
            '/': lambda a, b: a/b,
            '=': lambda a, b: (a,b)
        }[op]
        value = operation(lhs_value, rhs_value)
        self.nodes[node_name] = value
        return self.nodes[node_name]


def parse_string(contents):
    dag = DAG()
    for line in contents.split('\n'):
        name, expression = line.split(':')
        dag.add_node(name, expression)
    return dag


def parse_string_extended(contents):
    dag = ExtendedDAG()
    for line in contents.split('\n'):
        name, expression = line.split(':')
        if name == 'humn':
            continue
        if name == 'root':
            expression = (
                expression
                .replace('+', '=')
                .replace('-','=')
                .replace('*', '=')
                .replace('/', '=')
            )
        dag.add_node(name, expression)
    return dag


def evaluate(contents, node_name):
    dag = parse_string(contents)
    return dag.evaluate(node_name)


def parse_file():
    with open('day21_input.txt') as f:
        return f.read()

def prob1_eval_root():
    contents = parse_file()
    return evaluate(contents, 'root')

def prob2_eval_human():
    contents = parse_file()
    ex_dag = parse_string_extended(contents)
    return ex_dag.evaluate('root')


if __name__ == '__main__':
    print(prob1_eval_root())
    result = prob2_eval_human()
    print(result)
    human, number = result
    h = (number - human.real) / human.human
    print(h)
