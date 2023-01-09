def reduce_string(s: str) -> str:
    length = -1
    while (length != len(s)):
        length = len(s)
        s = s.replace("()", "").replace("<>", "").replace("{}", "").replace("[]", "")
    return s


def first_illegal_char(s: str) -> str:
    for char in reduce_string(s):
        if char in ")>}]": return char
    return ""


def return_autocompletion(s: str) -> str:
    d = {'{': '}', '(':')', '<':'>', '[':']'}
    try:
        complete = [d[char] for char in reduce_string(s)[::-1]]
    except KeyError:
        return ""  # Could also be an error
    return ''.join(complete)

