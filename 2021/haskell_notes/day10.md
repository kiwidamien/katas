# Day 10: Stacks and matching parens

Day 10 was a matching parens problem. As is typical for an advent of code problem, there was a funky numerical scoring system, so you could turn your input into a single number for the scoring. 

The fundamental computer science problem was

> Given a string that consists of the characters `{(<[]>)}`, and the standard definition of matching brackets (of any type) as a balanced string. We distinguish between two types of unbalanced strings:
> (a) unbalanced strings that cannot be prefixes of balanced strings. We call these illegal.
>     e.g. `"(<>[(]["`, as the 5th character `(` is closed by `]`
> (b) unbalanced strings that can be prefixes of balanced strings. We call these incomplete.
>     e.g. `"(<>[(["` is incomplete, as we can add `"])])"` to it and make it complete.

The [advent of code](https://adventofcode.com/2021/day/10) gives the following scoring systems
1. For the illegal strings, the score is dictated by the first illegal character. The scores were
  * First illegal character is `)`, then score is 3 points
  * First illegal character is `]`, then score is 57 points
  * First illegal character is `}`, then score is 1197 points
  * First illegal character is `>`, then score is 25137 points.
  
  So the string `(<>[(][` would have a score of 57, because the first character that makes this string illegal is `]`.

## The computer science part


We have two different implementations of this problem: as a string replacement problem, and as a stack problem. 

In the **stack problem**, the open characters `({<[` get pushed onto the stack as we encounter them. When we see a closing character, if it closes the character on the top of the stack then we can remove the top char. If it doesn't close the character on top of the stack, then we have an illegal string (and have found the first illegal character). If we end with a string that is empty, it was initially a balanced string. If we end with a non-empty stack but no violations, we have a stack we can complete.

This approach requires one traversal of the string, making it O(N). Working through the string `"(<>[(]["`:
1. We start with an empty stack
2. The first character `'('` is an opening. Our stack is now `['(']`.
3. The second character `'<'` is an opening. Our stack is now `['(', '<']`
4. The third character `'>'` is a closing character. The top of the stack is `'<'`, which matches, so we can pop it off the stack. Our stack is now `['(']`
5. The fourth character `'['` is an opening. Our stack is now `['(', '[']`
6. Our fifth character `'('` is an opening. Our stack is now `['(', '[', '(']`
7. Our sixth character `']'` is a closing character. It *doesn't* match the top of the stack, so it throws an error.

A balanced string would get reduced to "", and an incomplete string would end up with a non-empty stack but no errors.

The **string replacement problem** replaces any "cancelling" pair: `"()"`, `"{}"`, `"[]"`, `"<>"`. After a cancellation, a new canceling pair might be made, so we keep cancelling pairs until the string is no longer changing size. If the string was balanced, the reduced string is empty. If the string was illegal, the reduced string will contain one or more closing characters. If the string was incomplete, the reduced string will consist only of opening characters.

To work through an example, consider the string `"<()[<>][>]"`
1. First pass, we replace `()` and `<>` with nothing, so our string is now `<[][>]`
2. Second pass, we replace `[]` and the string is now `<[>]`
3. There is nothing left to replace, and the string contains closing characters `>` and `]`, so the string was illegal.

In step 1 and step 2, we did full passes of the string. So even through it was quicker to describe than the stack solution, the complexity is a lot worse: O(N^2)! A string like `"([([([([([([])])])])])])"` only eliminates two chacarters per pass.


Despite the worse run time, the string problem looks easier and only deals with strings. Stacks are easy to deal with in Python (you can use the vanilla list as a stack) but I was wanting to do them as a second pass in Haskell.

 
### The problem as string reduction (Haskell and Python)

Wwe can do a search and replace on the pairs `"()"`, `"<>"`, `"[]"`, `"{}"` and replace them with nothing. We can continue to do this until the size of the string doesn't change.

Let's see a Python implementation:
```python
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

```

What would this code look like in Haskell? Let's start by implementing the `reduce_string` method
```haskell
reduceString :: String -> String
reduceString s = if (length s == length reduced then reduced else reduceString reduced
  where reduced = eliminateCycle s

-- eliminateCycle is doing the work of the python s.replace(...).replace(...).replace(...).replace(...)
eliminateCycle :: String -> String
eliminateCycle s = (eliminate '(' ')' $ eliminate '<' '>' $ eliminate '{' '}' $ eliminate '[' ']' s)

-- eliminate char1 char2 str should replace all occurances of the substring "{char1}{char2}"
eliminate :: Char -> Char -> String -> String
```
Here `eliminate` is basically our replace function. We could import a solution, but how would we write it instead? 
We need a couple of special cases: if the string is less than 2 characters, no reduction is possible. This is the base case. Here is the code I came up with:
```haskell
eliminate :: Char -> Char -> String -> String
eliminate _ _ "" = ""
eliminate _ _ (c:[]) = [c]
eliminate open close (pot_open:pot_close:rest)
    | pot_open /= open = pot_open:(eliminate open close (pot_close:rest))
    | pot_close == close = eliminate open close rest
    | otherwise = pot_open:(eliminate open close (pot_close:rest))
```

The complete code (minus the scoring and IO parts) is
```haskell
reduceString :: String -> String
reduceString s = if (length s == length reduced) then reduced else reduceString reduced
  where reduced = eliminateCycle s


eliminateCycle :: String -> String
eliminateCycle s = (eliminate '(' ')' $ eliminate '[' ']' $ eliminate '<' '>' $ eliminate '{' '}' s)


eliminate :: Char -> Char -> String -> String
eliminate _ _ "" = ""
eliminate _ _ (c:[]) = [c]
eliminate open close (pot_open:pot_close:rest)
    | pot_open /= open = pot_open:(eliminate open close (pot_close:rest))
    | pot_close == close = eliminate open close rest
    | otherwise = pot_open:(eliminate open close (pot_close:rest))


findFirstIllegalChar :: String -> Char 
findFirstIllegalChar s 
  | (length closed) > 0 = head closed
  | otherwise = ' ' 
  where closed = filter (\x->elem x ")}]>") $ reduceString s


-- Because we were scoring this, I did not map the openings to the closings
-- Instead I just translated the openings to closing when I did the scoring.
autocomplete :: String -> String
autocomplete beg 
  | illegal = "" 
  | otherwise = reverse $ reduced
  where reduced = reduceString beg
        illegal = foldl (||) False $ map (\x->elem x ")}>]") reduced  
```

### A python stack implementation

To detect if a string was illegal or not, we maintained a stack of opening characters.
* If we encounter an opening character `({<[`, then we push it onto the stack
* If we encounter a closing character `)}>]`, then we compare to the top of the stack.
  * If the parens match, we pop the top of the stack (and it is still in a legal state).
  * If the parens don't match, we have triggered an illegal state. The current char is the illegal char.

If we have an empty stack at the end, we have a balanced string. If we have a valid non-empty stack, we have an incomplete string. We can simply add the appropriate closing characters to pop the stack. In Python, the solution would look like this

```python
closed_to_open = {')': '(', ']':'[', '}':'{', '>': '<'}
closed_chars, open_chars = [
    set(char_tup) for char_tup in zip(*closed_to_open.items())
]


def reduce_string(s: str) -> str:
    stack = []
    for char in s:
        if char in open_chars:
            stack.append(char)
        if char in closed_to_open:
            if len(stack)==0:
                return char
            if stack[-1]!=closed_to_open[char]:
                stack.append(char)
                break
            stack.pop()
    return ''.join(stack)


def first_illegal_char(s: str) -> str:
    reduced = reduce_stack(s)
    return reduced[-1] if reduced else ''


def return_completion(s: str) -> str:
    reduced = reduce_stack(s)
    if any(char in closed_chars for char in reduced):
        return ''  # could also raise an Error, as this cannot be completed
    open_to_closed = {v: k for k, v in closed_to_open.items()}
    return ''.join(open_to_closed[char] for char in reduced[::-1])
```

This is an O(N) solution, as we only have to pass through the string once. In terms of design, it is a little problematic that `return_completion` returns a blank string for both a balanced string and an illegal string! We can easily write wrappers like `def is_balanced(s): return reduce_stack(s)==""` if we need a test for balancing the string.

Initially I decided that I didn't want to try and implement a stack in Haskell, so I viewed the problem as a string reduction problem instead.

### The problem as string reduction (Haskell and Python)

A different approach is to take advantage of the string only being open and closed characters. This means we can do a search and replace on the pairs `"()"`, `"<>"`, `"[]"`, `"{}"` and replace them with nothing. We can continue to do this until the size of the string doesn't change.

Let's see a Python implementation:
```python
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

```

The code is shorter, and we are only doing string matching. It is significantly worse asymptotically: O(N^2)! We achieve this worst case by interleaving opening and closing symbols, so we only reduce one pair at a time. For example `"([([([([([([])])])])])])"` is a balanced string, but only two symbols get reduced in every replace loop.

Still, the code looked simpler to translate into Haskell. Let's start off implementing 
