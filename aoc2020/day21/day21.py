import functools
from copy import deepcopy
from typing import Dict, List, Set, Tuple

Ingredient = str
Allergen = str
RecipeInfo = Tuple[Set[Ingredient], Set[Allergen]]


def parse_file(filename: str) -> List[RecipeInfo]:
    with open(filename) as f:
        return [parse_line(line) for line in f.readlines()]


def parse_line(line: str) -> RecipeInfo:
    (ingredients, allergens) = line.strip().strip(")").split("(")
    ingredient_collection = set(ingredients.split())
    allergen_collection = set(
        w.strip() for w in allergens.replace("contains ", "").split(",")
    )
    return (ingredient_collection, allergen_collection)


def find_all_ingredients(recipes: List[RecipeInfo]) -> Set[Ingredient]:
    return functools.reduce(lambda acc, i_set: acc | i_set[0], recipes, set())


def find_possible_ingredients(
    recipes: List[RecipeInfo],
) -> Dict[Allergen, Set[Ingredient]]:
    universe = find_all_ingredients(recipes)
    catalog = {}
    for recipe in recipes:
        ingredients, allergens = recipe
        for allergen in allergens:
            catalog[allergen] = catalog.get(allergen, universe) & ingredients
    return catalog


def find_match_none(recipes: List[RecipeInfo]) -> Set[Ingredient]:
    candidates = find_all_ingredients(recipes)
    for _, ingredients in find_possible_ingredients(recipes).items():
        candidates = candidates - ingredients
    return candidates


def count_occ_safe_ingredients(recipes: List[RecipeInfo]) -> int:
    safe = find_match_none(recipes)
    safe_per_recipe = [len(ingredients & safe) for ingredients, _ in recipes]
    return sum(safe_per_recipe)


def _solve(
    p: Dict[Allergen, Set[Ingredient]], solved: Dict[Allergen, Ingredient]
) -> Dict[Allergen, Ingredient]:
    if len(p) == 0:
        return solved
    new_solved = solved.copy()
    p = deepcopy(p)
    for allergen, ingredients in p.items():
        if len(ingredients) == 1:
            new_solved[allergen] = ingredients.pop()
    eliminated_ingredients = set(new_solved.values())
    new_p = {a: i - eliminated_ingredients for a, i in p.items() if a not in new_solved}
    # Have we arrived at any contradictions?
    if any([len(possibilities) == 0 for possibilities in new_p.values()]):
        return None
    if new_solved != solved:
        return _solve(new_p, new_solved)

    # Begin trial and error
    for allergen, ingredients in new_p.items():
        for i in ingredients:
            attempt = {**new_p}
            del attempt[allergen]
            this_solve = {**new_solved}
            this_solve[allergen] = i
            potential = _solve(attempt, this_solve)
            if potential is not None:
                return potential

    return None


def solve(possibilities: Dict[Allergen, Set[Ingredient]]) -> Dict[Allergen, Ingredient]:
    return _solve(p=possibilities, solved={})


def format_answer(ans: Dict[Allergen, Ingredient]) -> List[str]:
    """Returns the ingredients in the lexographical order of the alergens

                      >>> format_answer({'a': 'zz', 'b': 'mm', 'c':'rr'})
    ['zz', 'mm', 'rr']
    """
    return [ans[allergen] for allergen in sorted(ans.keys())]
