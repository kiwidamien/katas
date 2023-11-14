module Day21 where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

type Ingredient = String

type Allergen = String

type RecipeInfo = (S.Set Ingredient, S.Set Allergen)

allIngredients :: [RecipeInfo] -> S.Set Ingredient
allIngredients info = foldl S.union S.empty $ map fst info

processRecord :: RecipeInfo -> Map Allergen (S.Set Ingredient) -> Map Allergen (S.Set Ingredient)
processRecord (is, as) m = foldl update m aAndI
  where
    aAndI = zip (S.toList as) (repeat is)
    update soFar (allergen, ingredients) = M.insertWith (\acc i -> S.intersection acc i) allergen ingredients soFar

findPossibleIngredients :: [RecipeInfo] -> Map Allergen (S.Set Ingredient)
findPossibleIngredients infoList = foldr (processRecord) M.empty infoList

findNonAllergens :: [RecipeInfo] -> (S.Set Ingredient)
findNonAllergens info = S.difference (allIngredients info) potential
  where
    possible = findPossibleIngredients info
    potential = foldr S.union S.empty $ M.elems possible

countConfirmedSafeIngredients :: [RecipeInfo] -> Int
countConfirmedSafeIngredients info = sum $ map (countInRecipe safeIngredients) info
  where
    countInRecipe :: (S.Set Ingredient) -> RecipeInfo -> Int
    countInRecipe safe r = length $ filter (\x -> elem x safe) $ S.toList $ fst r
    safeIngredients = findNonAllergens info

sepAt :: Char -> String -> [String]
sepAt _ [] = []
sepAt sep stuff = let (ff, ss) = break (== sep) stuff in ff : (sepAt sep $ drop 1 ss)

{-
 - Backtracking section
-}
simplify :: (Map Allergen Ingredient, Map Allergen (S.Set Ingredient)) -> (M.Map Allergen Ingredient, M.Map Allergen (S.Set Ingredient))
simplify (sol, poss) = if (newPoss == poss) then (sol, poss) else simplify (newSol, newPoss)
  where
    singleElem = M.filter (\lst -> 1 == length lst) poss
    addToSoltn = M.map (head . S.toList) singleElem
    newSol = M.union sol addToSoltn
    eliminated = M.elems newSol
    newPoss = M.filter (\x -> length x > 0) $ M.map (\lst -> S.filter (\i -> not (elem i eliminated)) lst) poss

{-
 - Parsing section
-}
parseLine :: String -> RecipeInfo
parseLine line = (S.fromList ingredients, S.fromList allergens)
  where
    (pre_ing, pre_all) = break (== '(') line
    ingredients = map (filter (/= ' ')) $ sepAt ' ' pre_ing
    allergens = map (filter (\x -> not $ (elem x " ()"))) $ sepAt ',' $ drop (length "(contains ") pre_all

part2 :: [RecipeInfo] -> String
part2 info = foldl joinComma "" $ map snd $ M.toAscList solution
  where
    solution = fst $ simplify (M.empty, findPossibleIngredients info)
    joinComma s word = if s == "" then word else s ++ "," ++ word

main :: String -> IO ()
main fname = do
  input <- lines <$> readFile fname
  let recipes = map parseLine input
  print (countConfirmedSafeIngredients recipes)
  print (part2 recipes)


example =
  [ (S.fromList $ words "grain flour milk", S.fromList $ words "dairy gluten"),
    (S.fromList $ words "flour salt sauce", S.fromList $ words "gluten fish"),
    (S.fromList $ words "milk salt", S.fromList $ words "dairy")
  ]
