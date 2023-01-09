import Data.List

data MaybeStack = Stack String | Error Char deriving Show


reduceString :: String -> MaybeStack 
reduceString s = foldl' addCharToStack (Stack "") s


addCharToStack :: MaybeStack -> Char -> MaybeStack
addCharToStack (Error c) _ = Error c
addCharToStack (Stack "") c
  | elem c "({<[" = Stack [c]
  | elem c ")}>]" = Error c
addCharToStack (Stack (top:rest)) c
  | elem c "({<[" = Stack (c:top:rest)
  | elem c ")}>]" && matches = Stack rest
  | elem c ")}>]" && not matches = Error c
    where pairs = ["()", "[]", "<>", "{}"]
          matches = elem [top, c] pairs


firstIllegalChar :: String -> Maybe Char
firstIllegalChar s = f $ reduceString s
  where f (Error c) = Just c
        f (Stack s) = Nothing


-- Like in the Python implementation, we have not changed openings to closings.
-- The autocomplete string tells you the openings you need to close
-- e.g. autocomplete "([<" returns Just "<[(", the correct autocomplete is ">])"
autocomplete :: String -> Maybe String
autocomplete prefix = f $ reduceString prefix
  where f (Error c) = Nothing
        f (Stack s) = Just reverse s

