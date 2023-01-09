eliminate :: Char -> Char -> String -> String
eliminate _ _ "" = ""
eliminate _ _ (c:[]) = [c]
eliminate open close (pot_open:pot_close:rest)
    | pot_open /= open = pot_open:(eliminate open close (pot_close:rest))
    | pot_close == close = eliminate open close rest
    | otherwise = pot_open:(eliminate open close (pot_close:rest))
