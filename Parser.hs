module Parser where

type Parser a = String -> Maybe (a, String)

char :: Parser Char
char []     = Nothing
char (x:xs) = Just (x, xs)

alt :: Parser a -> Parser a -> Parser a
(p `alt` q) xs = case (p xs) of
		   Nothing -> q xs
		   x -> x  

(#) :: Parser a -> Parser b -> Parser (a, b)
(p # q) xs = case (p xs) of
               Nothing -> Nothing
               Just (x, ys) -> (case (q ys) of
			          Nothing      -> Nothing
                                  Just (y, zs) -> Just ((x, y), zs))

-- A parser that always succeeds with the given value as the result.
succeed :: a -> Parser a
succeed c xs = Just (c, xs) 

-- Zero or more applications of p
many :: Parser a -> Parser [a]
many p = many1 p `alt` succeed []

-- One or more applications of p
many1 :: Parser a -> Parser [a]
many1 p = p # many p `build` (uncurry (:))

-- Apply a function to the result of applying the given parser.
build :: Parser a -> (a -> b) -> Parser b
build p f xs =
	case (p xs) of
          Just (x, ys) -> Just (f x, ys)
          otherwise -> Nothing
-- Parse using p and keep the result if it satisfies the predicate f.
sat :: Parser a -> (a -> Bool) -> Parser a
(p `sat` f) xs = case (p xs) of
                   Nothing -> Nothing
                   Just (x, ys) -> case (f x) of
                                     True -> Just (x, ys)
                                     False -> Nothing

