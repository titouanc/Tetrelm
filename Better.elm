module Better exposing (..)

(?>) : Maybe a -> (a -> b) -> Maybe b
(?>) mx f = case mx of 
  Just x  -> Just (f x)
  Nothing -> Nothing
(<?) = flip (?>)
infixr 2 <?
infixr 2 ?>

-- Monadic Maybe
(>>?) : Maybe a -> (a -> Maybe b) -> Maybe b
(>>?) mx f = case mx of
    Just x -> f x
    Nothing -> Nothing
(?<<) = flip (>>?)
infixr 1 >>?
infixr 1 ?<<