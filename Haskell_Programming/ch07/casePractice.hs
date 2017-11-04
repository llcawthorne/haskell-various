-- casePractice.hs
module CasePractice where

functionC x y = case x > y of
  True -> x
  _    -> y

ifEvenAdd2 n = case even n of
  True -> n + 2
  _    -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    _  -> 0
