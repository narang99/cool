module Lexer.Helpers where

accumulateErrorsOrResult :: Monoid a1 => [Either a1 a2] -> Either a1 [a2]
accumulateErrorsOrResult [] = Right []
accumulateErrorsOrResult (d:ds) = let
  nextDefs = accumulateErrorsOrResult ds
  in case d of
    Left str -> case nextDefs of
      Left str' -> Left $ str `mappend` str'
      Right _   -> Left str
    Right def -> case nextDefs of
      Left str' -> Left str'
      Right defs   -> Right $ def:defs

-- Takes a function that takes a list, operates on some part of the list
-- returns (result of operation, restOfList)
-- applies the function recursively to restOfList and appends all results
-- in a list
stepAndMapResult :: ([a] -> (b, [a])) -> [a] -> [b]
stepAndMapResult _ [] = []
stepAndMapResult f xs = let
  (res, xs') = f xs
  nextResults = stepAndMapResult f xs'
  in res : nextResults