module Lexer.Tokens
  ( TokenType (..),
    textToToken,
    Lexeme (..),
    LexToken (..),
  )
where

import qualified Data.Text.Lazy as TL

{-
  Special Character pattern types: ^ . + ? * $ " ( ) [ ] | , literal
  Special Characters for code block: {}
-}

data TokenType
  = Dot -- Special character pattern
  | Exponent -- Special character pattern
  | Dash -- Special character pattern
  | Plus -- Special character pattern
  | QMark -- Special character pattern
  | Asterik -- Special character pattern
  | Dollar -- Special character pattern
  | DQoute -- Special character pattern
  | OBrac -- Special character pattern
  | CBrac -- Special character pattern
  | OParen -- Special character pattern
  | CParen -- Special character pattern
  | OBrace -- Special character code
  | CBrace -- Special character code
  | Identifier -- set of letters used for definition
  | Code -- set of lines, unstructured to the lexer
  | Letter -- also includes escaped characters
  | Literal -- set of literals inside double quotes in pattern
  | Unknown -- Invalid erronous token
  deriving (Show, Eq)

type Lexeme = TL.Text

data LexToken = LexToken
  { token :: TokenType,
    lexeme :: Lexeme
  }
  deriving (Show)

-- NOTE: Only send text of max length 2 to this function
--       More than that and it will give erronous result
textToToken :: TL.Text -> TokenType
textToToken text = case TL.uncons text of
  Nothing -> Unknown
  Just ('\\', rest) -> escapedTextToToken rest
  Just (c, rest) ->
    if TL.null rest
      then charToToken c
      else Unknown

-- for special characters
charToToken :: Char -> TokenType
charToToken '.' = Dot
charToToken '+' = Plus
charToToken '?' = QMark
charToToken '*' = Asterik
charToToken '$' = Dollar
charToToken '\"' = DQoute
charToToken '[' = OBrac
charToToken ']' = CBrac
charToToken '(' = OParen
charToToken ')' = CParen
charToToken '{' = OBrace
charToToken '}' = CBrace
charToToken '^' = Exponent
charToToken '-' = Dash
charToToken _ = Letter

-- If the text length is not 1 return Unknown else Letter
-- not using length due to efficiency
escapedTextToToken :: TL.Text -> TokenType
escapedTextToToken text = case TL.uncons text of
  Nothing -> Unknown
  Just (_, rest) -> case TL.uncons rest of
    Nothing -> Letter
    Just (_, _) -> Unknown