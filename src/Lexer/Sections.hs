module Lexer.Sections where

import qualified Data.Char as C
import qualified Data.Text.Lazy as TL
import Lexer.TokenParser
import Lexer.Tokens

data LexSection = DefinitionSection | RuleSection | SubRoutineSection
  deriving (Eq, Enum, Show)

data Definition = Definition
  { name :: LexToken,
    value :: [LexToken]
  }
  deriving (Show)

data Rule = Rule
  { pattern :: [LexToken],
    code :: LexToken
  }
  deriving (Show)

data SubRoutine = SubRoutine LexToken deriving (Show)

-- pass list of lines of text
parseDefinition :: [TL.Text] -> (Either String Definition, [TL.Text])
parseDefinition [] = (Left "Error: empty line definition is not allowed", [])
parseDefinition (l : ls) =
  let twoWords = TL.words l
      def = parseLHSandRHS twoWords
   in case def of
        Left str -> (Left str, ls)
        Right def' -> (Right def', ls)
  where
    parseLHSandRHS [w1, w2] =
      let nameToken = parseCLikeIdentifier w1
          valueToken = parsePattern w2
       in case nameToken of
            Left n -> Left n
            Right nameToken' -> case valueToken of
              Left n' -> Left n'
              Right valueToken' -> Right $ Definition {name = nameToken', value = valueToken'}
    parseLHSandRHS l =
      Left $ "Invalid definition: Can have only two words" ++ show l

parseRule :: [TL.Text] -> (Either String Rule, [TL.Text])
parseRule [] = (Left "Error: Cant find rule, empty line", [])
-- if first column is empty in first line, error
-- if first column not empty: patter start
-- subsequent lines as long as columns are non empty they are treated as code
parseRule (l : ls) = case TL.uncons l of
  Nothing -> (Left "Error: cant have an empty line as rule\n", [])
  Just (c, _) -> if C.isSpace c 
    then (Left "Error: cant have a rule starting with space", [])
    else 
      let (firstWord, rest) = TL.break C.isSpace l
          (restOfCode, nextLines) = getUntilLineHasCode ls
          pat' = parsePattern firstWord
          codeForRule =
            LexToken
              { lexeme = rest `TL.append` restOfCode,
                token = Code
              }
      in case pat' of
            Left str -> (Left str, nextLines)
            Right pat ->
              ( Right
                  ( Rule
                      { pattern = pat,
                        code = codeForRule
                      }
                  ),
                nextLines
              )
    where
      getUntilLineHasCode :: [TL.Text] -> (TL.Text, [TL.Text])
      getUntilLineHasCode [] = (TL.empty, [])
      getUntilLineHasCode all@(l : ls) = case TL.uncons l of
        -- Not checking Nothing, need to trim at the start
        Just (c, _) ->
          if C.isSpace c
            then
              let (restOfCode, nextLines) = getUntilLineHasCode ls
               in (l `TL.append` restOfCode, nextLines)
            else (TL.empty, all)

parseSubroutine :: [TL.Text] -> (Either String SubRoutine, [TL.Text])
parseSubroutine lines =
  let wholeText = TL.concat lines
      code =
        LexToken
          { token = Code,
            lexeme = wholeText
          }
   in (Right $ SubRoutine code, [])