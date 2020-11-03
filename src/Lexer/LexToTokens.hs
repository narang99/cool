module Lexer.LexToTokens
  ( lexFileToTokens,
    lexToTokens,
    LexStructure (..),
  )
where

import qualified Data.Text.Lazy as TL
import qualified Lexer.FileIO as FIO
import Lexer.Helpers
import Lexer.Sections
import System.IO

data LexStructure = LexStructure
  { definitions :: [Definition],
    rules :: [Rule],
    subroutine :: [SubRoutine]
  } deriving (Show)

lexFileToTokens :: FilePath -> IO (Either String LexStructure)
lexFileToTokens path = do
  handle <- openFile path ReadMode
  lexerInput <- FIO.readFile handle
  return $ lexToTokens lexerInput

lexToTokens :: TL.Text -> Either String LexStructure
lexToTokens input =
  convertLexLineToTokens $ TL.lines input

parseDefinitions :: [TL.Text] -> Either String [Definition]
parseDefinitions defs =
  accumulateErrorsOrResult . parseDefinitions' $ defs
  where
    parseDefinitions' = stepAndMapResult parseDefinition

parseRules :: [TL.Text] -> Either String [Rule]
parseRules rls =
  accumulateErrorsOrResult . parseRules' $ rls
  where
    parseRules' = stepAndMapResult parseRule

parseSubroutines :: [TL.Text] -> Either String [SubRoutine]
parseSubroutines sbrts =
  accumulateErrorsOrResult . parseSubroutines' $ sbrts
  where
    parseSubroutines' = stepAndMapResult parseSubroutine

-- NOTE: This function needs to accumulate all errors into one finally
-- Right now it is only returning the first error
-- TODO: make it accumulate the errors
accumulateSections ::
  Either String [Definition] ->
  Either String [Rule] ->
  Either String [SubRoutine] ->
  Either String LexStructure
accumulateSections (Left a) _ _ = Left a
accumulateSections _ (Left b) _ = Left b
accumulateSections _ _ (Left c) = Left c
accumulateSections (Right a) (Right b) (Right c) =
  Right $
    LexStructure
      { definitions = a,
        rules = b,
        subroutine = c
      }

convertLexLineToTokens :: [TL.Text] -> Either String LexStructure
convertLexLineToTokens lines =
  let sections = splitInSections lines
   in case sections of
        Left str -> Left str
        Right (defs, rules, sbroutines) ->
          let parsedDefs = parseDefinitions defs
              parsedRules = parseRules rules
              parsedSubroutine = parseSubroutines sbroutines
           in accumulateSections parsedDefs parsedRules parsedSubroutine
  where
    splitInSections :: [TL.Text] -> Either String ([TL.Text], [TL.Text], [TL.Text])
    splitInSections lines = case splitAtDoublePercentage lines of
      [defs, rules, sbroutines] -> Right (defs, rules, sbroutines)
      _ -> Left "Error: The number of sections should be 3: Definitions, Rules, Subroutines"

isSectionDelimiter :: TL.Text -> Bool
isSectionDelimiter line = (TL.strip line == TL.pack "%%")

splitAtDoublePercentage :: [TL.Text] -> [[TL.Text]]
splitAtDoublePercentage lines =
  let (curAcc, acc) = splitAtDoublePercentage' lines
   in curAcc : acc
  where
    splitAtDoublePercentage' :: [TL.Text] -> ([TL.Text], [[TL.Text]])
    splitAtDoublePercentage' [] = ([], [])
    splitAtDoublePercentage' (l : ls) =
      let (nextRunningAcc, acc) = splitAtDoublePercentage' ls
       in if not . isSectionDelimiter $ l
            then (l : nextRunningAcc, acc)
            else ([], nextRunningAcc : acc)