module Lexer.TokenParser
  ( parseCLikeIdentifier,
    parsePattern,
  )
where

import qualified Data.Char as C
import qualified Data.Text.Lazy as TL
import Lexer.Tokens

-- can use C-like variables, for now only ascii allowed
parseCLikeIdentifier :: TL.Text -> Either String LexToken
parseCLikeIdentifier iden =
  if checkIfValidIdentifier iden
    then Right $ LexToken {token = Identifier, lexeme = iden}
    else Left $ "Invalid Identifier here: " ++ show iden
  where
    checkIfValidIdentifier iden = case TL.uncons iden of
      Nothing -> False
      Just (c, rest) ->
        if C.isAlpha c && TL.all C.isAlphaNum rest
          then True
          else False

listOfUnits :: TL.Text -> [TL.Text]
listOfUnits text = case TL.uncons text of
  Nothing -> []
  Just (c, rest) ->
    if isEscapeSequence c
      then case TL.uncons rest of
        -- a single backslash is invalid
        -- the parser in tokens will mark it as invalid
        -- Right now we just pass it as is
        Nothing -> singletonBackslash
        Just (escaped, rest') ->
          createEscapedSequence escaped : listOfUnits rest'
      else textFromChar c : listOfUnits rest
  where
    singletonBackslash = ['\\' `TL.cons` TL.empty]
    isEscapeSequence = (== '\\')
    createEscapedSequence c = '\\' `TL.cons` c `TL.cons` TL.empty
    textFromChar c = (c `TL.cons` TL.empty)

createLiterals ::
  Either String [LexToken] ->
  Either String [LexToken]
createLiterals (Left str) = Left str
createLiterals (Right tokList) = createLiterals' tokList
  where
    extractTillNextDQoute :: [LexToken] -> Either String (TL.Text, [LexToken])
    extractTillNextDQoute [] = Left "Error: Missing double quote\n"
    extractTillNextDQoute (t : ts) =
      if token t == DQoute
        then Right (TL.empty, ts)
        else case extractTillNextDQoute ts of
          Left str -> Left str
          -- TL.append maybe inefficient. May need to change
          Right (txt, toks) -> Right $ (lexeme t `TL.append` txt, toks)

    createLiterals' :: [LexToken] -> Either String [LexToken]
    createLiterals' [] = Right []
    createLiterals' (t : ts) =
      if token t == DQoute
        then case extractTillNextDQoute ts of
          Left str -> Left str
          Right (literal, rest) ->
            let lexToken =
                  LexToken
                    { lexeme = literal,
                      token = Literal
                    }
             in case createLiterals' rest of
                  Left str -> Left str
                  Right rest' -> Right $ lexToken : rest'
        else case createLiterals' ts of
          Left str -> Left str
          Right ts' -> Right $ t : ts'

parsePattern :: TL.Text -> Either String [LexToken]
parsePattern text =
  let textList = listOfUnits text
      tokenList = map textToToken textList
      accumulateListOfEithers ::
        [Either String (TL.Text, TokenType)] ->
        Either String [LexToken]
      accumulateListOfEithers [] = Right []
      accumulateListOfEithers (x : xs) =
        let rest = accumulateListOfEithers xs
         in case x of
              Left err -> case rest of
                Left err' -> Left $ err ++ err'
                Right _ -> Left err
              Right (lexeme', tType) -> case rest of
                Left err -> Left err
                Right rest' ->
                  let lexToken =
                        LexToken
                          { lexeme = lexeme',
                            token = tType
                          }
                   in Right $ lexToken : rest'

      convertToEitherTokens ::
        [(TL.Text, TokenType)] ->
        [Either String (TL.Text, TokenType)]
      convertToEitherTokens [] = []
      convertToEitherTokens ((lexeme, token) : ts) =
        let rest = convertToEitherTokens ts
            thisToken =
              if token == Unknown
                then Left $ "Error: string " ++ show lexeme ++ " is invalid\n"
                else Right (lexeme, token)
         in thisToken : rest
   in createLiterals
        . accumulateListOfEithers
        . convertToEitherTokens
        . zip textList
        $ tokenList
