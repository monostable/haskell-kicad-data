{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Data.Kicad.SExpr.Parse
( parse
)
where
import Text.ParserCombinators.Parsec hiding (spaces, parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.ParserCombinators.Parsec.Number
import Text.Parsec.Char (endOfLine)
import Control.Monad

import Data.Kicad.SExpr.SExpr
import Data.Kicad.SExpr.Write (writeKeyword)

{-| Parse a 'String' as a 'SExpr' or return an error. -}
parse :: String -> Either String SExpr
parse input = case Parsec.parse parseListOrComment "SExpr" input of
    Left err -> Left $ "Parse Error: " ++ show err
    Right val -> Right val


parseListOrComment :: Parser SExpr
parseListOrComment = do
    spaces
    skipMany parseComment
    s <- parseList
    return s


parseComment :: Parser String
parseComment = do
    char '#'
    s <- many (noneOf "\r\n")
    endOfLine
    spaces
    return s


parseList :: Parser SExpr
parseList = do
    char '('
    spaces
    first <- parseKeyword
    spaces
    rest <- let parseRest = try parseAtom `sepEndBy` spaces in case first of
        AtomKey KeyFpText -> do t <- parseString
                                  <?> "string designating type e.g. 'user'"
                                spaces1
                                s <- parseString
                                spaces
                                r <- parseRest
                                return (t:s:r)
        AtomKey KeyModule -> do t <- parseString
                                spaces
                                r <- parseRest
                                return (t:r)
        AtomKey KeyTedit  -> do s <- parseString
                                spaces
                                return [s]
        AtomKey KeyDescr  -> do s <- parseString
                                spaces
                                return [s]
        AtomKey KeyTags   -> do s <- parseString
                                spaces
                                return [s]
        AtomKey KeyPad    -> do n <- parseString
                                spaces1
                                t <- parseString
                                  <?> "string designating type e.g. 'smd'"
                                spaces1
                                s <- parseString
                                spaces
                                r <- parseRest
                                return (n:t:s:r)
        _                 -> parseRest
    spaces
    char ')'
    spaces
    return $ List (first:rest)

parseAtom :: Parser SExpr
parseAtom =  try parseDouble
         <|> try parseString
         <|> try parseListOrComment
         <?> "a double, string or s-expression"

parseOneKeyword :: Keyword -> Parser SExpr
parseOneKeyword kw = try $ string (writeKeyword kw) >> return (AtomKey kw)

parseKeyword :: Parser SExpr
parseKeyword = choice (map parseOneKeyword [minBound..maxBound]) <?> "keyword"

parseString :: Parser SExpr
parseString = liftM AtomStr (parseQuotedString <|> parseUnquotedString <?> "string")
        where
            parseQuotedString  = do
                char '"'
                x <- many (noneOf "\\\"" <|> (char '\\' >> anyChar))
                char '"'
                return x
            parseUnquotedString = many1 (noneOf "\" ()\r\n")

parseDouble :: Parser SExpr
parseDouble = do
    negate_or_id <- sign
    -- the Bool in floating3 is requireDigit which affects whether many (False,
    -- 0 or more) or many1 (True, 1 or more) is used
    x <- floating3 True
    lookAhead (char ')' <|> spaceChar)
    return $ AtomDbl $ negate_or_id x

spaces1 = skipMany1 spaceChar
spaces = skipMany spaceChar
spaceChar = endOfLine <|> space
