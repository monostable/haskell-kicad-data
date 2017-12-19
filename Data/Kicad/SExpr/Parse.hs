{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Data.Kicad.SExpr.Parse
( parse
)
where
import Text.ParserCombinators.Parsec hiding (spaces, parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.Parsec.Char (endOfLine)
import Control.Monad

import Data.Kicad.SExpr.SExpr

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
    first <- parseString
    spaces
    rest <- let parseRest = try parseAtom `sepEndBy` spaces in case first of
        Atom "fp_text" -> do t <- parseString
                               <?> "string designating type e.g. 'user'"
                             spaces1
                             s <- parseString
                             spaces
                             r <- parseRest
                             return (t:s:r)
        Atom "module" -> do t <- parseString
                            spaces
                            r <- parseRest
                            return (t:r)
        Atom "tedit"  -> do s <- parseString
                            spaces
                            return [s]
        Atom "descr"  -> do s <- parseString
                            spaces
                            return [s]
        Atom "tags"   -> do s <- parseString
                            spaces
                            return [s]
        Atom "pad"    -> do n <- parseString
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
parseAtom =  try parseString
         <|> try parseListOrComment
         <?> "a double, string or s-expression"

parseString :: Parser SExpr
parseString = liftM Atom (parseQuotedString <|> parseUnquotedString <?> "string")
        where
            parseQuotedString  = do
                char '"'
                x <- many (noneOf "\\\"" <|> (char '\\' >> anyChar))
                char '"'
                return x
            parseUnquotedString = many1 (noneOf "\" ()\r\n")

spaces1 = skipMany1 spaceChar
spaces = skipMany spaceChar
spaceChar = oneOf "\r\n "
