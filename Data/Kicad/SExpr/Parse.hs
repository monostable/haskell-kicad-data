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
    list <- try parseExpr `sepEndBy` spaces
    char ')'
    spaces
    return $ List list


parseExpr :: Parser SExpr
parseExpr =  try parseString
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
            parseUnquotedString = many1 (noneOf " ()\r\n")


spaces = skipMany spaceChar
spaceChar = oneOf "\r\n "
