{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Data.Kicad.SExpr.Parse
( parse
, parseWithFilename
)
where
import Text.ParserCombinators.Parsec hiding (spaces, parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.Parsec.Char (endOfLine)
import Text.Parsec (getPosition)

import Data.Kicad.SExpr.SExpr

{-| Parse a 'String' as a 'SExpr' or return an error. -}
parse :: String -> Either String SExpr
parse = parseWithFilename ""

{-| Parse a 'String' as a 'SExpr' giving a filename to use in the source code location -}
parseWithFilename :: String -> String -> Either String SExpr
parseWithFilename filename input =
    case Parsec.parse parseListOrComment filename input of
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
    pos <- getPosition
    char '('
    spaces
    list <- try parseExpr `sepEndBy` spaces
    char ')'
    spaces
    return $ List pos list


parseExpr :: Parser SExpr
parseExpr =  try parseString
         <|> try parseListOrComment
         <?> "a double, string or s-expression"


parseString :: Parser SExpr
parseString =
    do pos <- getPosition
       str <- parseQuotedString <|> parseUnquotedString <?> "string"
       return $ Atom pos str
            where
                parseQuotedString  = do
                    char '"'
                    x <- many (noneOf "\\\"" <|> (char '\\' >> anyChar))
                    char '"'
                    return x
                parseUnquotedString = many1 (noneOf " ()\r\n")


spaces = skipMany spaceChar
spaceChar = oneOf "\r\n\t "
