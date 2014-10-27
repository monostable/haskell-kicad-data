{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Data.Kicad.ParseSExpr
( parseSExpr
)
where
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Number
import Control.Monad

import Data.Kicad.SExpr

parseSExpr :: String -> Either String SExpr
parseSExpr input = case parse parseList "SExpr" input of
    Left err -> Left $ "Parse Error: " ++ show err
    Right val -> Right val

parseList :: Parser SExpr
parseList = do
    char '('
    spaces
    first <- parseKeyword
    spaces
    rest <- let parseRest = try parseAtom `sepEndBy` spaces1 in case first of
        AtomKey KeyFpText -> do t <- parseString
                                  <?> "string designating type e.g. 'user'"
                                spaces1
                                s <- parseString
                                spaces1
                                r <- parseRest
                                return (t:s:r)
        AtomKey KeyModule -> do t <- parseString
                                spaces1
                                r <- parseRest
                                return (t:r)
        AtomKey KeyPad    -> do n <- parseString
                                spaces1
                                t <- parseString
                                spaces1
                                s <- parseString
                                spaces1
                                r <- parseRest
                                return (n:t:s:r)
        _                 -> parseRest
    char ')'
    return $ List (first:rest)

parseAtom :: Parser SExpr
parseAtom =  try parseDouble
         <|> try parseString
         <|> try parseList
         <?> "a double, string or s-expression"

parseKeyword :: Parser SExpr
parseKeyword =  try (string "module"     >> return (AtomKey KeyModule   ))
            <|> try (string "fp_text"    >> return (AtomKey KeyFpText   ))
            <|> try (string "effects"    >> return (AtomKey KeyEffects  ))
            <|> try (string "font"       >> return (AtomKey KeyFont     ))
            <|> try (string "size"       >> return (AtomKey KeySize     ))
            <|> try (string "thickness"  >> return (AtomKey KeyThickness))
            <|> try (string "tedit"      >> return (AtomKey KeyTEdit    ))
            <|> try (string "fp_line"    >> return (AtomKey KeyFpLine   ))
            <|> try (string "start"      >> return (AtomKey KeyStart    ))
            <|> try (string "end"        >> return (AtomKey KeyEnd      ))
            <|> try (string "width"      >> return (AtomKey KeyWidth    ))
            <|> try (string "descr"      >> return (AtomKey KeyDescr    ))
            <|> try (string "tags"       >> return (AtomKey KeyTags     ))
            <|> try (string "attr"       >> return (AtomKey KeyAttr     ))
            <|> try (string "at"         >> return (AtomKey KeyAt       ))
            <|> try (string "pad"        >> return (AtomKey KeyPad      ))
            <|> try (string "layers"     >> return (AtomKey KeyLayers   ))
            <|> try (string "layer"      >> return (AtomKey KeyLayer    ))
            <|> try (string "drill"      >> return (AtomKey KeyDrill    ))
            <|> try (string "rect_delta" >> return (AtomKey KeyRectDelta))
            <?> "keyword"

parseString :: Parser SExpr
parseString = liftM AtomStr (parseQuotedString <|> parseUnquotedString <?> "string")
        where
            parseQuotedString  = do
                char '"'
                x <- many (noneOf "\\\"" <|> (char '\\' >> char '\"') <?> "only escaped quote" ) -- XXX anyChar?
                char '"'
                return x
            parseUnquotedString = many1 (noneOf "\" ()\r\n")

parseDouble :: Parser SExpr
parseDouble = do
    negate_or_id <- sign
    -- the Bool in floating3 is requireDigit which affects whether many (False)
    -- or many1 (True) is used
    x <- floating3 True
    lookAhead (char ')' <|> spaceChar)
    return $ AtomDbl $ negate_or_id x

spaces1 = skipMany1 spaceChar
spaces = skipMany spaceChar
spaceChar = newline <|> space
