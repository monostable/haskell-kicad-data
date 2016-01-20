module Data.Kicad.Util where

import Data.Maybe

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

headOr :: a -> [a] -> a
headOr d xs = fromMaybe d (maybeHead xs)

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs
