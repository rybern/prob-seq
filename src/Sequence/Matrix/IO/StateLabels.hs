{-# LANGUAGE OverloadedStrings #-}
module Sequence.Matrix.IO.StateLabels where

import qualified Data.Vector as V
import Data.Vector (Vector)

import Sequence.Matrix.Types

import Data.Monoid ((<>))
import Data.List
import Data.Text hiding (map, intersperse)
import Data.Attoparsec.Text

-- Writing

showStateLabels :: Vector (String, StateTag)
                -> [Text]
showStateLabels = V.toList . V.map showPair
  where showPair (label, tag) =
          "#" <> (pack label) <> ":" <> showTag tag
        showTag (StateTag tag rest) =
          (pack . show $ tag) <>
          case rest of
            [] -> ""
            [child] -> "," <> showTag child
            children -> ",[" <> (mconcat $ intersperse ";" (map showTag children)) <> "]"

-- Reading

parseStateLabels :: Parser (Vector (String, StateTag))
parseStateLabels = V.fromList <$> sepBy parseStatePair endOfLine

parseStatePair :: Parser (String, StateTag)
parseStatePair = do
  _ <- char '#'
  textLabel <- takeWhile1 (/= ':')
  let label = unpack $ textLabel

  _ <- char ':'

  stateTag <- parseStateTag

  return (label, stateTag)

parseStateTag :: Parser StateTag
parseStateTag = do
  tagHead <- decimal

  rest <- choice [
      do _ <- char ','
         _ <- char '['
         children <- sepBy parseStateTag (char ';')
         _ <- char ']'
         return children
    , do _ <- char ','
         child <- parseStateTag
         return [child]
    , return []
    ]

  return $ StateTag tagHead rest
