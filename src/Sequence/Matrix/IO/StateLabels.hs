{-# LANGUAGE OverloadedStrings #-}
module Sequence.Matrix.IO.StateLabels where

import qualified Data.Vector as V
import Data.Vector (Vector)

import Sequence.Matrix.Types

import Data.Monoid ((<>))
import Data.List
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Conversion as BS
import qualified Data.Text as Text
import Data.Attoparsec.Text

-- Writing

showStateLabels :: Vector (String, StateTag)
                -> ByteString
showStateLabels = V.foldl1' BS.append . flip V.replicate "#\n" . V.length

showStateLabels' :: Vector (String, StateTag)
                -> ByteString
showStateLabels' = V.foldl1' BS.append . V.map showPair
  where showPair (label, tag) =
          "#" <> (BS.pack label) <> ":" <> showTag tag <> "\n"
        showTag (StateTag tag rest) =
          (BS.toByteString tag) <>
          case rest of
            [] -> ""
            [child] -> "," <> showTag child
            children -> ",[" <> (mconcat $ intersperse ";" (map showTag children)) <> "]"

{-
showStateLabels' :: Vector (String, StateTag)
                -> [Text]
showStateLabels' = V.toList . V.map showPair
  where showPair (label, tag) =
          "#" <> (pack label) <> ":" <> showTag tag
        showTag (StateTag tag rest) =
          (pack . show $ tag) <>
          case rest of
            [] -> ""
            [child] -> "," <> showTag child
            children -> ",[" <> (mconcat $ intersperse ";" (map showTag children)) <> "]"
-}

-- Reading

parseStateLabels :: Parser (Vector (String, StateTag))
parseStateLabels = V.fromList <$> sepBy parseStatePair endOfLine

parseStatePair :: Parser (String, StateTag)
parseStatePair = do
  _ <- char '#'
  textLabel <- takeWhile1 (/= ':')
  let label = Text.unpack $ textLabel

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
