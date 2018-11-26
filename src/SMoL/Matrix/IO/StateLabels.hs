{-# LANGUAGE OverloadedStrings #-}
module SMoL.Matrix.IO.StateLabels where

import qualified Data.Vector as V
import Data.Vector (Vector)

import SMoL.Matrix.Types

import Data.Monoid ((<>))
import Data.List
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Conversion as BS
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- Writing

showStateLabels :: Vector (StateLabel String)
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

parseStateLabels :: Parser (Vector (StateLabel String))
parseStateLabels = V.fromList <$> sepBy parseStatePair endOfLine

parseStatePair :: Parser (StateLabel String)
parseStatePair = do
  _ <- char '#'
  textLabel <- takeWhile1 (/= ':')
  let label = Text.unpack $ textLabel

  _ <- char ':'

  stateTag <- parseStateTag

  return (StateLabel label stateTag IntMap.empty)

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
