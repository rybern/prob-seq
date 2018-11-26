{-# LANGUAGE OverloadedStrings #-}
module SMoL.Matrix.IO.TransMatrix where

import qualified Data.Vector as V
import Data.Vector (Vector)

import SMoL.Matrix.Types
--import qualified Math.LinearAlgebra.Sparse as M
import qualified SparseMatrix as M

import Data.Csv hiding (Parser)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Word as BS
import Data.Monoid ((<>))
import Data.List
import Data.Text (Text, pack)
import Data.Attoparsec.Text
import Data.Ratio ((%))

type DecimalProb = Bool

transCSVOptions = defaultEncodeOptions { encDelimiter = BS.head $ BSC.singleton ' '}

showTrans :: DecimalProb
          -> Trans
          -> BS.ByteString
showTrans decimals = encodeWith transCSVOptions . map (\((r, c), p) -> (r - 1, c - 1, p)) . tail . M.toAssocList

showTrans' :: DecimalProb
          -> Trans
          -> [Text]
showTrans' decimals = map showTriple . tail . M.toAssocList
  where showTriple ((r, c), v) = mconcat . intersperse " " $ [
            pack (show (r - 1))
          , pack (show (c - 1))
          , if decimals
            then pack . show $ v -- fromRational
            else "(" <> pack (show v) <> ")"
          ]

parseTrans :: Parser (Trans)
parseTrans = M.fromAssocList <$> (parseTransTriple `sepBy` endOfLine)

parseTransTriple :: Parser ((Int, Int), Prob)
parseTransTriple = do
  row <- decimal
  _ <- char ' '
  col <- decimal
  _ <- char ' '
  val <- double
  return ((row + 1, col + 1), val)

parseRatio :: Parser Rational
parseRatio = choice [
    do _ <- char '('
       num <- decimal
       _ <- string " % "
       den <- decimal
       _ <- char ')'
       return $ num % den
  , rational
  ]
