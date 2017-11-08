module Plot where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Map as M
import Control.Monad
import Data.Maybe (fromMaybe)

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

plotProbLines :: (Enum n, Num n, Num m, PlotValue n, PlotValue m)
              => FilePath
              -> String
              -> String
              -> String
              -> (n, n)
              -> (m, m)
              -> [(M.Map n m, String)]
              -> IO ()
plotProbLines fp title xaxis yaxis (lowerX, upperX) (lowerY, upperY) maps = toFile def fp $ do
    layout_title .= title
    forM_ maps $ \(m, name) -> do
      plot (line name [(interpolate (lowerX, upperX) 0 m)])

interpolate :: (Enum n, Ord n) => (n, n) -> m -> M.Map n m -> [(n, m)]
interpolate (lower, upper) def m = map (\k -> (k, fromMaybe def (M.lookup k m))) [lower..upper]
