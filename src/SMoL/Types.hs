module SMoL.Types where

--type Prob = Rational
type Prob = Double

normalize :: (Traversable t, Fractional a, Eq a) => t a -> t a
normalize t = let s = sum t in if s == 0
                               then (const (1 / fromIntegral (length t))) <$> t
                               else (/ s) <$> t
