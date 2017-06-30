module Sequence.AST where

import Sequence.Matrix.Operations
import Sequence.Matrix.Types
import Sequence.ProbTree
import Control.Monad.Random
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Function (fix)

data ProbSeq s = EmptySequence
               | DeterministicSequence (V.Vector s)
               | EitherOr Prob (ProbSeq s) (ProbSeq s)
               | AndThen (ProbSeq s) (ProbSeq s)
               | Possibly Prob (ProbSeq s)
               | UniformDistOver [ProbSeq s]
               | FiniteDistOver [(ProbSeq s, Prob)]
               | GeometricRepeat Prob (ProbSeq s)
               | FiniteDistRepeat [Prob] (ProbSeq s)
               | UniformDistRepeat Int (ProbSeq s)
               | ReverseSequence (ProbSeq s)
               | Collapse Int (ProbSeq s)
               | SkipDist Dist (ProbSeq s)
               deriving (Show)

sampleAST :: (MonadRandom m)
          => ProbSeq s
          -> m (V.Vector s)
sampleAST = sampleProbTree . probTree

sampleASTUniform :: (MonadRandom m)
                 => ProbSeq s
                 -> m (V.Vector s)
sampleASTUniform = sampleProbTreeUniform . probTree

sequenceProbAST :: (Eq s)
                => V.Vector s
                -> ProbSeq s
                -> Prob
sequenceProbAST v = probOfVec v . probTree

probTree :: ProbSeq s -> ProbTree (V.Vector s)
probTree = fix (flip probTree')

probTree' :: ProbSeq s
          -> (ProbSeq s -> ProbTree (V.Vector s))
          -> ProbTree (V.Vector s)
probTree' EmptySequence _ = Det V.empty
probTree' (DeterministicSequence v) _ = Det v
probTree' (EitherOr p s1 s2) f = DistOver [(f s1, p), (f s2, 1-p)]
probTree' (AndThen s1 s2) f = Series [f s1, f s2]
probTree' (Possibly p s) f = f (EitherOr p s EmptySequence)
probTree' (UniformDistOver seqs) f = let uniform = recip . fromIntegral . length $ seqs
                                     in DistOver $ map (\seq -> (f seq, uniform)) seqs
probTree' (FiniteDistOver seqs) f = DistOver $ map (\(seq, p) -> (f seq, p)) seqs
probTree' (GeometricRepeat p s) f = let g = EitherOr (1-p) EmptySequence (AndThen s g)
                                    in f g
probTree' (FiniteDistRepeat ps s) f = DistOver (zip (map f $ iterate (`AndThen` s) EmptySequence) (1-sum ps:ps))
probTree' (UniformDistRepeat n s) f = let uniform = recip . fromIntegral . succ $ n
                                      in f $ FiniteDistRepeat (replicate n uniform) s
probTree' (ReverseSequence s) f = V.reverse <$> f s
probTree' _ _ = error "collapse and skipdist not implemented"
--probTree' (Collapse n s) = undefined
--probTree' (SkipDist Dist (ProbSeq s)) f = undefined

buildMatSeq :: ProbSeq s
            -> MatSeq s
buildMatSeq = fix (flip buildMatSeq')

buildMatSeq' :: ProbSeq s
             -> (ProbSeq s -> MatSeq s)
             -> MatSeq s
buildMatSeq' EmptySequence _ = emptySequence
buildMatSeq' (DeterministicSequence v) _ = deterministicSequence v
buildMatSeq' (EitherOr p s1 s2) f = eitherOr p (f s1) (f s2)
buildMatSeq' (AndThen s1 s2) f = andThen (f s1) (f s2)
buildMatSeq' (Possibly p s) f = f (EitherOr p s EmptySequence)
buildMatSeq' (UniformDistOver seqs) f = let uniform = recip . fromIntegral . length $ seqs
                                        in finiteDistOver $ map (\seq -> (f seq, uniform)) seqs
buildMatSeq' (FiniteDistOver seqs) f = finiteDistOver $ map (\(seq, p) -> (f seq, p)) seqs
buildMatSeq' (GeometricRepeat p s) f = geometricRepeat p (f s)
buildMatSeq' (FiniteDistRepeat ps s) f = finiteDistRepeat ps (f s)
buildMatSeq' (UniformDistRepeat n s) f = let uniform = recip . fromIntegral . succ $ n
                                         in f $ FiniteDistRepeat (replicate n uniform) s
buildMatSeq' (ReverseSequence s) f = reverseSequence (f s)
buildMatSeq' _ _ = error "collapse and skipdist not implemented"
