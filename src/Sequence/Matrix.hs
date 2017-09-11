module Sequence.Matrix ( buildConstructor
                       , buildMatSeqTree
                       , buildMatSeq
                       , module Sequence.Matrix.Types ) where

import Sequence.Constructors
import Sequence.CoreConstructors
import Sequence.Matrix.Types
import Sequence.Matrix.Operations
import Data.Fix

buildCoreConstructor :: (Eq s) => CoreConstructor s (MatSeq s) -> MatSeq s
buildCoreConstructor CEmptySequence = emptySequence
buildCoreConstructor (CDeterministicSequence v) = deterministicSequence v
buildCoreConstructor (CSkip n) = skip n
buildCoreConstructor (CMatrixForm m) = m
buildCoreConstructor (CEitherOr p s1 s2) = eitherOr p s1 s2
buildCoreConstructor (CAndThen s1 s2) = andThen s1 s2
buildCoreConstructor (CIntersperse s1 s2) = intersperse s1 s2
buildCoreConstructor (CReverseSequence s) = reverseSequence s
buildCoreConstructor (CCollapse f n s) = collapse f n s
buildCoreConstructor (CGeometricRepeat p s) = geometricRepeat p s
buildCoreConstructor (CId m) = m

buildConstructor :: (Eq s) => Constructor s (MatSeq s) -> MatSeq s
buildConstructor = cata buildCoreConstructor . toCore

buildMatSeq :: (Eq s) => ProbSeq s -> MatSeq s
buildMatSeq = filterUnreachableStates . cata buildConstructor

buildMatSeqTree :: (Eq s) => ProbSeq s -> ProbSeqWith s (MatSeq s)
buildMatSeqTree = ProbSeqWith . Fix . cata (\constr -> ConstructorWith {
                                                 with =
                                                     filterUnreachableStates $ buildConstructor (with <$> constr)
                                               , constructor = Fix <$> constr
                                               } )
