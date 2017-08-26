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
buildCoreConstructor (CId m) = m
buildCoreConstructor (CMatrixForm m) = m
buildCoreConstructor (CSkip n) = skip n
buildCoreConstructor (CEitherOr p s1 s2) = eitherOr p s1 s2
buildCoreConstructor (CAndThen s1 s2) = andThen s1 s2
buildCoreConstructor (CGeometricRepeat p s) = geometricRepeat p s
buildCoreConstructor (CReverseSequence s) = reverseSequence s
buildCoreConstructor (CCollapse f n s) = collapse f n s
buildCoreConstructor (CInsert a ix b) = insert a ix b

buildConstructor :: (Eq s) => Constructor s (MatSeq s) -> MatSeq s
buildConstructor = filterUnreachableStates . cata buildCoreConstructor . toCore

buildMatSeq :: (Eq s) => ProbSeq s -> MatSeq s
buildMatSeq = cata buildConstructor

buildMatSeqTree :: (Eq s) => ProbSeq s -> ProbSeqWith s (MatSeq s)
buildMatSeqTree = ProbSeqWith . Fix . cata (\constr -> ConstructorWith {
                                                 with = buildConstructor (with <$> constr)
                                               , constructor = Fix <$> constr
                                               } )
