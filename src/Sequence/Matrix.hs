module Sequence.Matrix where

import Sequence.Constructors
import Sequence.CoreConstructors
import Sequence.Matrix.Types
import Sequence.Matrix.Operations
import Data.Fix

buildCoreConstructor :: CoreConstructor s (MatSeq s) -> MatSeq s
buildCoreConstructor CEmptySequence = emptySequence
buildCoreConstructor (CDeterministicSequence v) = deterministicSequence v
buildCoreConstructor (CId m) = m
buildCoreConstructor (CMatrixForm m) = m
buildCoreConstructor (CSkip n) = skip n
buildCoreConstructor (CEitherOr p s1 s2) = eitherOr p s1 s2
buildCoreConstructor (CAndThen s1 s2) = andThen s1 s2
buildCoreConstructor (CGeometricRepeat p s) = geometricRepeat p s
buildCoreConstructor (CReverseSequence s) = reverseSequence s
--buildCoreConstructor (CCollapse n s) = collapse n s

buildConstructor :: Constructor s (MatSeq s) -> MatSeq s
buildConstructor = cata buildCoreConstructor . toCore

buildMatSeq :: ProbSeq s -> MatSeq s
buildMatSeq = cata buildConstructor

buildMatSeqTree :: ProbSeq s -> ProbSeqWith s (MatSeq s)
buildMatSeqTree = ProbSeqWith . Fix . cata (\constr -> ConstructorWith {
                                                 with = buildConstructor (with <$> constr)
                                               , constructor = Fix <$> constr
                                               } )
