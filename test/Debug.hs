{-# LANGUAGE OverloadedLists #-}

import qualified Data.Vector as V
import Data.Word
import SequenceConstructor
import Sequence.Emission
import Sequence
import TestOperations

{-
--failing case:
constr = FiniteDistRepeat [0.2500764247236624,0.14590144003703306,1.9443748927665667e-2,4.6551782362205356e-2,0.20338597935718655,7.997519777684142e-2,0.25466542681540555] EmptySequence
(path, prob) = ([],0.14590144003703306)

prop = correctSampleProbProp (SampledSequenceConstructor (constr, (path, prob)))

sequ :: Sequence Word8
sequ = buildSequence constr
prob' = stateSequenceProbability sequ path
-}

getConstr :: SampledSequenceConstructor s -> SequenceConstructor s
getConstr (SampledSequenceConstructor (c, _)) = c

fail1 = SampledSequenceConstructor
  (FiniteDistRepeat
    [0.2500764247236624,0.14590144003703306,1.9443748927665667e-2,4.6551782362205356e-2,0.20338597935718655,7.997519777684142e-2,0.25466542681540555]
    EmptySequence
  ,([], 0.14590144003703306)
  )

fail2 :: SampledSequenceConstructor Word8
fail2 = SampledSequenceConstructor
  (AndThen
    EmptySequence
    (EitherOr
      0.793125764714127
      EmptySequence
      (DeterministicSequence [1,1,0,1,1,0,1,1,1,0,0,0,0]))
  ,([],0.793125764714127)
  )

fail2' :: SampledSequenceConstructor Word8
fail2' = SampledSequenceConstructor
  (EitherOr
    0.793125764714127
    EmptySequence
    (DeterministicSequence [1,1,0,1,1,0,1,1,1,0,0,0,0])
  ,([],0.793125764714127)
  )

fail2'' :: SampledSequenceConstructor Word8
fail2'' = SampledSequenceConstructor
  (EitherOr
    0.793125764714127
    (DeterministicSequence [1,1,0,1,1,0,1,1,1,0,0,0,0])
    EmptySequence
  ,([],0.793125764714127)
  )

fail3 :: SampledSequenceConstructor Word8
fail3 = SampledSequenceConstructor
  (AndThen
   (UniformDistOver
    [ReverseSequence (Possibly 0.9014767341320002 EmptySequence) -- sampler is selecting here
    ,AndThen
      (DeterministicSequence [1,0,0,0,1,0,0,1,0,1,1,0,1,1,0,1])
      (DeterministicSequence [1,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,1,1,0,1,1])
    ,EmptySequence
    ,DeterministicSequence [0,0,0,1,1,0,0,1,0,1,0,0,0,1,1]
    ,ReverseSequence
      (UniformDistRepeat
        8
        EmptySequence)
    ,DeterministicSequence [1,0,0,0,0,1,0,0,0,1,0,1,0,1,0]
    ])
    (UniformDistOver [DeterministicSequence [0,0,0,0,0,0,0,1,1]])
  ,([0,0,0,0,0,0,0,1,1],0.15024612235533336))

fail4 :: SampledSequenceConstructor Word8
fail4 = SampledSequenceConstructor
  (UniformDistRepeat
    5
    (ReverseSequence
      (UniformDistRepeat
        4
        (FiniteDistOver
          [(DeterministicSequence [0,1,0,0,1,0,1,1,1,1,1,1,0,1,0,0]
           ,0.19633501623142652)
          ,(ReverseSequence EmptySequence
           ,2.0327488954467025e-3)
          ,(EmptySequence
           ,0.18039814981505417)
          ,(EmptySequence
           ,5.538020311436943e-2)
          ,(DeterministicSequence [0,1,1,0,0,1,0,0,1,1,0,0,0,1,0,0,1,1,0,0,1] -- first, this one backwards
           ,8.800279288172433e-3)
          ,(DeterministicSequence [1,1,1,0,1,1,1,0,1,1,0,1,0,1,1] -- second, this one backward
           ,6.238378062944708e-2)
          ,(EmptySequence
           ,0.12137089741558661)
          ,(DeterministicSequence [1,1,1,1,0,0,0]
           ,0.14631202077313057)
          ,(Possibly
             0.2677894408964582
             (DeterministicSequence [1,1,0,1,1,1,1,1]) -- third, this one backwards
           ,0.22698690383736636)
          ]
        )
      )
    )
  ,([1,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,0
    ,1,1,0,1,0,1,1,0,1,1,1,0,1,1,1
    ,1,1,1,1,1,0,1,1]
   ,9.877151384474039e-8))

{-
let (pass, seq, probs) = examineFailingCase fail
-}
examineFailingCase :: SampledSequenceConstructor Word8 ->
                      ( Bool
                      , Sequence Word8
                      , V.Vector Prob
                      )
examineFailingCase (SampledSequenceConstructor (constr, (path, prob))) =
  (
    correctSampleProbProp (SampledSequenceConstructor (constr, (path, prob)))
  , sequ
  , stateSequenceProbability sequ path
  )
  where sequ = buildSequence constr
