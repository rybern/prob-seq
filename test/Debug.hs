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

-- p([]) agrees
-- probs came from matrix correctly
-- p(2 -> end) is correct (0.64 is correct)

-- the style of computation is different between the two - one enumerates paths, the other collapses. that causing?

-- the problem is that sampleStateSeq differentiates between pick 2, fail first and pick 3, fail first two. matrix doesn't.
t12 :: SampledSequenceConstructor Word8
t12 = SampledSequenceConstructor
  (FiniteDistRepeat
    [0.17527763941547947,0.24549167470262973,0.22146744471748583]
    (EitherOr
      0.25768500658936777
      EmptySequence
      (DeterministicSequence [11]))
  ,([11],4.695849300269045e-2))

-- maybe a real case
t11 :: SampledSequenceConstructor Word8
t11 = SampledSequenceConstructor
  (FiniteDistRepeat
    [0.20570791995607246,0.16915951059059542,0.22966181015324996,0.24097305678384745,3.872396887598021e-2,0.1157737336402545]
    (EitherOr
      0.677868943354219
      EmptySequence
      (DeterministicSequence [170]))
  ,([170],3.399482852853882e-2))

t10 :: SampledSequenceConstructor Word8
t10 = SampledSequenceConstructor (FiniteDistRepeat [0.31931631179247005,0.3423255550553669,0.338358133152163] (DeterministicSequence [195]),([],0.0))

t9''' :: SampledSequenceConstructor Word8
t9''' = SampledSequenceConstructor
  (FiniteDistRepeat
    [9.367040216878875e-2,0.2085835157630606,6.399492042564472e-2,0.22491994072942248,0.13827666406802683,1.3714483139570953e-2,0.11097873177779992,0.14586134192768574]
    (AndThen
      EmptySequence
      (DeterministicSequence [0]))
  ,([0,0,0,0,0,0,0],0.11097873177779992))

t9'' = SampledSequenceConstructor
  (AndThen
    EmptySequence
    (FiniteDistRepeat
      []
      (DeterministicSequence [5]))
  ,([],1.0))

-- real fail case
t9 :: SampledSequenceConstructor Word8
t9 = SampledSequenceConstructor
  (AndThen
    (FiniteDistRepeat
      [1.0]
      (EitherOr
        0.6290690237713935
        (DeterministicSequence [233])
        (DeterministicSequence [197])))
    EmptySequence
  ,([],1.0))

-- real fail case
t9' :: SampledSequenceConstructor Word8
t9' = SampledSequenceConstructor
    (FiniteDistRepeat
      [1.0]
      (DeterministicSequence [197])
  ,([],1.0))

t8 :: SampledSequenceConstructor Word8
t8 = SampledSequenceConstructor (FiniteDistRepeat [5.051670669448369e-3,0.11857007375931125,6.1279941201941045e-2,0.1360542368715279,0.15155165084488922,7.53519538606948e-2,0.20864227955839,3.773121925718115e-2,0.2057669739766161] (EitherOr 0.3846994909937804 (AndThen (EitherOr 0.11887042787979885 (DeterministicSequence [73]) (DeterministicSequence [23])) EmptySequence) (EitherOr 6.16411912380469e-2 (FiniteDistRepeat [0.11585980750753122,0.23760036873100496,0.36574449801225434,0.11952871069050347,0.161266615058706] EmptySequence) (DeterministicSequence [64]))),([64,23],1.1993223810841554e-2))

t7 :: SampledSequenceConstructor Word8
t7 = SampledSequenceConstructor
  (AndThen
    (EitherOr
      0.2217444119744031
      (DeterministicSequence [63])
      (DeterministicSequence [8]))
    (AndThen
      (ReverseSequence
        (Possibly
          0.16423460345026542
          (DeterministicSequence [21])))
      (ReverseSequence
        (DeterministicSequence [14])))
  ,([63,14],0.1853263064064747))

t_6 :: SampledSequenceConstructor Word8
t_6 = SampledSequenceConstructor
  (Possibly
    0.9180571943334087
    (DeterministicSequence [0])
  ,([],8.194280566659129e-2))

t_smaller5 :: SampledSequenceConstructor Word8
t_smaller5 = SampledSequenceConstructor
  (EitherOr
    0.6390397309716273
    (EitherOr
      0.5430528276585397
      (DeterministicSequence [61])
      (AndThen
        (DeterministicSequence [233])
        (EitherOr
          0.2034368336386011
          (DeterministicSequence [48])
          EmptySequence)))
    (DeterministicSequence [195])
  ,([233,48],5.9405060464712826e-2))

t_smaller4 = SampledSequenceConstructor (EitherOr 0.3466885292535655 (EitherOr 0.987046139810432 (DeterministicSequence [11]) (AndThen (AndThen (DeterministicSequence [60]) (EitherOr 8.678644574200223e-2 (EitherOr 0.4534695559309463 EmptySequence EmptySequence) (EitherOr 0.3798536400448812 (DeterministicSequence [25]) (DeterministicSequence [])))) EmptySequence)) EmptySequence,([60],2.933098708529919e-3))

t_smaller3 = SampledSequenceConstructor (EitherOr 0.880612161841229 (DeterministicSequence [182]) (EitherOr 0.4861549683450068 (AndThen (EitherOr 0.8415317157496898 (DeterministicSequence [210,135,230,44,238]) EmptySequence) (EitherOr 0.6156448634503322 (EitherOr 0.4802141379489353 (DeterministicSequence [133,148,42]) EmptySequence) EmptySequence)) (AndThen (DeterministicSequence [112,96,147,3,137]) EmptySequence)),([210,135,230,44,238],3.440322428070836e-2))

t_smaller2 = SampledSequenceConstructor (EitherOr 8.433357449942536e-2 EmptySequence (EitherOr 0.17050273089670098 EmptySequence (AndThen (DeterministicSequence [215,206,10,104,5,85,223,147,139,221,71,19,143,104,97,134,124,135,36,162,169,131,98,183,204]) (EitherOr 8.033655236930182e-2 EmptySequence (DeterministicSequence [4,137,220,155,255,244,152,176,84,40,181,233,225,155,210])))),([215,206,10,104,5,85,223,147,139,221,71,19,143,104,97,134,124,135,36,162,169,131,98,183,204],6.101904987769601e-2))

t_smaller = SampledSequenceConstructor (EitherOr 0.23021748076377857 (AndThen (EitherOr 0.2563931629407996 (DeterministicSequence [8,131,73,174,142,51,71,209,161,228,238,73,60,87,13,1,233,109,161,145,112,93,73,140,219]) EmptySequence) (EitherOr 8.346893796448906e-2 (EitherOr 0.8366047117266284 (DeterministicSequence [2,89,162,152,157,31,62,232,152,228,213,146,125,148,53,85,123,190,44,109]) (DeterministicSequence [95,47,129,36,21,108,128,3,108,195,4,107,107,251,143,119,145,12,223,146,52,38,131,162])) (DeterministicSequence [92,243,176,13,149,240,183,95,43,17,113,16,190,247,249,88,193,191,224]))) (EitherOr 0.780763176785834 (DeterministicSequence [3,108,66,51,130,134,172,127,169,122,30,168,34,28,201,163]) (AndThen (DeterministicSequence [98,77,3,74,247,130,187,161,16,123,145,231,19,79]) EmptySequence)),([8,131,73,174,142,51,71,209,161,228,238,73,60,87,13,1,233,109,161,145,112,93,73,140,219,92,243,176,13,149,240,183,95,43,17,113,16,190,247,249,88,193,191,224],5.4099334828053854e-2))


t = EitherOr 0.8639735666967614 (AndThen (AndThen (EitherOr 0.7518557941869654 (DeterministicSequence [31,32,207,229,180,230,79,10,182,65,179,234,153,30,165,110,152,75,175,209,248]) EmptySequence) (AndThen (EitherOr 0.7410717976346282 (AndThen (AndThen EmptySequence (EitherOr 9.700746231174973e-2 (EitherOr 0.11796538900321119 (DeterministicSequence [240,230,206,217,4,242,138,170,4,216,147,144,140,209]) EmptySequence) EmptySequence)) (DeterministicSequence [164,53,108,187,192,54,53,65,174,225,229,170,123,221,156,214])) (AndThen (DeterministicSequence [174,64,246,140,170,179,183,188,112,238,191,87,246,34,44,159,231,43,124,24,58,8,58,56,31]) (DeterministicSequence [158,9,56,212,79,207,6,110,129,134,42,226,253,29,20]))) (EitherOr 8.700061253532898e-2 EmptySequence EmptySequence))) EmptySequence) (DeterministicSequence [116,27,145,252,157,127,248,165,106])

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
