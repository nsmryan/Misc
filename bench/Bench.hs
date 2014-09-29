
import Criterion.Main

import Control.Applicative
import Control.Monad
import Control.Monad.ST

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Random.Distribution.Uniform as Uni
import Data.Random

import Math.Probable.Random as Pr

import System.Random.MWC.Monad as MWC
import System.Random.MWC

import GA
import UtilsRandom
import PointMutation
import Selection
import Crossover
import Utils
import Examples


--TODO Operator benchmarks against naive verions
--     more fair pm for vectors? ST and/or unboxed?
--     Randomness benchmarks to decide default RNG
--     Algorithm benchmarks
--     Algorithm benchmarks against other frameworks

main = defaultMain
  [
    bgroup "Ones"
    [
      randBench "GA" $ (geneticAlgorithm 50 100 1000 0.01 0.6 simpleEval)
    ]

  , bgroup "GA"
    [
      bgroup "GA Generations"
      [
        randBench "GA 100 gens" $ (geneticAlgorithm 25 50 100 0.01 0.6 simpleEval)
      , randBench "GA 200 gens" $ (geneticAlgorithm 25 50 200 0.01 0.6 simpleEval)
      , randBench "GA 300 gens" $ (geneticAlgorithm 25 50 300 0.01 0.6 simpleEval)
      , randBench "GA 400 gens" $ (geneticAlgorithm 25 50 400 0.01 0.6 simpleEval)
      ]

    , bgroup "GA Individual Sizes"
      [
        randBench "GA 100 Locuses" $ (geneticAlgorithm 50 100 100 0.01 0.6 simpleEval)
      , randBench "GA 200 Locuses" $ (geneticAlgorithm 50 200 100 0.01 0.6 simpleEval)
      , randBench "GA 300 Locuses" $ (geneticAlgorithm 50 300 100 0.01 0.6 simpleEval)
      , randBench "GA 400 Locuses" $ (geneticAlgorithm 50 400 100 0.01 0.6 simpleEval)
      ]

    , bgroup "GA Population Sizes"
      [
        randBench "GA 100 Individuals" $ (geneticAlgorithm 100 100 100 0.01 0.6 simpleEval)
      , randBench "GA 200 Individuals" $ (geneticAlgorithm 200 100 100 0.01 0.6 simpleEval)
      , randBench "GA 300 Individuals" $ (geneticAlgorithm 300 100 100 0.01 0.6 simpleEval)
      , randBench "GA 400 Individuals" $ (geneticAlgorithm 400 100 100 0.01 0.6 simpleEval)
      ]

    , bgroup "GA Mutation Rates" 
      [
        bcompare
        [
          randBench "GA pm = 0.01" $ (geneticAlgorithm 100 100 100 0.01 0.6 simpleEval)
        , randBench "GA pm = 0.25" $ (geneticAlgorithm 100 100 100 0.25 0.6 simpleEval)
        , randBench "GA pm = 0.50" $ (geneticAlgorithm 100 100 100 0.50 0.6 simpleEval)
        , randBench "GA pm = 0.75" $ (geneticAlgorithm 100 100 100 0.75 0.6 simpleEval)
        , randBench "GA pm = 1.00" $ (geneticAlgorithm 100 100 100 1.00 0.6 simpleEval)
        ]
      ]

    , bgroup "GA Crossover Rates" 
      [
        randBench "GA pc = 0.25" $ (geneticAlgorithm 100 100 100 0.01 0.25 simpleEval)
      , randBench "GA pc = 0.50" $ (geneticAlgorithm 100 100 100 0.01 0.50 simpleEval)
      , randBench "GA pc = 0.75" $ (geneticAlgorithm 100 100 100 0.01 0.75 simpleEval)
      , randBench "GA pc = 1.00" $ (geneticAlgorithm 100 100 100 0.01 1.00 simpleEval)
      ]
    ]

  , bgroup "PM"
    [
      bgroup "PM Naive/Geometric"
      [
        randBench "PM Naive Seq"  $ pointMutationNaive 0.01 8 $ pop32All0 100 10
      , randBench "PM Naive Vect" $ pointMutationNaiveVector 0.01 8 (V.replicate 100 (V.replicate 10 0))
      , randBench "PM Geometric"  $ pointMutation      0.01 10 8 $ pop32All0 100 10
      ]

    , bgroup "PM Mutation Rate"
      [
        bgroup "PM Naive Seq"
        [
          randBench "PM Naive Seq 0.2" $ pointMutationNaive 0.2 8 $ pop32All0 10 10
        , randBench "PM Naive Seq 0.4" $ pointMutationNaive 0.4 8 $ pop32All0 10 10
        , randBench "PM Naive Seq 0.6" $ pointMutationNaive 0.6 8 $ pop32All0 10 10
        , randBench "PM Naive Seq 0.8" $ pointMutationNaive 0.8 8 $ pop32All0 10 10
        , randBench "PM Naive Seq 1.0" $ pointMutationNaive 1.0 8 $ pop32All0 10 10
        ]
        
      , bgroup "PM Naive Vect"
        [
          randBench "PM Naive Vect 0.2" $ pointMutationNaiveVector 0.2 8 (V.replicate 10 (V.replicate 10 0))
        , randBench "PM Naive Vect 0.4" $ pointMutationNaiveVector 0.4 8 (V.replicate 10 (V.replicate 10 0))
        , randBench "PM Naive Vect 0.6" $ pointMutationNaiveVector 0.6 8 (V.replicate 10 (V.replicate 10 0))
        , randBench "PM Naive Vect 0.8" $ pointMutationNaiveVector 0.8 8 (V.replicate 10 (V.replicate 10 0))
        , randBench "PM Naive Vect 1.0" $ pointMutationNaiveVector 1.0 8 (V.replicate 10 (V.replicate 10 0))
        ]

      , bgroup "PM Geometric"
        [
          randBench "PM Geometric 0.2" $ pointMutation 0.2 10 8 $ pop32All0 10 10
        , randBench "PM Geometric 0.4" $ pointMutation 0.4 10 8 $ pop32All0 10 10
        , randBench "PM Geometric 0.6" $ pointMutation 0.6 10 8 $ pop32All0 10 10
        , randBench "PM Geometric 0.8" $ pointMutation 0.8 10 8 $ pop32All0 10 10
        , randBench "PM Geometric 1.0" $ pointMutation 1.0 10 8 $ pop32All0 10 10
        ]
      ]

    , bgroup "PM Individual Size"
      [
        bgroup "Naive Seq" 
        [
            randBench "PM Naive Seq 100" $ pointMutationNaive 0.01 8 $ pop32All0 10 100
          , randBench "PM Naive Seq 200" $ pointMutationNaive 0.01 8 $ pop32All0 10 200
          , randBench "PM Naive Seq 300" $ pointMutationNaive 0.01 8 $ pop32All0 10 300
          , randBench "PM Naive Seq 400" $ pointMutationNaive 0.01 8 $ pop32All0 10 400
          , randBench "PM Naive Seq 500" $ pointMutationNaive 0.01 8 $ pop32All0 10 500
        ]
      , bgroup "Naive Vect"
        [
            randBench "PM Naive Vector 100" $ pointMutationNaiveVector 0.01 8 (V.replicate 10 (V.replicate 100 0))
          , randBench "PM Naive Vector 200" $ pointMutationNaiveVector 0.01 8 (V.replicate 10 (V.replicate 200 0))
          , randBench "PM Naive Vector 300" $ pointMutationNaiveVector 0.01 8 (V.replicate 10 (V.replicate 300 0))
          , randBench "PM Naive Vector 400" $ pointMutationNaiveVector 0.01 8 (V.replicate 10 (V.replicate 400 0))
          , randBench "PM Naive Vector 500" $ pointMutationNaiveVector 0.01 8 (V.replicate 10 (V.replicate 500 0))
        ]
      , bgroup "Geometric" 
        [
            randBench "PM Geometric 100" $ pointMutation 0.01 100 8 $ pop32All0 10 100
          , randBench "PM Geometric 200" $ pointMutation 0.01 200 8 $ pop32All0 10 200
          , randBench "PM Geometric 300" $ pointMutation 0.01 300 8 $ pop32All0 10 300
          , randBench "PM Geometric 400" $ pointMutation 0.01 400 8 $ pop32All0 10 400
          , randBench "PM Geometric 500" $ pointMutation 0.01 500 8 $ pop32All0 10 500
        ]
      ]

    , bgroup "PM Population Size"
      [
        bgroup "Naive Seq"
        [
          randBench "PM Naive Seq 100" $ pointMutationNaive 0.01 8 $ pop32All0 100 10
        , randBench "PM Naive Seq 200" $ pointMutationNaive 0.01 8 $ pop32All0 200 10
        , randBench "PM Naive Seq 300" $ pointMutationNaive 0.01 8 $ pop32All0 300 10
        , randBench "PM Naive Seq 400" $ pointMutationNaive 0.01 8 $ pop32All0 400 10
        , randBench "PM Naive Seq 500" $ pointMutationNaive 0.01 8 $ pop32All0 500 10
        ]
      , bgroup "Naive Vector"
        [
          randBench "PM Naive Vector 100" $ pointMutationNaiveVector 0.01 8 (V.replicate 100 (V.replicate 10 0)) 
        , randBench "PM Naive Vector 200" $ pointMutationNaiveVector 0.01 8 (V.replicate 200 (V.replicate 10 0)) 
        , randBench "PM Naive Vector 300" $ pointMutationNaiveVector 0.01 8 (V.replicate 300 (V.replicate 10 0)) 
        , randBench "PM Naive Vector 400" $ pointMutationNaiveVector 0.01 8 (V.replicate 400 (V.replicate 10 0)) 
        , randBench "PM Naive Vector 500" $ pointMutationNaiveVector 0.01 8 (V.replicate 500 (V.replicate 10 0)) 
        ]
      , bgroup "Geometric"
        [
          randBench "PM Geometric 100" $ pointMutation 0.01 10 8 $ pop32All0 100 10
        , randBench "PM Geometric 200" $ pointMutation 0.01 10 8 $ pop32All0 200 10
        , randBench "PM Geometric 300" $ pointMutation 0.01 10 8 $ pop32All0 300 10
        , randBench "PM Geometric 400" $ pointMutation 0.01 10 8 $ pop32All0 400 10
        , randBench "PM Geometric 500" $ pointMutation 0.01 10 8 $ pop32All0 500 10
        ]
      ]
    ]

    , let seqPop = S.replicate 100 (S.replicate 100 (0 :: Int))
          vectPop = V.replicate 100 (V.replicate 100 (0 :: Int))
        in
          bgroup "Crossover"
      [
        bgroup "Crossover Naive/Geometric"
        [
          randBench "Crossover Naive Vector Seq" $ crossVectSeq  0.60 vectPop
        , randBench "Crossover Naive Vector Par" $ crossVectPar  0.60 vectPop
        , randBench "Crossover Naive Seq"    $ crossoverSeq  0.60 seqPop
        , randBench "Crossover Naive Par"    $ crossoverPar  0.60 seqPop
        , randBench "Crossover Geometric"    $ crossover     0.60 seqPop
        ]

      , bgroup "Crossover Rate"
        [
          bgroup "Crossover Naive Vector Sequential"
          [
            randBench "Crossover Naive Vector Seq 0.2" $ crossVectSeq 0.2 vectPop
          , randBench "Crossover Naive Vector Seq 0.6" $ crossVectSeq 0.6 vectPop
          , randBench "Crossover Naive Vector Seq 1.0" $ crossVectSeq 1.0 vectPop
          ]

        , bgroup "Crossover Naive Vector Parallel"
          [
            randBench "Crossover Naive Vector Par 0.2" $ crossVectPar 0.2 vectPop
          , randBench "Crossover Naive Vector Par 0.6" $ crossVectPar 0.6 vectPop
          , randBench "Crossover Naive Vector Par 1.0" $ crossVectPar 1.0 vectPop
          ]

        , bgroup "Crossover Naive Seq"
          [
            randBench "Crossover Naive Seq 0.2" $ crossoverSeq 0.2 seqPop
          , randBench "Crossover Naive Seq 0.6" $ crossoverSeq 0.6 seqPop
          , randBench "Crossover Naive Seq 1.0" $ crossoverSeq 1.0 seqPop
          ]

        , bgroup "Crossover Naive Par"
          [
            randBench "Crossover Naive Par 0.2" $ crossoverPar 0.2 seqPop
          , randBench "Crossover Naive Par 0.6" $ crossoverPar 0.6 seqPop
          , randBench "Crossover Naive Par 1.0" $ crossoverPar 1.0 seqPop
          ]
          
        , bgroup "Crossover Geometric"
          [
            randBench "Crossover Geometric 0.2" $ crossover 0.2 seqPop
          , randBench "Crossover Geometric 0.6" $ crossover 0.6 seqPop
          , randBench "Crossover Geometric 1.0" $ crossover 1.0 seqPop
          ]
        ]

      , bgroup "Crossover Individual Size"
        [
          bgroup "Naive Vector Seq"
          [
              randBench "Crossover Naive Vector Seq 100" $ crossVectSeq 0.60 (V.replicate 100 (V.replicate 100 (0 :: Int)))
            , randBench "Crossover Naive Vector Seq 250" $ crossVectSeq 0.60 (V.replicate 100 (V.replicate 250 (0 :: Int)))
            , randBench "Crossover Naive Vector Seq 500" $ crossVectSeq 0.60 (V.replicate 100 (V.replicate 500 (0 :: Int)))
          ]

        , bgroup "Naive Vector Par" 
          [
              randBench "Crossover Naive Vector Par 100" $ crossVectPar 0.60 (V.replicate 100 (V.replicate 100 (0 :: Int)))
            , randBench "Crossover Naive Vector Par 250" $ crossVectPar 0.60 (V.replicate 100 (V.replicate 250 (0 :: Int)))
            , randBench "Crossover Naive Vector Par 500" $ crossVectPar 0.60 (V.replicate 100 (V.replicate 500 (0 :: Int)))
          ]

        , bgroup "Naive Seq" 
          [
              randBench "Crossover Naive Seq 100" $ crossoverSeq 0.60 (S.replicate 100 (S.replicate 100 (0 :: Int)))
            , randBench "Crossover Naive Seq 250" $ crossoverSeq 0.60 (S.replicate 100 (S.replicate 250 (0 :: Int)))
            , randBench "Crossover Naive Seq 500" $ crossoverSeq 0.60 (S.replicate 100 (S.replicate 500 (0 :: Int)))
          ]

        , bgroup "Naive Par" 
          [
              randBench "Crossover Naive Par 100" $ crossoverPar 0.60 (S.replicate 100 (S.replicate 100 (0 :: Int)))
            , randBench "Crossover Naive Par 250" $ crossoverPar 0.60 (S.replicate 100 (S.replicate 250 (0 :: Int)))
            , randBench "Crossover Naive Par 500" $ crossoverPar 0.60 (S.replicate 100 (S.replicate 500 (0 :: Int)))
          ]

        , bgroup "Geometric" 
          [
              randBench "Crossover Geometric 100" $ crossover 0.60 $ pop32All0 100 100
            , randBench "Crossover Geometric 250" $ crossover 0.60 $ pop32All0 100 250
            , randBench "Crossover Geometric 500" $ crossover 0.60 $ pop32All0 100 500
          ]
        ]

      , bgroup "Crossover Population Size"
        [
          bgroup "Naive Vector Seq"
          [
            randBench "Crossover Naive Vector Seq 100" $ crossVectSeq 0.60 (V.replicate 100 (V.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Vector Seq 250" $ crossVectSeq 0.60 (V.replicate 250 (V.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Vector Seq 500" $ crossVectSeq 0.60 (V.replicate 500 (V.replicate 100 (0 :: Int))) 
          ]

       ,  bgroup "Naive Vector Par"
          [
            randBench "Crossover Naive Vector Par 100" $ crossVectPar 0.60 (V.replicate 100 (V.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Vector Par 250" $ crossVectPar 0.60 (V.replicate 250 (V.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Vector Par 500" $ crossVectPar 0.60 (V.replicate 500 (V.replicate 100 (0 :: Int))) 
          ]

        , bgroup "Naive Vector"
          [
            randBench "Crossover Naive Seq 100" $ crossoverSeq 0.60 (S.replicate 100 (S.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Seq 250" $ crossoverSeq 0.60 (S.replicate 250 (S.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Seq 500" $ crossoverSeq 0.60 (S.replicate 500 (S.replicate 100 (0 :: Int))) 
          ]

        , bgroup "Naive Vector"
          [
            randBench "Crossover Naive Par 100" $ crossoverPar 0.60 (S.replicate 100 (S.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Par 250" $ crossoverPar 0.60 (S.replicate 250 (S.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Par 500" $ crossoverPar 0.60 (S.replicate 500 (S.replicate 100 (0 :: Int))) 
          ]

        , bgroup "Geometric"
          [
            randBench "Crossover Geometric 100" $ crossover 0.60 $ pop32All0 100 100
          , randBench "Crossover Geometric 250" $ crossover 0.60 $ pop32All0 250 100
          , randBench "Crossover Geometric 500" $ crossover 0.60 $ pop32All0 500 100
          ]
      ]
    ]

  , bgroup "Random"
    [
      bgroup "Random Lists IO"
      [
        bench "MWC 1 Doubles" $ nfIO $ runWithSystemRandom $ asRandIO $ replicateM 1 (MWC.uniform :: Rand IO Double)
      , bench "PureMT 1 Doubles" $ nfIO $ runRandIO $ replicateM 1 (sample Uni.stdUniform :: RVar Double)
      , bench "Probable 1 Doubles" $ nfIO $ mwc $ replicateM 1 double
      , bench "MWC 100000 Doubles" $ nfIO $ runWithSystemRandom $ asRandIO $ replicateM 100000 (MWC.uniform :: Rand IO Double)
      , bench "PureMT 100000 Doubles" $ nfIO $ runRandIO $ replicateM 100000 (sample Uni.stdUniform :: RVar Double)
      , bench "Probable 100000 Doubles" $ nfIO $ mwc $ replicateM 100000 double
      ]
    ]
  ]

randBench name rand = bench name $ nfIO $ runRandIO rand

simpleEval = return . ones

