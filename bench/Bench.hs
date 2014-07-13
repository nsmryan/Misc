
import Criterion.Main

import Control.Applicative

import qualified Data.Sequence as S
import qualified Data.Vector as V

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
        , randBench "PM Geometric 400" $ pointMutation 0.01 10 8 $ pop32All0 300 10
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
          randBench "Crossover Naive Vector"  $ crossoverNaive 0.06 vectPop
        , randBench "Crossover Geometric"     $ crossover      0.06 seqPop
        ]

      , bgroup "Crossover Rate"
        [
          bgroup "Crossover Naive Vector"
          [
            randBench "Crossover Naive Vector 0.2" $ crossoverNaive 0.2 vectPop
          , randBench "Crossover Naive Vector 0.4" $ crossoverNaive 0.4 vectPop
          , randBench "Crossover Naive Vector 0.6" $ crossoverNaive 0.6 vectPop
          , randBench "Crossover Naive Vector 0.8" $ crossoverNaive 0.8 vectPop
          , randBench "Crossover Naive Vector 1.0" $ crossoverNaive 1.0 vectPop
          ]
          
        , bgroup "Crossover Geometric"
          [
            randBench "Crossover Geometric 0.2" $ crossover 0.2 seqPop
          , randBench "Crossover Geometric 0.4" $ crossover 0.4 seqPop
          , randBench "Crossover Geometric 0.6" $ crossover 0.6 seqPop
          , randBench "Crossover Geometric 0.8" $ crossover 0.8 seqPop
          , randBench "Crossover Geometric 1.0" $ crossover 1.0 seqPop
          ]
        ]

      , bgroup "Crossover Individual Size"
        [
          bgroup "Naive Vector" 
          [
              randBench "Crossover Naive Vector 100" $ crossoverNaive 0.06 (V.replicate 100 (V.replicate 100 (0 :: Int)))
            , randBench "Crossover Naive Vector 200" $ crossoverNaive 0.06 (V.replicate 100 (V.replicate 200 (0 :: Int)))
            , randBench "Crossover Naive Vector 300" $ crossoverNaive 0.06 (V.replicate 100 (V.replicate 300 (0 :: Int)))
            , randBench "Crossover Naive Vector 400" $ crossoverNaive 0.06 (V.replicate 100 (V.replicate 400 (0 :: Int)))
            , randBench "Crossover Naive Vector 500" $ crossoverNaive 0.06 (V.replicate 100 (V.replicate 500 (0 :: Int)))
          ]
        , bgroup "Geometric" 
          [
              randBench "Crossover Geometric 100" $ crossover 0.06 $ pop32All0 100 100
            , randBench "Crossover Geometric 200" $ crossover 0.06 $ pop32All0 100 200
            , randBench "Crossover Geometric 300" $ crossover 0.06 $ pop32All0 100 300
            , randBench "Crossover Geometric 400" $ crossover 0.06 $ pop32All0 100 400
            , randBench "Crossover Geometric 500" $ crossover 0.06 $ pop32All0 100 500
          ]
        ]

      , bgroup "Crossover Population Size"
        [
          bgroup "Naive Vector"
          [
            randBench "Crossover Naive Vector 100" $ crossoverNaive 0.06 (V.replicate 100 (V.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Vector 200" $ crossoverNaive 0.06 (V.replicate 200 (V.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Vector 300" $ crossoverNaive 0.06 (V.replicate 300 (V.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Vector 400" $ crossoverNaive 0.06 (V.replicate 400 (V.replicate 100 (0 :: Int))) 
          , randBench "Crossover Naive Vector 500" $ crossoverNaive 0.06 (V.replicate 500 (V.replicate 100 (0 :: Int))) 
          ]
        , bgroup "Geometric"
          [
            randBench "Crossover Geometric 100" $ crossover 0.06 $ pop32All0 100 100
          , randBench "Crossover Geometric 200" $ crossover 0.06 $ pop32All0 200 100
          , randBench "Crossover Geometric 300" $ crossover 0.06 $ pop32All0 300 100
          , randBench "Crossover Geometric 400" $ crossover 0.06 $ pop32All0 300 100
          , randBench "Crossover Geometric 500" $ crossover 0.06 $ pop32All0 500 100
          ]
        ]
    ]
  ]

randBench name rand = bench name $ nfIO $ runRandIO rand

simpleEval = return . ones
