module Main where

import Ex2

main =
  putStrLn $ unlines
    [ "Running Exercise2"

    -- Tests f1, f2, f3 follow format
    -- Empty input
    -- One int before to check indexing
    -- First element 
    -- ...
    -- [1...1000] to check rollover and bigger values 

    -- f1 tests
    , "f1 ([] :: [Int]) = " ++ show (f1 ([] :: [Int]))       -- [] need to define type bc compiler needs type
    , "f1 [1..328] = " ++ show (f1 [1..328])                 -- []
    , "f1 [1..329] = " ++ show (f1 [1..329])                 -- [329]
    , "f1 [1..658] = " ++ show (f1 [1..658])                 -- [329,658]
    , "f1 [1..1000] = " ++ show (f1 [1..1000])               -- [329,658,987]

    -- f2 tests
    , "f2 [] = " ++ show (f2 [])                             -- 0 
    , "f2 [1..196] = " ++ show (f2 [1..196])                 -- 0
    , "f2 [1..197] = " ++ show (f2 [1..197])                 -- 197
    , "f2 [1..394] = " ++ show (f2 [1..394])                 -- 591
    , "f2 [1..1000] = " ++ show (f2 [1..1000])               -- 2955

    -- f3 tests
    , "f3 [] = " ++ show (f3 [])                             -- 1
    , "f3 [1..209] = " ++ show (f3 [1..209])                 -- 1
    , "f3 [1..210] = " ++ show (f3 [1..210])                 -- 210
    , "f3 [1..420] = " ++ show (f3 [1..420])                 -- 88200
    , "f3 [1..630] = " ++ show (f3 [1..630])                 -- 55566000
    , "f3 [1..840] = " ++ show (f3 [1..840])                 -- 675440000
    , "f3 [1..1000] = " ++ show (f3 [1..1000])               -- 675440000

    -- f4 tests
    , "f4 [Just 42] = " ++ show (f4 [Just 42])               -- (0, [])
    , "f4 [] = " ++ show (f4 [])                             -- (0, [])

    -- opcode 1
    , "f4 [Just 93, Just 2, Just 4, Just 6, Just 2, Just 1, Just 2, Just 3] = "
      ++ show (f4 [Just 93, Just 2, Just 4, Just 6, Just 2, Just 1, Just 2, Just 3]) -- 14, [1, 2, 3]

    , "f4 [Just 93, Just 2, Nothing, Just 6, Just 1, Just 2, Just 3] = "
    ++ show (f4 [Just 93, Just 2, Nothing, Just 6, Just 1, Just 2, Just 3]) -- 2, [6, 1, 2, 3]

    -- opcode 2
    , "f4 [Just 95, Just 2, Just 4, Just 6, Just 2, Just 2, Just 1, Just 2, Just 3] = "
    ++ show (f4 [Just 95, Just 2, Just 4, Just 6, Just 2, Just 2, Just 1, Just 2, Just 3]) -- 16, [1, 2, 3]

    , "f4 [Just 95, Just 2, Just 4, Nothing, Just 6, Just 2, Just 2, Just 1, Just 2, Just 3] = "
    ++ show (f4 [Just 95, Just 2, Just 4, Nothing, Just 6, Just 2, Just 2, Just 1, Just 2, Just 3]) -- 16, [1, 2, 3]

    -- opcode 3
    , "f4 [Just 87, Just 2, Just 4] = "
    ++ show (f4 [Just 87, Just 2, Just 4]) -- 6, []
  
    , "f4 [Just 87, Just 2, Just 4, Nothing, Just 6] = "
    ++ show (f4 [Just 87, Just 2, Just 4, Nothing, Just 6]) -- 6, [6]

    -- opcode 4
    , "f4 [Just 18, Just 2, Just 4, Just 6, Just 2, Just 16, Just 1, Just 2] = "
    ++ show (f4 [Just 18, Just 2, Just 4, Just 6, Just 2, Just 16, Just 1, Just 2]) -- 14, [1, 2]

    , "f4 [Just 2, Just 3, Just 18, Just 2, Just 4, Nothing, Just 16, Just 1, Just 2] = "
    ++ show (f4 [Just 2, Just 3, Just 18, Just 2, Just 4, Nothing, Just 16, Just 1, Just 2]) -- 6, [1, 2]

    -- opcode 5
    , "f4 [Just 99, Just 2, Just 4, Just 6, Just 2, Just 13, Just 1, Just 2] = "
    ++ show (f4 [Just 99, Just 2, Just 4, Just 6, Just 2, Just 13, Just 1, Just 2]) -- 14, [1, 2]

    , "f4 [Just 99, Just 2, Just 4, Nothing, Just 6, Just 13, Just 1, Just 2] = "
    ++ show (f4 [Just 99, Just 2, Just 4, Nothing, Just 6, Just 13, Just 1, Just 2]) -- 12, [1, 2]

    -- opcode 6
    , "f4 [Just 66, Just 2, Just 4, Just 6, Just 2, Just 15, Just 1, Just 2] = "
    ++ show (f4 [Just 66, Just 2, Just 4, Just 6, Just 2, Just 15, Just 1, Just 2]) -- 14, [1, 2]

    , "f4 [Just 66, Just 2, Nothing, Just 6, Just 2, Just 15, Just 1, Just 2] = "
    ++ show (f4 [Just 66, Just 2, Nothing, Just 6, Just 2, Just 15, Just 1, Just 2]) -- 17, [1, 2]

    -- opcode 7
    , "f4 [Just 64, Just 2, Just 4, Just 6, Just 2, Just 1, Just 2, Just 3] = "
      ++ show (f4 [Just 64, Just 2, Just 4, Just 6, Just 2, Just 1, Just 2, Just 3]) -- 96, [1, 2, 3]

    , "f4 [Just 64, Just 2, Nothing, Just 6, Just 1, Just 2, Just 3] = "
    ++ show (f4 [Just 64, Just 2, Nothing, Just 6, Just 1, Just 2, Just 3]) -- 2, [6, 1, 2, 3]
  
    -- opcode 8
    , "f4 [Just 49, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 1, Just 2] = "
    ++ show (f4 [Just 49, Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 1, Just 2]) -- 720 [1, 2]

    , "f4 [Just 49, Just 1, Just 2, Just 3, Nothing, Just 5, Just 6, Just 1, Just 2] = "
    ++ show (f4 [Just 49, Just 1, Just 2, Just 3, Nothing, Just 5, Just 6, Just 1, Just 2]) -- 180 [1, 2]

    -- opcode 9
    , "f4 [Just 47, Just 2, Just 4] = "
    ++ show (f4 [Just 47, Just 2, Just 4]) -- 8, []

    , "f4 [Just 47, Just 2, Just 4, Nothing, Just 6] = "
    ++ show (f4 [Just 47, Just 2, Just 4, Nothing, Just 6]) -- 64, [6]

    -- opcode 10
    , "f4 [Just 28, Just 2, Just 4, Just 6, Just 2, Just 16, Just 1, Just 2] = "
    ++ show (f4 [Just 28, Just 2, Just 4, Just 6, Just 2, Just 16, Just 1, Just 2]) -- 96, [1, 2]
    
    , "f4 [Just 2, Just 3, Just 28, Just 2, Just 4, Nothing, Just 16, Just 1, Just 2] = "
    ++ show (f4 [Just 2, Just 3, Just 28, Just 2, Just 4, Nothing, Just 16, Just 1, Just 2]) -- 8, [16, 1, 2]
    
    -- opcode 11
    , "f4 [Just 71, Just 2, Just 4, Just 6, Just 2, Just 16, Just 1, Just 2] = "
    ++ show (f4 [Just 71, Just 2, Just 4, Just 6, Just 2, Just 16, Just 1, Just 2]) -- 96, [1, 2]

    , "f4 [Just 71, Just 2, Just 4, Nothing, Just 2, Just 16, Just 1, Just 2] = "
    ++ show (f4 [Just 71, Just 2, Just 4, Nothing, Just 2, Just 16, Just 1, Just 2]) -- 16, [1, 2]

    -- opcode 12
    , "f4 [Just 89, Just 2, Just 4, Just 6, Just 2, Just 16, Just 1, Just 2] = "
    ++ show (f4 [Just 89, Just 2, Just 4, Just 6, Just 2, Just 16, Just 1, Just 2]) -- 96, [1, 2]

    , "f4 [Just 89, Just 2, Just 4, Nothing, Just 2, Just 16, Just 1, Just 2] = "
    ++ show (f4 [Just 89, Just 2, Just 4, Nothing, Just 2, Just 16, Just 1, Just 2]) -- 32, [1, 2]

    -- f5 tests
    , "f5 [] = " ++ show (f5 [])  -- []
    
    , "f5 [Just 42] = " ++ show (f5 [Just 42])
    ]

