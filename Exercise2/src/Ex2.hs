module Ex2 where

rollover :: Int ; rollover = 1000000000

add :: Int -> Int -> Int
add x y = (x+y) `mod` rollover

mul :: Int -> Int -> Int
mul x y
  | p == 0    = 1
  | otherwise = p
  where p = (x*y) `mod` rollover

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- *** Q1,2,3 (8 marks)
-- Hint for Q1,2,3: taking every 3rd element of [1..13] returns [3,6,9,12]

-- *** Q1
-- returns a list of every 329th element of its input
f1 :: [a] -> [a]    -- DO NOT CHANGE !
f1 [] = []
f1 xs = case drop 328 xs of
  []     -> []                      
  (y:ys) -> y : f1 (drop 329 xs)    

-- *** Q2
-- sums every 197th element of its input
-- if list is too short it returns 0
-- you can use `add` or `(+)` here - won't effect grading
f2 :: [Int] -> Int    -- DO NOT CHANGE !
f2 [] = 0
f2 ns = case drop 196 ns of
  []     -> 0
  (y:ys) -> add y (f2 (drop 197 ns)) 

-- *** Q3 
-- multiplies every 210th element of its input
-- if list is too short it returns 1
-- you can use `mul` or `(*)` here - won't effect grading
f3 :: [Int] -> Int    -- DO NOT CHANGE !
f3 [] = 1
f3 ns = case drop 209 ns of 
  []     -> 1
  (y:ys) -> mul y (f3 (drop 210 ns)) 
  
-- *** Q4 (9 marks)
-- Operation Table (See Exercise2 description on BB)
--    ____________________________________________
--    | opcode | operation | operands  | Nothing |
--    --------------------------------------------
--    |   93   |    add    | fixed  4  | term    |
--    |   95   |    add    | fixed  5  | skip    |
--    |   87   |    add    | fixed  3  | 0       |
--    |   18   |    add    | stop@ 16  | term    |
--    |   99   |    add    | stop@ 13  | skip    |
--    |   66   |    add    | stop@ 15  | 7       |
--    |   64   |    mul    | fixed  4  | term    |
--    |   49   |    mul    | fixed  6  | skip    |
--    |   47   |    mul    | fixed  3  | 8       |
--    |   28   |    mul    | stop@ 16  | term    |
--    |   71   |    mul    | stop@ 16  | skip    |
--    |   89   |    mul    | stop@ 16  | 2       |
--    --------------------------------------------
-- initially, skip any number that is not an opcode
-- if called with [], return `(0,[])`
-- if no numbers found after an `add` opcode, return (0,[])
-- if no numbers found after an `mul` opcode, return (1,[])
-- if list ends midway through opcode processing, return result so far
-- if a Nothing is skipped for a fixed N opcode,
--    that Nothing does not contribute to the count.
-- Hint:
--   When building a list for test purposes,
--   remember a value of type `Maybe a` needs to be built
--   using one of the two data constructors of the `Maybe` type.

type Operation = Int -> Int -> Int
data OperandRule = Fixed Int | StopAt Int
data NothingRule = Term | Skip | Default Int

data OpcodeInfo = OpcodeInfo {
  operation :: Operation,
  operandRule :: OperandRule,
  nothingRule :: NothingRule
}

opcodeTable :: [(Int, OpcodeInfo)]
opcodeTable = [
  (93, OpcodeInfo (+) (Fixed 4) Term),
  (95, OpcodeInfo (+) (Fixed 5) Skip),
  (87, OpcodeInfo (+) (Fixed 3) (Default 0)),
  (18, OpcodeInfo (+) (StopAt 16) Term),
  (99, OpcodeInfo (+) (StopAt 13) Skip),
  (66, OpcodeInfo (+) (StopAt 15) (Default 7)),
  (64, OpcodeInfo (*) (Fixed 4) Term),
  (49, OpcodeInfo (*) (Fixed 6) Skip),
  (47, OpcodeInfo (*) (Fixed 3) (Default 8)),
  (28, OpcodeInfo (*) (StopAt 16) Term),
  (71, OpcodeInfo (*) (StopAt 16) Skip),
  (89, OpcodeInfo (*) (StopAt 16) (Default 2)) ]

f4 :: [Maybe Int] -> (Int,[Maybe Int])    -- DO NOT CHANGE !
f4 [] = (0,[])
f4 (x:xs) = case x of
  Just opcode -> case lookup opcode opcodeTable of 
    Just opcodeInfo ->
      let acc = if opcode `elem` [93,95,87,18,99,66] then 0 else 1
      in processList opcodeInfo xs acc
    Nothing -> f4 xs -- skip non opcodes 
  Nothing -> f4 xs -- skip corrupted values 

processList :: OpcodeInfo -> [Maybe Int] -> Int -> (Int, [Maybe Int])
processList opcodeInfo [] acc = (acc, []) 
processList opcodeInfo (y:ys) acc =
  case operandRule opcodeInfo of
    Fixed n -> processFixed opcodeInfo (y:ys) acc n
    StopAt k -> processStopAt opcodeInfo (y:ys) acc k

processFixed :: OpcodeInfo -> [Maybe Int] -> Int -> Int -> (Int, [Maybe Int])
processFixed _ [] acc _ = (acc, []) 
processFixed _ rest acc 0 = (acc, rest) 
processFixed info (y:ys) acc n = case y of
  Just val ->
    let newAcc = operation info acc val
    in processFixed info ys newAcc (n-1)
  Nothing -> case nothingRule info of
    Term -> (acc, ys) 
    Skip -> processFixed info ys acc n 
    Default d-> 
      let newAcc = operation info acc d 
      in processFixed info ys newAcc (n-1)

processStopAt :: OpcodeInfo -> [Maybe Int] -> Int -> Int -> (Int, [Maybe Int])
processStopAt _ [] acc _ = (acc, []) 
processStopAt info (y:ys) acc stopVal = case y of
  Just val | val == stopVal -> (acc, ys)
  Just val ->
    let newAcc = operation info acc val
    in processStopAt info ys newAcc stopVal
  Nothing -> case nothingRule info of
      Term -> (acc, ys) 
      Skip -> processStopAt info ys acc stopVal 
      Default d-> 
        let newAcc = operation info acc d 
        in processStopAt info ys newAcc stopVal

-- *** Q5 (3 marks)
-- uses `f4` to process all the opcodes in the maybe list,
-- by repeatedly applying it to the leftover part
-- Note: this will be tested against a correct version of `f4`,
--       rather than your submission.
f5 :: [Maybe Int] -> [Int]    -- DO NOT CHANGE !
f5 [] = [] 
f5 xs =
  let (result, rest) = f4 xs
  in result : f5 rest

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

