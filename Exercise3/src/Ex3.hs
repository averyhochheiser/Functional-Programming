module Ex3 where

-- required for all Qs:
data MathExpr2 -- the expression datatype
  = Literal Float -- floating-point value
  | VName String -- variable/identifier name
  | RatioOf MathExpr2 MathExpr2 -- divide first by second
  | Subtract MathExpr2 MathExpr2 -- subtracts second from first
  | Negate MathExpr2 -- numerical negation (-x)
  -- the following are boolean expressions (using numbers)
  -- the number 0.0 represents False, all others represent True.
  | Not MathExpr2 -- logical not
  | IsLess MathExpr2 MathExpr2 -- True if first is less than second
  | EqToZero MathExpr2 -- True if numeric value is zero
  deriving (Eq, Ord, Show)

type Dict = [(String, Float)]

insert :: String -> Float -> Dict -> Dict
insert s f d = (s, f) : d

find :: String -> Dict -> Maybe Float
find s [] = Nothing
find s ((t, f) : d)
  | s == t = Just f
  | otherwise = find s d

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- Implement the following function (which may have runtime errors)
-- This will only be graded using dictionaries and values that do
-- not result in runtime errors in a correct implementation.
eval :: Dict -> MathExpr2 -> Float
eval _ (Literal n) = n
eval d (VName x) = case lookup x d of Just y -> y
eval d (RatioOf e1 e2) = eval d e1 / eval d e2
eval d (Subtract e1 e2) = eval d e1 - eval d e2
eval d (Negate e) = -(eval d e)
eval d (Not e) = if eval d e == 0.0 then 1.0 else 0.0
eval d (IsLess e1 e2) = if eval d e1 < eval d e2 then 1.0 else 0.0
eval d (EqToZero e) = if eval d e == 0.0 then 1.0 else 0.0

-- Q2 (9 marks)
-- Implement the following function (which always returns a value)
-- Grading of this will put emphasis on cases that would cause a
-- runtime error for Q1.
meval :: Dict -> MathExpr2 -> Maybe Float
meval _ (Literal n) = Just n
meval d (VName x) = lookup x d
meval d (RatioOf e1 e2) = do
  v1 <- meval d e1
  v2 <- meval d e2
  if v2 == 0.0 then Nothing else Just (v1 / v2)
meval d (Subtract e1 e2) = do
  v1 <- meval d e1
  v2 <- meval d e2
  Just (v1 - v2)
meval d (Negate e) = fmap negate (meval d e)
meval d (Not e) = do
  v <- meval d e
  Just (if v == 0.0 then 1.0 else 0.0)
meval d (IsLess e1 e2) = do
  v1 <- meval d e1
  v2 <- meval d e2
  Just (if v1 < v2 then 1.0 else 0.0)
meval d (EqToZero e) = do
  v <- meval d e
  Just (if v == 0.0 then 1.0 else 0.0)

-- Q3 (8 marks)
-- Laws of Arithmetic for this question:
--    x - 0 = x
--    0 = x - x
-- The following function should implement simplifications
-- using ONLY the above two laws, wherever they apply.
simp :: MathExpr2 -> MathExpr2
simp (Literal n) = Literal n
simp (VName x) = VName x
simp (RatioOf e1 e2) = RatioOf (simp e1) (simp e2)
simp (Subtract e1 e2)
  | s2 == Literal 0 = s1 -- x - 0 = x
  | s1 == s2 = Literal 0 -- 0 = x - x
  | otherwise = Subtract s1 s2
  where
    s1 = simp e1
    s2 = simp e2
simp (Negate e) = Negate (simp e)
simp (Not e) = Not (simp e)
simp (IsLess e1 e2) = IsLess (simp e1) (simp e2)
simp (EqToZero e) = EqToZero (simp e)

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...
