{- Assignment 4
 - Name: Sai Santhosh Thunga
 - Date: November 29th, 2020
 -}
module Assign_4 where

import Test.QuickCheck (quickCheck)

macid :: String
macid = "thungas"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                abs (2*X + 3 + 4)  * X
 -          can be encoded as
 -                Prod [ Abs (Sum [Prod [Coef 2.0,X],Coef 3,Coef 4]),X]
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Sum (MathExpr a) (MathExpr a)
  | Prod (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
    deriving (Eq,Show,Read)

{- --------------------------------------------------------------------
 - Function: eval
 - --------------------------------------------------------------------
 - Description: Evaluates the MathExpr at some value v
 - ------------------------------------------------------------------------
 - |  Input   | The first parameter is of type MathExpr a                                      
              | v takes any input a that belongs to both the Floating and Eq typeclass      
 - ------------------------------------------------------------------------
 - |  Output  | Evaluates MathExpr a at some value v and returns the value of type a
 - -----------------------------------------------------------------------|
 -}

eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v            = v
eval (Coef n) _     = n
eval (Sum p1 p2) v  = eval p1 v + eval p2 v
eval (Prod p1 p2) v = eval p1 v * eval p2 v
eval (Power p1 b) v = eval p1 v ^ b
eval (Cos p1) v     = cos $ eval p1 v
eval (Sin p1) v     = sin $ eval p1 v
eval (Abs p1) v     = abs $ eval p1 v


{- --------------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Defined Num typeclass instances for data type MathExpr.
 - --------------------------------------------------------------------
 -}

instance Num a => Num (MathExpr a) where
  x + y         = Sum x y 
  x * y         = Prod x y
  negate x      = Prod x $ Coef $ -1
  abs x         = Abs x
  fromInteger i = Coef $ fromInteger i
  signum _      = error "signum n is left un-implemented"

{- --------------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Defined Fractional typeclass instances for data type MathExpr
 - --------------------------------------------------------------------
 -}

instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e $ negate 1
  fromRational e = Coef $ fromRational e

{- --------------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Defined Floating typeclass instances for data type MathExpr
 - ------------------------------------------------------------------------
 -}

instance Floating a => Floating (MathExpr a) where
  pi      = pi
  sin     = Sin
  cos     = Cos
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- ------------------------------------------------------------------
 - diff
 - ------------------------------------------------------------------
 - Description: Takes a math expression and outputs the derivitive of the math expression
 - ------------------------------------------------------------------------
 - |  Input      | Takes an input of MathExpr a type                      | 
 - ------------------------------------------------------------------------
 - |  Output     | Returns a MathExpr a                                   |
 - -----------------------------------------------------------------------|
 -}

diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X            = 1
diff (Coef _)     = 0
diff (Sum p1 p2)  = diff p1 + diff p2
diff (Prod p1 p2) = Sum (Prod (diff p1) p2) $ Prod p1 $ diff p2
diff (Power p1 b) = Prod (Prod (Coef $ fromIntegral b) $ Power p1 $ b-1) $ diff p1 
diff (Cos p1)     = Prod (Prod (Coef $ negate 1) $ Sin p1) $ diff p1
diff (Sin p1)     = Prod (Cos p1) $ diff p1
diff (Abs p1)     = Prod (Prod p1 $ Power (Abs p1) $ negate 1) $ diff p1

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description: Creates a string representation of the Math Expression
 - ------------------------------------------------------------------------
 - |  Input      | Takes an input of MathExpr a type                     |
 - ------------------------------------------------------------------------
 - |  Output     | Returns a String respresenting the MathExpr inputted  |
 - -----------------------------------------------------------------------|
 -}
 
pretty :: (Show a) => MathExpr a -> String
pretty X            = "X"
pretty (Coef n)     = show n
pretty (Sum p1 p2)  = "(" ++ pretty p1 ++ " + " ++ pretty p2 ++ ")"
pretty (Prod p1 p2) = "(" ++ pretty p1 ++ " * " ++ pretty p2 ++ ")"
pretty (Power p1 b) = "(" ++ pretty p1 ++ " ^^ " ++ show b ++ ")"
pretty (Cos p1)     = "cos" ++ "(" ++ pretty p1 ++ ")"
pretty (Sin p1)     = "sin" ++ "(" ++ pretty p1 ++ ")"
pretty (Abs p1)     = "abs" ++ "(" ++ pretty p1 ++ ")"

{- -----------------------------------------------------------------
 - Test Cases

Function: eval
Test Case Number: #1
Input: eval (Sum (Sum X X) (Sum X X)) 4
Expected Output: 16.0
Actual Output: 16.0

Function: eval
Test Case Number: #2
Input: eval (Prod (Cos X) (Cos X)) (pi/4)
Expected Output: 0.5
Actual Output: 0.5000000000000001

Function: eval
Test Case Number: #3
Input: eval (Prod (Power X 10) (Power (Sin X) 4)) 0
Expected Output: 0.0
Actual Output: 0.0




Function: diff
Test Case Number: #1
Input: diff (Power X 100000)
Expected Output: Prod (Prod (Coef 100000.0) (Power X 99999)) (Coef 1.0)
Actual Output: Prod (Prod (Coef 100000.0) (Power X 99999)) (Coef 1.0)

Function: diff
Test Case Number: #2
Input: diff (Power (Cos X) 100000)
Expected Output: Prod (Prod (Coef 100000.0) (Power (Cos X) 99999)) 
                      (Prod (Prod (Coef (-1.0)) (Sin X)) (Coef 1.0))
Actual Output: Prod (Prod (Coef 100000.0) (Power (Cos X) 99999)) 
                    (Prod (Prod (Coef (-1.0)) (Sin X)) (Coef 1.0))

Function: diff
Test Case Number: #3
Input: diff (Prod (Abs(Sin X)) X)
Expected Output: Sum (Prod (Prod (Prod (Sin X) (Power (Abs (Sin X)) (-1))) 
                     (Prod (Cos X) (Coef 1.0))) X) (Prod (Abs (Sin X)) (Coef 1.0))
Actual Output: Sum (Prod (Prod (Prod (Sin X) (Power (Abs (Sin X)) (-1))) 
                   (Prod (Cos X) (Coef 1.0))) X) (Prod (Abs (Sin X)) (Coef 1.0))




Function: pretty
Test Case Number: #1
Input: pretty (Sum X (Prod (Coef 3) (Sin X)))
Expected Output: "(X + (3 * sin(X)))"
Actual Output: "(X + (3 * sin(X)))"

Function: pretty
Test Case Number: #2
Input: pretty (diff (Power X 10))
Expected Output: "((10.0 * (X ^^ 9)) * 1.0)"
Actual Output: "((10.0 * (X ^^ 9)) * 1.0)"

Function: pretty
Test Case Number: #3
Input: pretty (diff (Power (Cos X) 100000))
Expected Output: "((100000.0 * (cos(X) ^^ 99999)) * ((-1.0 * sin(X)) * 1.0))"
Actual Output: "((100000.0 * (cos(X) ^^ 99999)) * ((-1.0 * sin(X)) * 1.0))"

 - -----------------------------------------------------------------
 -}

-- QucikCheck Property

evalProp :: Double -> Double -> Bool
evalProp r s =  
  eval (Sum p q)   0 == r + s &&
  eval (Prod p q)  0 == r * s &&
  eval (Cos p)     0 == cos r &&
  eval (Power p 0) s == 1     &&
  eval (Sin p)     0 == sin r &&
  eval (Abs p)     0 == abs (abs r)
    where
       p = Coef r
       q = Coef s


diffProp :: Double -> Double -> Bool
diffProp r s = 
  diff X                     == 1 &&
  diff (Coef p)              == 0 &&
  diff (Sum p q)             == diff p + diff q &&
  diff (Prod p q)            == Sum (Prod (diff p) q) (Prod p $ diff q) &&
  diff (Sin p)               == Cos p * diff p &&
  diff (Cos p)               == Prod (Prod (Coef $ negate 1) $ Sin p) (diff p) &&
  diff (Power p 1)           == Prod (Prod (Coef 1) $ Power p 0) (diff p) &&
  ((r == 0) || (diff (Abs p) == Prod (Prod p (Power (Abs p) $ negate 1)) (diff p)))
    where
      p = Coef r
      q = Coef s


{-

-- QuickCheck

-----------------------------------------------

Function: eval
Property: evalProp :: Double -> Double -> Bool

evalProp r s =  
  eval (Sum p q)   0 == r + s &&
  eval (Prod p q)  0 == r * s &&
  eval (Cos p)     0 == cos r &&
  eval (Power p 0) s == 1     &&
  eval (Sin p)     0 == sin r &&
  eval (Abs p)     0 == abs (abs r)
    where
       p = Coef r
       q = Coef s

Actual Text Result: Pass

-----------------------------------------------

Function: diff
Property: diffProp :: Double -> Double -> Bool

diffProp r s = 
  diff X == 1 &&
  diff (Coef p) == 0 &&
  diff (Sum p q) == diff p + diff q &&
  diff (Prod p q) == Sum (Prod (diff p) q) (Prod p $ diff q) &&
  diff (Sin p) == Cos p * diff p &&
  diff (Cos p) == Prod (Prod (Coef $ negate 1) $ Sin p) (diff p) &&
  diff (Power p 1) == Prod (Prod (Coef 1) $ Power p 0) (diff p) &&
  ((r == 0) || (diff (Abs p) == Prod (Prod p (Power (Abs p) $ negate 1)) (diff p)))
    where
      p = Coef r
      q = Coef s

Actual Text Result: Pass

-}
