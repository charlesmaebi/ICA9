{-
CS 381 — ICA 9 Template
Name: Charles Aebi
Expr2 with Either + Division

IMPORTANT:
Rename this file to ICA9.hs before submitting.

Do NOT use library helpers such as:
mapM, sequence, traverse

The goal is to practice writing the recursion yourself
so you can clearly see how evaluation stops and how
errors propagate.
-}

-----------------------------
-- Expression Language
-----------------------------

data Expr
  = N Int
  | Plus Expr Expr
  | Mult Expr Expr
  | Neg Expr
  | Equal Expr Expr
  | Not Expr
  | Div Expr Expr        -- NEW
  deriving (Eq)

-----------------------------
-- Pretty Printing (Provided)
-----------------------------

instance Show Expr where
  show (N n) = show n
  show (Plus e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Mult e1 e2)  = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Neg e)       = "(-" ++ show e ++ ")"
  show (Equal e1 e2) = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
  show (Not e)       = "(not " ++ show e ++ ")"
  show (Div e1 e2)   = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"

-----------------------------
-- Values (Object Language)
-----------------------------

data Val
  = I Int
  | B Bool
  deriving (Show, Eq)

-----------------------------
-- Typed Errors (Extensible)
-----------------------------

-- When constructing a TypeMismatch, include a short message.
-- A good pattern is to prefix the operator:
--
--   TypeMismatch "Plus: expected Int + Int"
--   TypeMismatch "Not: expected Bool"
--   TypeMismatch "Div: expected Int / Int"
--
-- Your wording does NOT need to match exactly.
-- Just make the error clear.

data Error
  = TypeMismatch String
  | DivideByZero
  deriving (Show, Eq)

-----------------------------
-- Semantics
-----------------------------

-- sem now returns Either Error Val

sem :: Expr -> Either Error Val

-- Integer literal
sem (N i) = Right (I i)

-------------------------------------------------
-- PLUS (FULLY IMPLEMENTED EXAMPLE)
-------------------------------------------------
-- Use this as your model for the remaining cases.
-- Notice how errors are propagated immediately.

sem (Plus e1 e2) =
  case (sem e1, sem e2) of
    (Right (I i), Right (I j)) -> Right (I (i + j))
    (Left err, _)              -> Left err
    (_, Left err)              -> Left err
    _ -> Left (TypeMismatch "Plus: expected Int + Int")

-------------------------------------------------
-- TODO: Implement the rest following the pattern
-------------------------------------------------

sem (Mult e1 e2) =
  case (sem e1, sem e2) of
    (Right (I i), Right (I j)) -> Right (I (i * j))
    (Left err, _)              -> Left err
    (_, Left err)              -> Left err
    _ -> Left (TypeMismatch "Mult: expected Int * Int")

sem (Neg e) =
  case sem e of 
    Right (I i) -> Right (I (-i))
    Left err    -> Left err
    _ -> Left (TypeMismatch "Neg: expected Int")

sem (Equal e1 e2) =
  case (sem e1, sem e2) of
    (Right v1, Right v2) -> Right (B (v1 == v2))
    (Left err, _)        -> Left err
    (_, Left err)        -> Left err

sem (Not e) =
  case sem e of
    Right (B b) -> Right (B (not b))
    Left err    -> Left err
    _ -> Left (TypeMismatch "Not: expected Bool")

-------------------------------------------------
-- DIV (NEW)
-------------------------------------------------
-- Rules:
-- 1. Both operands must be integers
-- 2. Division by zero -> Left DivideByZero
-- 3. Otherwise return integer division using `div`

sem (Div e1 e2) =
  case (sem e1, sem e2) of
    (Right (I i), Right (I 0)) -> Left DivideByZero
    (Right (I i), Right (I j)) -> Right (I (i `div` j))
    (Left err, _)               -> Left err
    (_, Left err)               -> Left err
    _ -> Left (TypeMismatch "Div: expected Int / Int")

-----------------------------
-- Running a Program
-----------------------------

type Prog = [Expr]

runProg :: Prog -> Either Error [Val]

runProg [] = Right []

runProg (e:es) =
  case sem e of
    Left err -> Left err
    Right v  ->
      case runProg es of
        Left err  -> Left err
        Right vs  -> Right (v:vs)

-----------------------------
-- Sample Programs for Part E
-----------------------------
-- A program with at least three expressions (including one Div) 
-- that evaluates successfully.
progOK :: Prog
progOK = 
  [ N 10
  , Plus (N 3) (N 4)
  , Div (N 20) (N 5)
  ]

-- A program with at least three expressions whose first failure 
-- is the second expression and it produces Left DivideByZero.
progZero :: Prog
progZero = 
  [ N 7
  , Div (N 5) (N 0)
  , Plus (N 1) (N 2)
  ]

-- A program with at least three expressions whose first expression 
-- is a failure that produces Left (TypeMismatch ...).
progBad :: Prog
progBad = 
  [ Plus (N 1) (Not (N 0))
  , Div (N 10) (N 2)
  , N 5
  ]

-----------------------------
-- Optional Main (Helpful)
-----------------------------

main :: IO ()
main = do
  putStrLn "Running progOK:"
  print (runProg progOK)

  putStrLn "\nRunning progZero:"
  print (runProg progZero)

  putStrLn "\nRunning progBad:"
  print (runProg progBad)
