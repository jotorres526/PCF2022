module LTerm where

data Vars = X | Y | Z deriving (Show, Eq)
data LTerm = Leaf (Either Vars Double) 
           | Mult Double LTerm 
           | Add LTerm LTerm
           deriving Show 

sem :: LTerm -> (Vars -> Double) -> Double
sem (Leaf (Left v)) m = m v -- regra var
sem (Leaf (Right r)) m = r -- regra con
sem (Mult s t) m = let r = sem t m in s * r -- regra scl
sem (Add t1 t2) m =
  let r1 = sem t1 m -- regra add
      r2 = sem t2 m
   in r1 + r2

x = Leaf (Left X)
y = Leaf (Left Y)
z = Leaf (Left Z)
t = Add x (Mult 2 y)
t' = Add (Mult 2 x) (Mult 2 y)
t'' = Add (Mult 3 (Mult 2 x)) (Mult 2 (Add y z))

m X = 3
m Y = 4
m Z = 5