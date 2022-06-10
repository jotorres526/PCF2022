module BTerm where

import LTerm

data BTerm = Geq LTerm LTerm
           | Conj BTerm BTerm
           | Neg BTerm
           deriving Show

bsem :: BTerm -> (Vars -> Double) -> Bool 
bsem (Geq t1 t2) m =
  let r1 = sem t1 m
      r2 = sem t2 m
   in r1 <= r2

bsem (Neg t1) m =
  let v = bsem b m
   in not v
  
bsem (Conj b1 b2) m =
  let r1 = bsem b1 m
      r2 = bsem b2 m
   in r1 && r2

b = Geq t t'
c = Neg b
