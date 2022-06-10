module WhileLanguage where
import LTerm
import BTerm

one = Leaf (Right 1)
data Prog = Ass Vars LTerm
          | Seq Prog Prog
          | Cond BTerm Prog Prog
          | While BTerm Prog
          deriving Show

changeMem :: Vars -> Double -> (Vars -> Double) -> (Vars -> Double)
changeMem v r m = \u -> if u == v then r else m u

wsem :: Prog -> (Vars -> Double) -> (Vars -> Double)
wsem (Ass v t) m = changeMem v (sem t m) m
wsem (Seq p q) m =
  let m'  = wsem p m
      m'' = wsem q m' in m''
wsem (Cond b p q) m =
  if bsem b m then wsem p m
  else wsem q m
wsem (While b p) m =
    if bsem b m then
      let m' = wsem p m
       in wsem (While b p) m'
    else
      m
pr = While (Geq x y)
      (Seq
        (Ass X
          (Add x y))
        (Ass Y
          (Add y one)))
