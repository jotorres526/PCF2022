module ImperativeLanguage where

import LTerm
import BTerm


-- Declaração de todas as construções que fazem um programa, cada uma representa uma regra semântica
data Prog
  = Asg Vars LTerm
  | Wait Double Prog
  | Seq Prog Prog
  | If BTerm Prog Prog
  | While BTerm Prog
  deriving Show

-- Função que altera a memória de uma variável
changeMem :: Vars -> Double -> (Vars -> Double) -> (Vars -> Double)
changeMem v r m = \u -> if u == v then r else m u

-- Tradução direta das regras de semântica apresentadas no enunciado
-- Dado um programa (Prog) e uma memória (Vars -> Double), retorna um par (segundos, memória)
isem :: Prog -> (Vars -> Double) -> (Double, Vars -> Double)
isem (Asg var term) mem = (0.0, changeMem var (sem term mem) mem)
isem (Wait time prog) mem = let (n, mem') = isem prog mem in
                            (time + n, mem')
isem (Seq prog1 prog2) mem = let (n, mem') = isem prog1 mem in
                             let (m, mem'') = isem prog2 mem' in
                             (n + m, mem'')
isem (If cond ifBody elseBody) mem = if bsem cond mem
                                     then isem ifBody mem
                                     else isem elseBody mem
isem (While cond prog) mem = if bsem cond mem
                             then let (n, mem') = isem prog mem in
                                  let (m, mem'') = isem (While cond prog) mem' in
                                  (n + m, mem'')
                             else (0, mem)

-- Função que avalia a memória de uma variável e o tempo de execução do programa
-- Recebe um programa, uma memória e a variável a testar
-- Retorna um par onde p1 = tempo de execução; p2 = valor da variável
eval :: Prog -> (Vars -> Double) -> Vars -> (Double, Double)
eval prog mem var = let (time, value) = isem prog mem in
                    (time, value var)   


program1 = Asg X one
program2 =
  While
    (Geq y x)
    (Wait 5
      (Seq
        (Asg X (Add x y))
        (Asg Y (Add y (Leaf (Right 1))))))
