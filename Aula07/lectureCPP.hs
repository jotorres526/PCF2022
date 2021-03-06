{- Cyber-Physical Programming 2021/2022 - Recalling Haskell.
 - Why Haskell? We will use Haskell in this course to implement 
 - programming languages and respective semantics.
 - Author: Renato Neves.
 - Goal: Solve the exercises listed below.
-}

module LectureCPC where
import Nat
import List
import Cp
import Data.Char
-- Basic functions and conditionals -- 

-- Implement the function that returns 
-- the maximum of two integers.
max' :: (Int,Int) -> Int
max' (a,b) | a < b = b
           | otherwise = a     

-- Implement the function that returns 
-- the maximum of three integers.
max3 :: (Int,(Int,Int)) -> Int
max3 (a,(b,c)) = max' (a, (max' (b,c))) 

-- Implement the function that scales 
-- a 2-dimensional real vector by a 
-- real number.
scaleV :: (Double, (Double,Double)) -> (Double, Double)
scaleV (s, (v1, v2)) = (s*v1, s*v2)

-- Implement the function that adds up
-- two 2-dimensional real vectors
addV :: ((Double,Double),(Double,Double)) -> (Double, Double)
addV ((v1, v2), (u1, u2)) = (v1+u1, v2+u2)

-- Type Synonims
type TSMatrix = ((Double,Double),(Double,Double))
type TVector = (Double,Double)

--multM :: (TSMatrix,TVector) -> TVector
--multM (((m11,m21)(m21,m22)), (n1,n2)) = (m11*n1 + m21*n1, m21*n2 + m22*n2)
--------------------------------------

-- Recursion ------------------------- 

-- Implement the function that calculates
-- the factorial of an integer
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)


-- Implement the function that return the length
-- of a given list
len :: [a] -> Int
len [] = 0
len (h:t) = 1 + len t

len' :: [a] -> Int
len' = undefined

-- Implement the function that removes all
-- the even numbers from a given list
odds' :: [Int] -> [Int]
odds' [] = [] 
odds' (h:t) | mod h 2 /= 0 = h:(odds' t) 
            | otherwise = odds' t 
-- Implement Caesar Cypher with shift=3
-- https://en.wikipedia.org/wiki/Caesar_cipher
-- Suggestion: add "import Data.Char", and use
-- the functions "chr" and "ord"
ecode :: String -> String
ecode [] = []
ecode (h:t) = chr((mod (x + n) 26)) : ecode t
    where
        x = ord (h)
        n = 3

dcode :: String -> String
dcode = undefined

-- Implement the QuickSort algorithm
-- Think how to implement it in your favorite language
-- without using recursion
qSort :: [Int] -> [Int]
qSort = undefined

-- Implement the solution to the Hanoi problem
-- Think how to implement it in your favorite language
-- without using recursion
hanoi :: Int -> a -> a -> a -> [(a,a)]
hanoi = undefined
-- --------------------------------------

-- Datatypes ----------------------------

-- The datatype of leaf trees
data LTree a = Leaf a | Fork (LTree a, LTree a) deriving Show

-- Implement the function that increments all values
-- in a given leaf tree
incr :: LTree Int -> LTree Int
incr = undefined

-- Implement the function that counts the number of leafs
-- in a leaf tree 
count :: LTree Int -> Int
count = undefined

-- The datatype of binary trees
data BTree a = Empty | Node a (BTree a, BTree a) deriving Show

-- Implement the function that increments all values
-- in a given binary tree
bincr :: BTree Int -> BTree Int
bincr = undefined

-- Implement the function that counts the number of leafs
-- in a leaf tree 
bcount :: BTree Int -> Int
bcount = undefined


-- The datatype of "full" trees
data FTree a b = Tip a | Join b (FTree a b, FTree a b) deriving Show

-- Implement the function that sends a full tree into a leaf tree
fTree2LTree :: FTree a b -> LTree a
fTree2LTree = undefined

-- Implement the function that sends a full tree into a binary tree
fTree2BTree :: FTree a b -> BTree b 
fTree2BTree = undefined


-- Implement the semantics of the following very simple 
-- language of Arithmetic Expressions
data Vars = X1 | X2
data Ops = Sum | Mult
type AExp = FTree (Either Vars Int) Ops
type AState = Vars -> Int

semA :: (AExp, AState) -> Int 
semA = undefined

-- The datatype of "rose" trees
data RTree a b = Rtip a | Rjoin b [RTree a b] deriving Show

-- Implement the semantics of the following very simple 
-- language of Boolean Expressions
data BOps = Disj | Conj | Neg
type BExp = RTree (Either Vars Bool) BOps 
type BState = Vars -> Bool

semB :: (BExp, BState) -> Bool
semB = undefined

-- Improve the two previous programming languages by defining
-- data types specific to them
--------------------------------------
