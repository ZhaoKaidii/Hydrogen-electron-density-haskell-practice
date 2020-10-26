{-# LANGUAGE FlexibleContexts #-}
module Main where

import Lib

main :: IO ()
main = someFunc


double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)


octant :: Num a => a -> a
octant x = quadruple(double x) 

coll :: Integral a => a -> a -> a
coll x y = factorial x `div` factorial y

poly :: Num a => a -> a
poly x = let 
    y = x + 1
    in y * y

deunss :: Floating a => a -> a
deunss xs = de(un(ss xs)) 
    where   
        de xs = sin xs * cos(octant xs)
        un xs = poly xs + square xs
        ss xs = tan xs * quadruple xs

fs :: [a] -> a
fs xs=head xs

a :: [Integer]
a = [1,2,3,5,5]

b :: (Char, Integer, Integer, [Integer], Bool -> Bool)
b= ('a',5,8,[5,6],not)


cp :: Ord p => p -> p -> p
cp xs n = if n < xs
    then xs
    else n

sg :: (Ord a, Num a, Num p) => a -> p
sg xs 
    |xs < 0 = -1
    |xs == 0 = 0
    |otherwise = 1

sg1 :: (Ord a, Num a, Num p) => a -> p
sg1 xs=if xs<0
    then -1
    else (if xs==0
         then 0
         else 1)



sg2 :: (Ord a, Num a) => a -> [Char]
sg2 xs=if xs<0
    then "neg"
    else (if xs==0
         then "zero"
         else "pos")         


sg3 :: (Ord a, Num a) => a -> Char
sg3 xs=if xs<0
    then '-'
    else (if xs==0
         then 'z'
         else '+')   


nt :: Bool -> Bool
nt xs=if xs==True 
    then False
    else True


fst :: [a] -> a
fst [x,y] = x

lastElm :: [c] -> c
lastElm xs =(head.reverse) xs

oct :: Num c => [c] -> c
oct xs =  (octant.head) xs

tal :: [a] -> [a]
tal []=[]
tal (x:xs)=xs

add1 :: Num a => a -> a
add1 x =x +1

sum1 :: Num b => [b] -> [b]
sum1 xs= map add1 xs

sum2 :: Num b => [b] -> [b]
sum2 xs= map (\x->x+2) xs

add :: Num a => a -> a -> a
add x y= x+y


pow :: (Num t, Num p, Eq t, Eq p) => p -> t -> p
pow m 0 =1
pow 0 n =0
pow m n = m* pow m (n-1)

data Expr=Val Int | Add Expr Expr
                  | Mult Expr Expr

expr :: Expr
expr=Add (Mult (Val 2) (Val 5)) (Val 6)
eval::Expr->Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mult x y)= eval x * eval y



switch1 x= ((x*x)+x*abs x )/(2*square(x))

switch2 x= if x>=0
           then 1
           else 0


matt :: (Ord t, Num t, Enum t) => t -> [[t]]
matt n =
    let mat n result t = 
            if t >= n*n  then result
            else result ++ [[t+1..t+n]] ++ (mat n [] (t+n))
    in  mat n [] 0


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * (f a + f b + innerSum) / 2
    where 
        h = (b - a) / 10000
        innerPts  = map ((2*) . f . (a+)) (points (10000-1) h) 
        innerSum = sum innerPts

points  :: Double -> Double -> [Double]
points i x 
    | i <= 0 = []
    | otherwise = (i*x) : points (i-1) x

map f []=[]
map f (x:xs)= f x :(map f xs)

data Tree=Leaf Int
        -- |Node Int Tree Int
    
tree::Tree
tree=Node 5 (Leaf 4)
             --(Node 3 (Leaf 2)
                      --(Leaf 1))

occurs::Int->Tree->Bool
occurs m (Leaf n)=m == n
occurs m (Node t1 t2) = m ==n
                    --  ||  occurs t1
                    --  ||  occurs t2

flatten::Tree->[Int]
flatten (Leaf n)=[n]
flatten (Node n t1 t2)=flatten t1
                      -- ++[n]
                      -- ++flatten t2

domain x = [(y*x)/100|y<-[0..99]]

multpi x= x*pi/1000

ttt :: [Double]
ttt = [r*w*x*y| x<-[1..2],y<-[0..x-1],r<-domain 2,w<-[-y..y]]

list = [(r,x,y,w)| x<-[1..2],y<-[0..x-1],r<-domain 2,w<-[-y..y]]


pp x = sin(x)
ppp  = integration pp  0 pi

opop x = [(y*x)/100|y<-[0..99]]
pipi= opop pi
pppp = map sin pipi

ppt=map multpi pppp






