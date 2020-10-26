module Main where

import Lib ( someFunc )

main :: IO ()
main = someFunc

--some basic function & coefficients
square :: Num a => a -> a
square x= x*x

factorial :: (Num a, Enum a) => a -> a
factorial x = product [1..x]

facto :: (Eq p, Num p) => p -> p
facto 0 =1
facto x = x*facto(x-1)

divi :: Fractional a => a -> a -> a
divi x y = x / y


-- define laguerrel polynomials
coef :: Fractional a => a -> a -> a
coef x n = divi  y z
           where y=2*n-x-1
                 z=n

coeff :: Fractional a => a -> a
coeff n = 
    let x= (n-1)  
    in  x/n 

laguerrel :: (Eq a, Fractional a) => a -> a -> a
laguerrel x 0 = 1
laguerrel x 1 =1-x
laguerrel x n =  (coef x n )* laguerrel x (n-1) - (coeff n) * laguerrel x (n-2)

coefl :: Fractional a => a -> a -> a -> a
coefl x m n = divi y z
              where y=2*n-x-1+m
                    z=n
coefll :: Fractional a => a -> a -> a
coefll m n = 
           let x=m+n-1
               y=n
           in x/n

laguerrell :: (Eq a, Fractional a) => a -> a -> a -> a
laguerrell x m 0 = 1
laguerrell x m 1 = m-x+1
laguerrell x m n =  (coefl x m n )* laguerrell x m (n-1) - (coefll m n) * laguerrell x m (n-2)



--define Legendre polynomials

coeflp :: Fractional a => a -> a
coeflp n = divi y n
           where y=(2*n-1)

coeflpp :: Fractional a => a -> a
coeflpp n =
           let y=(n-1)
           in y/n

legpoly :: (Eq a, Fractional a) => a -> a -> a
legpoly x 0 =1
legpoly x 1 =x
legpoly x n = (coeflp n )*x*legpoly x (n-1) -(coeflpp n)*legpoly x (n-2)


--define associated Legendre polynomials
coefaslp :: Floating a => a -> a -> a -> a
coefaslp x m n = let   
                     xs=m+n
                     y=m+n-1
                     z=2*n+1
                     w=(1-x**2)**0.5
                in  xs*y/(z*w)

coefaslpp :: Floating a => a -> a -> a -> a
coefaslpp x m n=let 
                   xs=n-m+2
                   y=n-m+1
                   z=2*n+1
                   w=(1-x**2)**0.5
                in xs*y/(z*w)

sss :: Floating a => a -> a -> a
sss x n = (-1)/((2*n+1)*sqrt(1+square x))

asslegpoly :: (Eq a, Floating a) => a -> a -> a -> a
asslegpoly x m 0 = 1
asslegpoly x 0 1 = x
asslegpoly 1 m n = 0
asslegpoly (-1) m n = 0
asslegpoly x 0 n = (coeflp n )*x*asslegpoly x 0 (n-1) -(coeflpp n)*asslegpoly x 0 (n-2)
asslegpoly x m n = (coefaslp x m n)*(asslegpoly x (m-1) (n-1)) - (coefaslpp x m n) *(asslegpoly x (m-1) (n+1))

assslegpoly :: (Floating p, Eq p) => p -> p -> p -> p
assslegpoly x m n = let 
                     z= (-1)*m
                    in ((-1)**(z))*(asslegpoly x z n) *(facto(n+m)/facto(n-m))


swaslp :: (Ord p, Floating p) => p -> p -> p -> p
swaslp x m n =if m>=0
              then asslegpoly x m n
              else assslegpoly x m n

--Reparameterization in terms of angles
talp :: (Ord a, Floating a) => a -> a -> a -> a
talp x m n = 
                       let z= cos x
                           w=(-1)**m
                       in  w*swaslp z m n
--some factors
ss :: Floating a => a -> a
ss x = (-1)**((x + abs x)/2) 

factor :: (Floating a, Eq a) => a -> a -> a
factor x y =  let 
                       q=2*y+1
                       w=facto(y- x)
                       r=facto(y+ x)
                       in (q*w)/(r*4*pi)

sqf :: (Floating a, Eq a) => a -> a -> a
sqf m  n = sqrt (factor m n)
factormz :: Floating a => a -> a
factormz x =let 
                   q=2*x+1
                   
                   in (q)/(4*pi)

--spheric harmonic function & the integrand, y should be a complex num mathematically but here it's no need to define that data type 
ehm :: (Eq p, Floating p) => p -> p -> p
ehm 0 0 = 1  
ehm y m =  ((-1)**m) * exp (m*y)
                    

sp :: (Floating a, Ord a) => a -> a -> a -> a
sp x m n= (sqf m n) * (talp x m n)


sphm :: (Floating a, Ord a) => a -> a -> a -> a -> a
sphm x y n m = (ehm y m) * (sp x m n)

laglll :: (Eq a, Fractional a) => a -> a -> a -> a
laglll r l n = let
                      x=2*l+1
                      y=n-l-1
                      z=2*r/n
                     in laguerrell z x y

res :: (Eq a, Floating a) => a -> a -> a -> a
res r n l = u*x*y*z*sqrt w
          where x=2**(l+1.5)
                y=exp (-1*r/n)
                z=(r/n)**l
                w=(facto (n-l-1))/facto(l+n)
                u=1/((sqrt pi )*(n**2))

phiz :: (Floating a, Ord a) => a -> a -> a -> a -> a -> a -> a
phiz r x y n l m =(res r n l)*(laglll r l n)* sphm x y l m

yyut :: (Eq a, Floating a) => a -> a -> a -> a
yyut r n l  =(res r n l)*(laglll r l n)
phizero :: (Floating a, Ord a) => a -> a -> a -> a -> a -> a
phizero r x n l m = phiz r x 0 n l m

itgd :: (Floating a, Ord a) => a -> a -> a -> a -> a -> a
itgd r x n l m= 4*pi*square(phizero r x n l m)

--can do a integration, can use a map to difine or use a sum of list; could obtain the density distribution for an electron with given quantum numbers; here is a sum of all electrons
innn :: (Enum a, Fractional a) => a -> [a]
innn x = [(y*x)/1000|y<-[0..999]]

totaleffitem :: (Enum a, Floating a, Ord a) => a -> a -> a -> [a]
totaleffitem r x n = [itgd r x z w u | z<-[1..n], w <-[0..z-1],u<-[-w..w]]
totaleffitemm :: (Enum a, Floating a, Ord a) => a -> a -> [a]
totaleffitemm r n = [(itgd r x z w u)*(pi/1000) | x<-innn pi, z<-[1..n],w <-[0..z-1],u<-[-w..w]]




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

--'sumintt' and 'ratioeledensity' is the ratial possibility of electrons (assume n shells full filled), 'twoshell' is the example for 2 full shell, already in atomic units(r=1 means r is 1 borh radius)
sumint :: (Enum a, Floating a, Ord a) => a -> a -> a -> a
sumint r x n =sum (totaleffitem r x n)
sumintt :: (Enum a, Floating a, Ord a) => a -> a -> a
sumintt r n = sum (totaleffitemm r  n)



ratioeledensity :: (Floating a, Enum a, Ord a) => a -> a -> a
ratioeledensity r n = (r**2) * sumintt r n

twoshell :: (Floating a, Enum a, Ord a) => a -> a
twoshell r = ratioeledensity r 2
