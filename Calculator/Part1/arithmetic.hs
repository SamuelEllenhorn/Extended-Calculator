
-- A Virtual Machine (VM) for Arithmetic (specification)

-----------------------
-- Data types of the VM
-----------------------

-- Natural numbers
data NN = O | S NN
  deriving (Eq,Show) -- for equality and printing

-- Integers
data II = II NN NN
  deriving (Eq,Show) -- for equality and printing

-- Positive integers (to avoid dividing by 0)
data PP = I | T PP
    deriving (Eq,Show)

-- Rational numbers
data QQ =  QQ II PP


--maybeMonad






------------------------
-- Arithmetic on the  VM
------------------------

----------------
-- NN Arithmetic
----------------

-- add natural numbers
addN :: NN -> NN -> NN
addN O m = m
addN (S n) m = S (addN n m)

-- multiply natural numbers
multN :: NN -> NN -> NN
multN O m = O
multN (S O) n = n
multN (S n) m = addN (multN n m) m            

-- subtract natural numbers
subtr :: NN -> NN -> NN
subtr O n = n
subtr n O = n
subtr (S n) (S m) = subtr n m  -----


------------------------------
--added Functions:
------------------------------

-- less than: n<m
less :: NN -> NN -> Bool
less O (S n) = True
less _ O = False
less (S n) (S m) = less n m


-- less or equal
lq :: NN -> NN -> Bool
lq n m = less n m || n == m 


-- The remainder:
-- Note: p>q in order for it to work as expected.
remN :: NN -> NN -> NN
remN p q = subtr p (multN q r) where
  r = divN p q


-- greatest common divisor
-- a `gcd` c = c `gcd` r where r = a `mod` c
gcdN :: NN -> NN -> NN
gcdN O n = n
gcdN n O = n
gcdN b c = gcdN c r where
  r = remN b c


-- original nat div

divN :: NN -> NN -> NN
divN _ O = error "Division by Zero"
divN O _ = O
divN p q
  | p `less` q = O 
  | otherwise = addN (S O) ((subtr p q) `divN` q)


--safe nat division <------------------------added funcitonality 




safediv :: NN -> NN -> Maybe NN
safediv _ O = Nothing
safediv O _ = Just O
safediv p q
  | p `less` q = Just O
  | otherwise = Just (addN (S O) a) where 
    Just a = ((subtr p q) `safediv` q)

      



----------------
-- II Arithmetic
----------------

-- Addition: (a-b)+(c-d)=(a+c)-(b+d)
addI :: II -> II -> II
addI (II a b) (II c d) = II ( a `addN` c)  (b `addN` d)

-- Multiplication: (a-b)*(c-d)=(ac+bd)-(ad+bc) 
multI :: II -> II -> II
multI (II a b) (II c d) = (II ((multN a c) `addN` (multN b d)) ((multN a d) `addN` (multN b c)))


-- Subtraction: (a-b)-(c-d)=(a+d)-(b+c)
subtrI :: II -> II -> II
subtrI (II a b) (II c d) = (II (a `addN` d) (b `addN` c))

-- Negation: -(a-b)=(b-a)
negI :: II -> II
negI (II a b) = (II b a)

----------------
-- QQ Arithmetic
----------------

-- add positive numbers
addP :: PP -> PP -> PP
addP I m = T m
addP (T n) m = addP n (T m)

-- multiply positive numbers
multP :: PP -> PP -> PP    
multP I n = n   
multP (T n) m = m `addP` multP n m 


-- convert numbers of type PP to numbers of type II
ii_pp :: PP -> II
ii_pp I = II (S O) O -- convertet int to postive int 1.
ii_pp (T n) = addI (ii_pp n) (II (S O) O)
{-
PP is an inductive type
 that can eihter be 1 I, or the succseror of 1 T
-}




{-

added helper function
-}

nn_ii :: NN -> II
nn_ii O = II O O
nn_ii (S n) = II (S(n)) O


-- Addition: (a/b)+(c/d)=(ad+bc)/(bd)
addQ :: QQ -> QQ -> QQ
addQ (QQ a b) (QQ c d) = QQ (multI a (ii_pp d) `addI` ((multI (ii_pp b) c))) (multP b d) 





-- Multiplication: (a/b)*(c/d)=(ac)/(bd)
multQ :: QQ -> QQ -> QQ
multQ (QQ a b) (QQ c d) = QQ (multI a c) (multP b d)




----------------
-- Normalisation
----------------

{-

helper:
-}


grtr :: NN -> NN -> Bool
grtr a O = True
grtr O a = False 
grtr (S(a)) (S(b)) = grtr a b



normalizeI :: II -> II
normalizeI (II a b)      
    | grtr a b = II (subtr a b) O
    | otherwise = II O (subtr b a)
  


----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------

-- Precondition: Inputs are non-negative
nn_int :: Integer -> NN
nn_int 0 = O
nn_int x = S(nn_int(x-1))




int_nn :: NN->Integer
int_nn O = 0
int_nn (S x) = 1 + (int_nn x) 




ii_int :: Integer -> II
ii_int a
    | a>0 = nn_ii (nn_int a)
    |otherwise = (II O (nn_int(-1*a)))


int_ii :: II -> Integer
int_ii (II a O) = int_nn a
int_ii (II O b) = (-1) * (int_nn b)
int_ii x = int_ii (normalizeI x)




-- Precondition: Inputs are positive
pp_int :: Integer -> PP
pp_int 1 = I
pp_int x = T(pp_int(x-1))




int_pp :: PP->Integer
int_pp I = 1
int_pp (T(x)) = 1 + (int_pp(x))






float_qq :: QQ -> Float
float_qq (QQ a b) = (fromIntegral(int_ii a)/fromIntegral(int_pp b))


------------------------------
-- Normalisation by Evaluation
------------------------------

nbv :: II -> II
nbv a = ii_int(int_ii(a))
----------
-- Testing
----------


----------------------------
--Extending the Calc
----------------------------


--helper functioned needed for frac
-- Convert from PN to NN
p2n :: PP -> NN
p2n I = S O
p2n (T n) = S O `addN` p2n n--add the amount of ones\


n2p :: NN -> PP
n2p O = error "0 cannot be coarced to PN"
n2p (S O) = I     
n2p (S n) = n2p n `addP` I ---if called on the succser of a number add 1
--------------------------------------------

type Frac = (NN,PP)

mulF :: Frac -> Frac -> Frac
mulF (a,b) (c,d) = f where --f will be defined in following lines
  num = a `multN` c  --multiply the numerator
  den = b `multP` d  --mult the denom
  f = simplifyF (num,den)
  
-- addFractions:
addF :: Frac -> Frac -> Frac
addF (a,b) (c,d) = f where
  num =  (multN a (p2n d)) `addN` (multN c (p2n b))
  den = b `multP` d
  f = simplifyF (num,den)


--divFractions:
divF :: Frac -> Frac -> Frac
divF (a,b) (c,d) = f where --f will be defined in following lines
  fracOne = (a,b)
  fracTwo = (p2n(d), n2p(c))--if C is zero we will end up with an invalid ans
  frac = (fracOne `mulF` fracTwo)
  f = simplifyF(frac)


--safeDivFrac:


safedivF :: Frac -> Frac -> Maybe Frac
safedivF (a,b) (O,d)  = Nothing--f will be defined in following lines
safedivF (a,b) (c,d) = f where
  fracOne = (a,b)
  fracTwo = (p2n(d),n2p(c))--if C is zero we will end up with an invalid ans
  f = Just(simplifyF(fracOne `mulF` fracTwo))
  




-- Fraction equality
equalF :: Frac -> Frac -> Bool
equalF (a,b) (c,d) = a `multN` (p2n d) == c `multN` (p2n b)

-- Simplify Fractions
simplifyF :: Frac -> Frac
simplifyF (a,b) = (p,q) where
  --find gcd of num, then devide by that
  gcd =  a `gcdN` (p2n b)
  p = a `divN` gcd
  q = n2p $ (p2n b) `divN` gcd 





--------------------
--bools
--------------------

data Boole = Tr | F
  deriving (Eq,Show) -- 
  

--------------------
--bool addition (or)
--------------------
addBoole :: Boole -> Boole -> Boole
addBoole Tr F = Tr
addBoole F F = F

addBoole Tr Tr = Tr

--------------------
--bool mult (and)
--------------------
multBoole :: Boole -> Boole -> Boole

multBoole F F = F
multBoole Tr F = F
multBoole Tr Tr = Tr


--------------------
--bool (not)
--------------------
notBoole :: Boole -> Boole
notBoole F = Tr
notBoole Tr = F

--------------------
--end bool
--------------------



--------------
--test
--------------

nn2int :: NN -> Int
nn2int O = 0
nn2int (S n) = 1 + nn2int n

int2nn :: Int -> NN
int2nn 0 = O
int2nn n = (S O) `addN` int2nn (n-1)

int2pn :: Int -> PP
int2pn 0 = error "0 cannot be coarced into PN"
int2pn 1 = I
int2pn n = I `addP` int2pn (n-1)

ints2frac :: (Int,Int) -> Frac
ints2frac (n,p) = (int2nn n, int2pn p)

frac2int :: Frac -> (Int,Int)
frac2int (n,p) = (nn2int n, nn2int(p2n p))


main = do
    -- Integers: (II i j) represents i-j, (II k l) represents k-l
    let i = 4
    let j = 2
    let k = 1
    let l = 3

    print $ int_ii (addI (II (nn_int i) (nn_int j)) (II (nn_int k) (nn_int l)))
    print $ int_ii (multI (II (nn_int i) (nn_int j)) (II (nn_int k) (nn_int l)))
    -- Fractions: (QQ i j) represents i/j, (QQ k l) represents k/l
    print $ float_qq (addQ (QQ (ii_int i) (pp_int j)) (QQ (ii_int k) (pp_int l)))
    print $ float_qq (multQ (QQ (ii_int i) (pp_int j)) (QQ (ii_int k) (pp_int l)))
    -- Normalisation (recursive definition)
    print $ normalizeI (II (nn_int i) (nn_int j))
    -- Normalisation (by evaluation)
    print $ nbv (II (nn_int i) (nn_int j))
    print $ multN (S(S (S O))) (S O) 

    print $ "safeDivTest:"
    print $ safediv (O)(S O)
    print $ safediv (S O)(S O)
    print $ safediv (S O)(O)

      



    print $ "int2nn test"
    print $ int2nn(4)


    print $ "divFrac"
    print $ divF (int2nn(9),int2pn(3)) (int2nn(9),int2pn(3)) 
    print $ divF (int2nn(18),int2pn(1)) (int2nn(9),int2pn(1)) 

    print $ "safedivF"
    print $ safedivF (int2nn(9),int2pn(3)) (int2nn(9),int2pn(3)) 
    print $ safedivF (int2nn(18),int2pn(1)) (int2nn(9),int2pn(1)) 
    print $ safedivF (int2nn(18),int2pn(1)) (int2nn(0),int2pn(1)) 
 
    
  


test1 :: Bool
test1 =  frac2int(addF (ints2frac (2,3) ) (ints2frac (6,8)) ) == (17,12)

test2 :: Bool
test2 = equalF (ints2frac (2,6)) (ints2frac (1,3)) == True

test3 :: Bool
test3 = addF (ints2frac (36,60)) (ints2frac (24,45)) == ints2frac (17,15)

test4 :: Bool
test4 = frac2int( simplifyF (addF (ints2frac (36,60)) (ints2frac (24,45)) )) == (17,15)



