{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- ��������� ������-���������

-- ������������� �����������
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- ����� ������������� �����������
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- ����������� ���������
undefined = undefined

-- ���� ������� ����������� ��� �����, ��������� �� undefined ��������.
-- ����� ����� ����� ������������ (natEq � natLt --- ������� ���������).

-------------------------------------------
-- ����������� ����

data Hole = Hole
hole = hole

-- ��� � ������������ ���������
data Unit = Unit deriving (Show,Read)

-- ����, ������������
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- �������, ��������������
data Either a b = Left a | Right b deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- ������� ��������, ��� ���������� if � ���� Bool ������������ ������,
-- ���� case ������ ��������.

-- �� ��� ����� ����������� ���� if
if' True a b = a
if' False a b = b

-- ����������. ������������� ���, ������������ ��������� ���������
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- ������ ��������

-- ���������� "��"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- ���������� "�"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- ���������� "���"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- ����������� �����

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1
nat14 = (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))))))
nat7 = (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))

-- ���������� ��� ����������� �����
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n ��������� � m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n ������ m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- �������� ��� ����������� �����
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- ��������� ��� ����������� �����
(-.) :: Nat -> Nat -> Nat
(Succ n) -. (Succ m) = n -. m
n -. Zero = n
Zero -. m = Zero
 

infixl 7 *.
-- ��������� ��� ����������� �����
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- ����� � ������� �� ������� n �� m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod Zero _ = Pair Zero Zero
natDivMod _ Zero = Pair Zero Zero
natDivMod n m = case natCmp n m of LT -> Pair Zero n
                                   EQ -> Pair (Succ Zero) Zero
                                   GT -> Pair (Succ (natDiv (n -. m) m)) (natMod (n -. m) m)

natDiv n = fst . natDivMod n -- �����
natMod n = snd . natDivMod n -- �������

-- ����� GCD ���������� ������� (������ �������� 2 (���������������� �����) + 1 (���) �������)
gcd :: Nat -> Nat -> Nat
gcd a Zero = a
gcd a b = gcd b (natMod a b)

-------------------------------------------
-- ����� �����

-- ���������, ����� ������������� ������� ����� ���� ������������
data Int = UNDEFINED deriving (Show,Read)

intZero   = undefined   -- 0
intOne    = undefined     -- 1
intNegOne = undefined -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg = undefined

-- ������ ����� ��� ��� �����������
intCmp :: Int -> Int -> Tri
intCmp = undefined

intEq :: Int -> Int -> Bool
intEq = undefined

intLt :: Int -> Int -> Bool
intLt = undefined

infixl 6 .+., .-.
-- � ���� ��� ������������ �������� ���� �� ��� �����
(.+.) :: Int -> Int -> Int
n .+. m = undefined

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
n .*. m = undefined

-------------------------------------------
-- ������������ �����

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- � ������������ ��� ���� �������� ��������
ratInv :: Rat -> Rat
ratInv = undefined

-- ������ ��� ������
ratCmp :: Rat -> Rat -> Tri
ratCmp = undefined

ratEq :: Rat -> Rat -> Bool
ratEq = undefined

ratLt :: Rat -> Rat -> Bool
ratLt = undefined

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
n %+ m = undefined

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
n %* m = undefined

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- �������� ��� ���������.
-- ���������� �����, �� ������������ ����� � ����

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- ������������� �����������
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- � ��� ������������� �����������
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b