{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

data Hole = Hole
hole = hole

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1
nat14 = (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))))))
nat7 = (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
(Succ n) -. (Succ m) = n -. m
n -. Zero = n
Zero -. m = Zero
 

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod Zero _ = Pair Zero Zero
natDivMod _ Zero = Pair Zero Zero
natDivMod n m = case natCmp n m of LT -> Pair Zero n
                                   EQ -> Pair (Succ Zero) Zero
                                   GT -> Pair (Succ (natDiv (n -. m) m)) (natMod (n -. m) m)

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd a Zero = a
gcd a b = gcd b (natMod a b)

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero   = Pos Zero   -- 0
intOne    = Pos (Succ Zero)     -- 1
intNegOne = Neg Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = (Pos Zero)
--intNeg Pos x = Neg (x -. 1)
intNeg (Pos (Succ x)) = (Neg x)
intNeg (Neg x) = (Pos (Succ x))

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Pos a) (Pos b) = natCmp a b
intCmp (Pos _) (Neg _) = GT
intCmp (Neg _) (Pos _) = LT
intCmp (Neg a) (Neg b) = natCmp b a

intEq :: Int -> Int -> Bool
intEq (Pos a) (Pos b) = natEq a b
intEq (Neg _) (Pos _) = False
intEq (Pos _) (Neg _) = False
intEq (Neg a) (Neg b) = natEq a b

intLt :: Int -> Int -> Bool
intLt (Pos a) (Pos b) = natLt a b
intLt (Pos _) (Neg _) = False
intLT (Neg _) (Pos _) = True
intLT (Neg a) (Neg b) = natLt b a

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
--n .+. m = undefined
(Pos a) .+. (Pos b) = Pos (a +. b)
(Neg a) .+. (Neg b) = Neg (a +. b +.  natOne)
(Pos a) .+. (Neg b) = case natLt a (b +. natOne) of
	True -> Neg (b -. a)
	False -> Pos (a -. b -. natOne)
--(Neg a) .+. (Pos b) = (Pos b) .+. (Neg a)
a .+. b = b .+. a

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
--n .*. m = undefined
(Pos a) .*. (Pos b) = Pos (a *. b)
(Neg a) .*. (Neg b) = Pos ((a +. natOne) *. (b +. natOne))
(Pos a) .*. (Neg b) = Neg (a *. (b +. natOne))
a .*. b = b .*. a
-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show, Read)

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

ratZero = Rat intZero natOne
ratOne = Rat intOne natOne

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv = undefined

-- Дальше как обычно
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
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b