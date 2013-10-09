{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = natZero
length (Cons _ b) = natOne +. length b

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ b = b
(Cons a b) ++ c = Cons a (b ++ c)

-- Список без первого элемента
tail :: List a -> List a
tail Nil = error "empty list"
tail (Cons _ b) = b

-- Список без последнего элемента
init :: List a -> List a
init Nil = error "empty list"
init (Cons _ Nil) = Nil
init (Cons a b) = Cons a (init b)

-- Первый элемент
head :: List a -> a
head Nil = error "empty list"
head (Cons a _) = a

-- Последний элемент
last :: List a -> a
last Nil = error "empty list"
last (Cons a Nil) = a
last (Cons _ b) = last b

-- n первых элементов списка
take :: Nat -> List a -> List a
take _ Nil = Nil
take Zero _ = Nil
take (Succ n) (Cons a b) = Cons a (take n b)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop _ Nil = Nil
drop Zero a = a
drop (Succ n) (Cons _ b) = drop n b

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter p (Cons a b) = case p a of
						True -> Cons a (filter p b)
						False -> filter p b

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter p (Cons a as) = case p a of
						Just b -> Cons b (gfilter p as)
						Nothing -> gfilter p as

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (Cons a b) = case p a of 
							True -> Cons a (takeWhile p b)
							False -> Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p (Cons a b) = case p a of
							False -> (Cons a b)
							True -> dropWhile p b

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p = undefined

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break = undefined

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
(Cons a _) !! Zero = a
(Cons a b) !! Succ n = b !! n
 
-- Список задом на перёд
reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons a b) = reverse b ++ (Cons a Nil)

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = bv ++ map (Cons x) bv where bv = subsequences xs

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ z Nil = z
foldl f z (Cons a b) = foldl f (f z a) b

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl _ z Nil = Cons z Nil
scanl f z (Cons a b) = Cons z (scanl f (f z a) b)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ z Nil = z
foldr f z (Cons a b) = f a (foldr f z b)

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr _ z Nil = Cons z Nil
scanr f z (Cons a b) = Cons (f a c) cs where (Cons c cs) = scanr f z b

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons a b) = Cons (f a) (map f b)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons a b) = a ++ concat b

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap _ Nil = Nil
concatMap f (Cons a b) = (f a) ++ (concatMap f b)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons a b) (Cons c d) = Cons (Pair a c) (zip b d)

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (Cons a b) (Cons c d) = Cons (f a c) (zipWith f b d)