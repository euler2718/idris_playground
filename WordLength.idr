-- allLengths : List String -> List Nat
-- allLengths [] = []
-- allLengths (word :: words) = length word :: allLengths words
import Data.Vect

total allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

insert : Ord elem =>
         (x : elem) -> (xsSorted : Vect len elem) -> Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort : Ord elem =>
          Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

infixl 5 |>
(|>) : a -> (a->b) -> b
a |> f = f a

mutual
    isEven : Nat -> Bool
    isEven Z = True
    isEven (S k) = isOdd k

    isOdd : Nat -> Bool
    isOdd Z = False
    isOdd (S k) = isEven k

myLength : List a -> Nat
myLength [] = 0
myLength (x :: xs) = 1 + myLength xs

-- reverser : (x : a) -> (xs : List a) -> List a
-- reverser x [] = [x]
-- reverser x (y :: xs) = reverser xs ++ [y] ++ [x]

myReverse : List a -> List a
myReverse [] = []
-- myReverse (x :: xs) = ?whatever ++ ?another
-- myReverse ([x]) = x
-- myReverse [] = []
-- myReverse (x :: xs) = myReverse xs ++ [x]

myMap : (a -> b) -> List a -> List b
myMap f [] = []
myMap f (x :: xs) = (f x) :: (myMap f xs)

myVectMap : (a -> b) -> Vect n a -> Vect n b
myVectMap f [] = []
myVectMap f (x :: xs) = f x :: myVectMap f xs

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect len elem)) -> Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
    zipWith (\x, y => x::y) x xsTrans

-- transposeMat [] = createEmpties
-- transposeMat (x :: xs) = let xsTrans = transposeMat xs in
--     zipWith (\x, y => x::y) x xsTrans
-- transposeMat (x :: xs) = let xsTrans = transposeMat xs in
--                          transposeHelper x xsTrans


addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = ?addMatrix_rhs
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = let adder = addMatrix xs ys in
                                (zipWith (\a, b => a + b) x y) :: adder


multer : Num a => (x : Vect m a) ->  (ysTrans : Vect p (Vect m a)) -> Vect p a
multer x ysTrans = map (\c => sum (zipWith (*) x c)) ysTrans

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] [] = [] -- this seems unnecessary but shows the function is not total
multMatrix [] (x :: xs) = []
multMatrix (x :: xs) ys = let ysTrans = transposeMat ys in
                          multer x ysTrans :: multMatrix xs ys
                        --   map (\c => sum (zipWith (*) x)) ysTrans :: multMatrix xs ys
-- multMatrix [] [] = []
-- multMatrix [] ys = []
-- multMatrix (x :: xs) ys = let ysTrans = transposeMat ys in
--                           map (\c => sum (zipWith (*) x)) ysTrans :: multMatrix xs ys

-- uncurry : ( a -> b -> c ) -> ( a, b) -> c

-- curry : ( ( a, b ) -> c ) -> a -> b -> c

-- zipWith : ( a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
-- zipWith f [] [] = []
-- zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys