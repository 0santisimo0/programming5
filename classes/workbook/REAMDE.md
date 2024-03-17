## Chapter 3 exercises
What are the types of the following functions?

1. `second`:
   - Type: `[a] -> a`

2. `swap`:
   - Type: `(a, b) -> (b, a)`

3. `pair`:
   - Type: `a -> b -> (a, b)`

4. `double`:
   - Type: `Num a => a -> a`

5. `palindrome`:
   - Type: `Eq a => [a] -> Bool`

6. `twice`:
   - Type: `(a -> a) -> a -> a`




- **Why is it not feasible in general for function types to be instances of the Eq class?**

Functions may have internal state or their behavior can depend on external factors, making it difficult to define equality solely based on their types.

- **When is it feasible?**

It is feasible for functions to be instances of the Eq class if they are pure and always produce the same result for the same arguments, regardless of any external factors.


## Chapter 4 exercises


5. Without using any other library functions or operators, show how the meaning of the following
pattern matching definition for logical conjunction && can be formalised using conditional
expressions:
Hint: use two nested conditional expressions.

-- True && True  = True
-- _ && _        = False


```haskell
nestedConditionals :: Bool -> Bool -> Bool
nestedConditionals conditional1 conditional2 
                    | conditional1 && conditional2 = True
                    | otherwise = False
```

```ruby
True && True = True
True && False = False
False && True = False
False && False = False
```

6. Do the same for the following alternative definition, and note the difference in the number of
conditional expressions that are required:
True && b = b
False && _ = False

```ruby
True && b = b
True && False = False
False && True = False
False && False = False
```

```haskell
exercise2 :: Bool -> Bool -> Bool
exercise2 conditional1 b 
                    | conditional1 && b = b
                    | otherwise = False
```

7. Show how the meaning of the following curried function definition can be formalised in terms of
lambda expressions:
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

```haskell
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z
```


8. The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a
digit, and proceeds as follows:
consider each digit as a separate number;
moving left, double every other number from the second last;
subtract 9 from each number that is now greater than 9;
add all the resulting numbers together;
if the total is divisible by 10, the card number is valid.
Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result isgreater than 9. For example:
> luhnDouble 3
6
> luhnDouble 6
3
Using luhnDouble and the integer remainder function mod, define a function luhn :: Int ->
Int -> Int -> Int -> Bool that decides if a four-digit bank card number is valid. For
example:
> luhn 1 7 8 4
True
> luhn 4 7 8 3
False


> **Solved In  [Code File Chapter4](Chapter4.hs)**

---
---



## Chapter 5 exercises


6. A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
Using a list comprehension and the function factors, define a function perfects :: Int ->
[Int] that returns the list of all perfect numbers up to a given limit. For example:

> perfects 500
> -  [6,28,496]

> **Solved In  [Code File Chapter5](Chapter5.hs)**


7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators
can be re-expressed using two comprehensions with single generators. Hint: nest one
comprehension within the other and make use of the library function concat :: [[a]] -> [a].

> **Solved In  [Code File Chapter5](Chapter5.hs)**


8. Redefine the function positions using the function find.

> **Solved In  [Code File Chapter5](Chapter5.hs)**


9. The scalar product of two lists of integers xs and ys of length n is given by the sum of the products
of corresponding integers:

> **Solved In  [Code File Chapter5](Chapter5.hs)**


## Chapter 6 exercises


5. Using the recursive definitions given in this chapter, show how length
[1,2,3,4,5], and init [1,2,3] are evaluated.

```haskell
= length [1,2,3,4,5]
= 1 + length [2,3,4,5]
1 + (1 + length [3,4,5])
1 + (1 + (1 + length [4,5]))
1 + (1 + (1 + (1 + length [5])))
1 + (1 + (1 + (1 + (1 + length []))))
1 + (1 + (1 + (1 + (1 + 0))))
1 + (1 + (1 + (1 + 1)))
1 + (1 + (1 + 2))
1 + (1 + 3)
1 + 4
= 5

init [1,2,3]
1 : init [2,3]
1 : (2 : init [3])
1 : (2 : [])
1 : [2]
= [1,2]

drop 3 [1,2,3]
= drop 2 [2,3]
= drop 1 [3]
= drop 0 []
= []

```

6.Without looking at the definitions from the standard prelude, define the following library functions
on lists using recursion.
3. Decide if all logical values in a list are True:
a. and :: [Bool] -> Bool

b. Concatenate a list of lists:
concat :: [[a]] -> [a]

c. Produce a list with n identical elements:
replicate :: Int -> a -> [a]

d. Select the nth element of a list:
(!!) :: [a] -> Int -> a

e. Decide if a value is an element of a list:
elem :: Eq a => a -> [a] -> Bool

> **Solved In  [Code File Chapter6](Chapter6.hs)**

7.Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted
lists to give a single sorted list. For example:
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
Note: your definition should not use other functions on sorted lists such as insert or isort, but
should be defined using explicit recursion.

> **Solved In  [Code File Chapter6](Chapter6.hs)**

8. Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in
which the empty list and singleton lists are already sorted, and any other list is sorted by mergingtogether the two lists that result from sorting the two halves of the list separately.
Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose
lengths differ by at most one.

> **Solved In  [Code File Chapter6](Chapter6.hs)**

9.Using the five-step process, construct the library functions that:
a. calculate the sum of a list of numbers;

```haskell
1. sumList :: Num a => [a] -> a
2. sumList :: Num a => [a] -> a
3. sumList [] = 0
4. sumList (x:xs) = x + sumList xs
5. Test
```

b. take a given number of elements from the start of a list;

```haskell
1. takeNum:: Int -> [a] -> [a]
2. takeNum :: Int -> [a] -> [a]
3. takeNum 0 _ = []
- takeNum _ [] = []
4. takeNum n (x:xs) = x : takeFirst (n-1) xs
5. Test
```

c. select the last element of a non-empty list.

```haskell
1. selectLast :: [a] -> a
2. selectLast :: [a] -> a
3. selectLast [x] = x
4. selectLast (_:xs) = selectLast xs
5. Test
```

## Chapter 7 exercises

6. A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list
can be defined as follows:
That is, the function unfold p h t produces the empty list if the predicate p is true of the
argument value, and otherwise produces a non-empty list by applying the function h to this value to
give the head, and the function t to generate another argument that is recursively processed in the
same way to produce the tail of the list. For example, the function int2bin can be rewritten more
compactly using unfold as follows:
int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)
Redefine the functions chop8, map f and iterate f using unfold.

- **chop8**

```haskell
chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)
```

- **map f**

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail
```

- **iterate f**

```haskell
iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f
```


7. Modify the binary string transmitter example to detect simple transmission errors using the concept
of parity bits. That is, each eight-bit binary number produced during encoding is extended with a
parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. In turn,
each resulting nine-bit binary number consumed during decoding is checked to ensure that its parity
bit is correct, with the parity bit being discarded if this is the case, and a parity error being
reported otherwise.
Hint: the library function error :: String -> a displays the given string as an error message
and terminates the program; the polymorphic result type ensures that error can be used in any
context.

```haskell
encodeP :: String -> String
encodeP = concatMap addParityBit
  where
    addParityBit c = c : parityBit c
    parityBit c | odd $ countOnes $ intToBinary $ ord c = "1" | otherwise = "0"
    countOnes = length . filter (== '1')
    intToBinary = reverse . take 8 . (++ repeat '0') . reverse . decimalToBinary
    decimalToBinary 0 = "0"; decimalToBinary 1 = "1"; decimalToBinary n = decimalToBinary (n `div` 2) ++ show (n `mod` 2)

decodeP :: String -> Either String String
decodeP [] = Right []
decodeP (c:cs) | length cs < 8 = Left "Transmission error: insufficient bits"
                        | checkParity c cs = fmap (chr .binaryToInt . init) (Right cs')
                        | otherwise = Left "Transmission error: parity bit mismatch"
  where
    cs' = take 8 cs
    checkParity p bs = p == calculatedParityBit bs
    calculatedParityBit = head . show . (`mod` 2) . length . filter (== '1')
    binaryToInt = foldl (\acc x -> acc * 2 + x) 0
```


8. Test your new string transmitter program from the previous exercise using a faulty communication
channel that forgets the first bit, which can be modelled using the tail function on lists of bits.


adding.-

```haskell
faultyChannel :: String -> String
faultyChannel = tail

```

> **Solved In  [Code File Chapter7](Chapter7.hs)**


9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies
its two argument functions to successive elements in a list, in turn about order. For example:
> altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]


> **Solved In  [Code File Chapter7](Chapter7.hs)**
