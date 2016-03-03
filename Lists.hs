-- Exploring lists, recursive functions, and pattern matching.
-- The functions in Data.List can be found here:
--    https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.2.0/Data-List.html
module Main (main) where

-- Import the List module.  We use some functions in here that are not provided by default.
import Data.List

main :: IO ()
main = do
  -- Strings are syntactic sugar for lists
  -- and bracketed comma delimited lists are syntactic sugar for : and [] list constructors.
  -- Remember, (:) binds expressions on the right first, i.e. it associates right.

  putStrLn "\nThese 4 expressions all mean the same thing:"
  print $ "abc"
  print $ ['a', 'b', 'c']
  print $ 'a' : ('b' : ('c' : []))
  print $ 'a' : 'b' : 'c' : []

  putStrLn "\nTesting our implementation of head (returns the first element in a list)."
  print $ head  [1, 2, 3]
  print $ head' [1, 2, 3]

  putStrLn "\nTesting our implementation of tail (returns the list minus the first element)."
  print $ tail  [1, 2, 3]
  print $ tail' [1, 2, 3]

  putStrLn "\nTesting our implementation of length (returns the length of a list)."
  print $ length  "abc"
  print $ length' "abc"

  putStrLn "\nTesting our implementation of (++) (appends two lists together)."
  print $ [1, 2, 3] ++  [4, 5, 6]
  print $ [1, 2, 3] ++. [4, 5, 6]

  putStrLn "\nTesting our implementation of last (returns the last element of a list)."
  print $ last  "abc"
  print $ last' "abc"

  putStrLn "\nTesting our implementation of init (returns the this minus the last element)."
  print $ init  "abc"
  print $ init' "abc"

  -- The next exercises to try...

  putStrLn "\nTry implementing your version of null (lookup the definition in the webpage above).  Uncomment the line to test it."
  print $ (null  [], null  "abc")
  --print $ (null' [], null' "abc")

  putStrLn "\nTry implementing your version of intersperse."
  print $ intersperse  ',' "abcdefg"
  --print $ intersperse' ',' "abcdefg"

  putStrLn "\nTry implementing your version of map."
  print $ map  (+ 1) [1, 2, 3, 4]
  --print $ map' (+ 1) [1, 2, 3, 4]

  putStrLn "\nTry implementing your version of reverse."
  print $ reverse  [1, 2, 3, 4, 5, 6]
  --print $ reverse' [1, 2, 3, 4, 5, 6]

  putStrLn "\nTry implementing your version of sum."
  print $ sum  [1, 2, 3]
  --print $ sum' [1, 2, 3]

  putStrLn "\nTry implementing your version of replicate."
  print $ replicate  8 'a'
  --print $ replicate' 8 'a'

  putStrLn "\nTry implementing your version of take."
  print $ take  3 [1 .. 20]
  --print $ take' 3 [1 .. 20]

  putStrLn "\nTry implementing your version of drop."
  print $ drop  3 [1 .. 20]
  --print $ drop' 3 [1 .. 20]

  putStrLn "\nTry implementing your version of elem."
  print $ elem  'f' ['a' .. 'z']
  --print $ elem' 'f' ['a' .. 'z']

  putStrLn "\nTry implementing your version of filter."
  print $ filter  even [1 .. 20]
  --print $ filter' even [1 .. 20]

  putStrLn "\nTry implementing your version of zip."
  print $ zip  [1, 2, 3] [4, 5, 6]
  --print $ zip' [1, 2, 3] [4, 5, 6]

  putStrLn "\nTry implementing your version of unzip."
  print $ unzip  [(1, 2), (3, 4), (5, 6), (7, 8)]
  --print $ unzip' [(1, 2), (3, 4), (5, 6), (7, 8)]




head' :: [a] -> a
head' n = case n of       -- A 'case' expression defines a sequence of patterns. 
  [] -> error "invalid"   -- The first pattern to match indicates the expression that is returned.
  a : _ -> a

tail' :: [a] -> [a]
tail' n = case n of
  [] -> error "invalid"
  _ : b -> b

length' :: [a] -> Int
length' n = case n of
  [] -> 0
  _ : tail -> 1 + length' tail

-- In Haskell, operators are just functions too.
-- We can define new operators like this:
(++.) :: [a] -> [a] -> [a]
(++.) a b = case a of
  [] -> b
  m : n -> m : (n ++. b)

last' :: [a] -> a
last' a = case a of
  [] -> error "invalid"
  [a] -> a
  _ : a -> last' a

init' :: [a] -> [a]
init' a = case a of
  [] -> error "invalid"
  [_] -> []
  a : b -> a : init' b


-- Functions awaiting implementations...

null' :: [a] -> Bool
null' = undefined

intersperse' :: a -> [a] -> [a]
intersperse' = undefined

map' :: (a -> b) -> [a] -> [b]
map' = undefined

reverse' :: [a] -> [a]
reverse' = undefined    -- This probably the most tricky one of the bunch.

sum' :: [Int] -> Int
sum' = undefined

replicate' :: Int -> a -> [a]
replicate' = undefined

take' :: Int -> [a] -> [a]
take' = undefined

drop' :: Int -> [a] -> [a]
drop' = undefined

elem' :: Eq a => a -> [a] -> Bool
elem' = undefined

filter' :: (a -> Bool) -> [a] -> [a]
filter' = undefined

zip' :: [a] -> [b] -> [(a, b)]
zip' = undefined

unzip' :: [(a, b)] -> ([a], [b])
unzip' = undefined


