-- Examples from today.  You can run this program by doing this from the Terminal:
-- $ runhaskell -W Examples.hs

module Main (main) where
{-
Haskell is a ...
  lazy
  pure
  strongly typed
  functional 
  language.
-}

main :: IO ()
main = do
  -- Print a string to the output.
  putStrLn "Hello Haskell!"

  -- Print the first 20 positive integers ('take' is a function that takes the first n elements of a list).
  print $ take 20 allPositiveIntegers

  -- Example of calling function we defined below.
  print $ add 1 2
  print $ increment 22

  -- An example of passing a function (increment) into another function (map) as an argument.
  -- Map applies the function to every element in the list.
  print $ map increment [1, 2, 3]

  -- Testing problem 1 and problem 2 (solutions below) from Ninety-Nine Haskell Problems.
  print $ myLast    [1, 2, 3, 4]
  print $ myButLast [1, 2, 3, 4]



-- An infinite list is an example of lazy evaluation.
allPositiveIntegers :: [Integer]
allPositiveIntegers = [0 ..]

-- Multiple arguments functions are really just functions that return other functions.  Note the type annotation with the optional parentheses.
add :: Int -> (Int -> Int)
add a b = a + b

-- Using some higher-order function cleverness, we defined increment in terms of add.
-- This works because when you apply add to 1, it returns a function.
increment :: Int -> Int
increment = add 1



-- Problem 1 from Ninety-Nine Haskell Problems.
myLast :: [a] -> a
myLast a = case a of
  []    -> error "Empty list."
  [a]   -> a
  _ : a -> myLast a

-- Problem 2 from Ninety-Nine Haskell Problems.
myButLast :: [a] -> a
myButLast a = case a of
  []     -> error "Empty list."
  [_]    -> error "Single element list."
  [a, _] -> a
  _ : a  -> myButLast a

