import Control.Monad
{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}
data Validation a = Fail String | Success a deriving (Show)

-- Make the Validation a Monad
instance Monad Validation where
  return x = Success x
  (Success x) >>= f = f x
  (Fail s) >>= f = Fail s


{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}
positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x 
  | x > 0 = Success x
  | otherwise = Fail "Not positive"

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a)  =>  a -> Validation a
evenCheck x
  | even x = Success x
  | otherwise = Fail "Not even"

{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}
positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
positiveAndEvenCheck x = return x >>= positiveCheck >>= evenCheck
