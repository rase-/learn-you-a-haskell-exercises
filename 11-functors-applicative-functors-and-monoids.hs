import Control.Applicative
import Data.Monoid

-- Just my own stuff to remember functors
data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq, Ord)

instance Functor MyMaybe where
  fmap f MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
  fmap f Empty = Empty
  fmap f (Value x xs) = (Value (f x) (fmap f xs))

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty b = b
combineLists (Value x xs) b = (Value x (combineLists xs b))

-- Make our list a Monoid
instance Monoid (List a) where
  mempty = Empty
  mappend xs ys = combineLists xs ys

-- Make our list an Applicative
instance Applicative List where
  pure x = Value x Empty
  Empty <*> xs = Empty
  (Value f fs) <*> xs = mappend (fmap f xs) (fs <*> xs)

-- Make sure that the List obeys the laws for Applicative and Monoid
-- ofc it does!

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty
threeValueList = Value 1  . Value 2 $ Value 3 Empty
fourValueList = Value 2 . Value 3 . Value 4 $ Value 5 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
twoPlus = (+2) <$> twoValueList
threePlus = (+2) <$> threeValueList
fourPlus = (+2) <$> fourValueList

-- Use <$> and <*> on the lists with a binary function
squareTwo = (*) <$> twoValueList <*> twoValueList
squareThree = (*) <$> threeValueList <*> threeValueList
squareFour = (*) <$> fourValueList <*> fourValueList

-- Create some lists of binary functions
multiplyTwo = Value (*) $ Value (*) Empty
multiplyThree = Value (*) . Value (*) $ Value (*) Empty
multiplyFour = Value (*) . Value (*) . Value (*) $ Value (*) Empty

addOne = Value (+) Empty
addTwo = Value (+) $ Value (+) Empty

addAndMultiply = Value (+) $ Value (*) Empty

-- Use <*> on the binary functions list and the number lists
derp = addOne <*> twoValueList <*> twoValueList
herp = addAndMultiply <*> twoValueList <*> threeValueList
