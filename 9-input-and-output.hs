import Data.List
import System.Environment
import System.Random

{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
  
main = do
  args <- getArgs
  if null args
    then return ()
    else do
      if head args == "-n" then putStr . unwords $ tail args else putStrLn $ unwords args 

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery gen = take 6 $ nub $ randomRs (1, 49) gen
