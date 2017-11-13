-- scansExercises.hs
module ScansExercises where

fibs    = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 fibs
fibslt100 = takeWhile (<100) fibs

factorial = scanl (*) 1 [2..]
