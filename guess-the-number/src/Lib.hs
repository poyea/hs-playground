module Lib
    ( checkNumber
    ) where
    
checkNumber :: Int -> Int -> String
checkNumber theNumber target
    | theNumber < target = "The number is smaller than the target."
    | theNumber > target = "The number is greater than the target."
    | otherwise          = "You're correct!"
