--Using function composition, implement a function isOdious :: Int -> Bool that returns True 
--if the argument has an odd number of 1s in its binary representation and False oth-
--erwise. Use this function to implement the function odious :: non-negative odious numbers.
--  Prelude> take 10 evils
--  [0,3,5,6,9,10,12,15,17,18]
--  Prelude> take 10 odious
--  [1,2,4,7,8,11,13,14,16,19]


toBinary :: Int -> [ Int ] 
toBinary 0 = [ 0 ]        
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ] 


cnt :: Eq a => a -> [a] -> Int 
cnt x [] = 0
cnt x (y:ys)
    | x == y = 1 + (cnt x ys)
    | otherwise = cnt x ys


isOdious :: Int -> Bool
isOdious n = odd(cnt 1 (toBinary n)) 
-- isOdious  returns True if the argument has an odd number of 1s in its binary 
-- representation and False otherwise.


odious:: [Int] 
odious = filter (isOdious) [0..]
-- odious returns the list of non-negative odious numbers.