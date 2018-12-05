import Data.List
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Read as TR


-- type IReader t a = t -> Either String (a, t)
-- type Reader a = IReader Text a
-- decimal :: Reader Integer

 -- Text -> Either String (Integer, Text)

readInt :: T.Text -> Integer
readInt text = either (const 0) (fst) (TR.signed TR.decimal text)

sumOfElements :: [Integer] -> Integer
sumOfElements [] = 0
sumOfElements (x:xs) = x + sumOfElements xs

sumOfElemsFold :: [Int] -> Int
sumOfElemsFold xs = foldl' (+) 0 xs

answerPart1 = do
    content <- fmap (fmap readInt . T.lines) $ TIO.readFile "ex1.txt"
    let answer = sumOfElements content
    print answer

part2 :: [Integer] -> Maybe Integer
part2 = findDuplicate . scanl (+) 0 . cycle

findDuplicate :: Eq a => [a] -> Maybe a
findDuplicate = go []
    where go seen [] = Nothing
          go seen (x:xs)    | x `elem` seen = Just x
                            | otherwise = go(x:seen) xs

answerPart2 = do
    content <- fmap(fmap readInt . T.lines) $ TIO.readFile "ex1.txt"
    print $ part2 content
