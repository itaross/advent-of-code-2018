module Ex2 where
    
import Data.List
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe (catMaybes)

part1 :: [String] -> Integer
-- multiply the number of time a letter occurs 2 or 3 times separately in a list of words
part1 xs = (foldr ((+) . count 2) 0 xs) * (foldr ((+) . count 3) 0 xs)
    where
        -- sort the characters in a  string: "abaccbed" -> "aabbcde"
        -- count the occurence of x times by grouping the letter in a word: "aabbcde" -> ["aa", "bb", "c", "d", "e"]
        -- then calculate its length: ["aa", "bb", "c", "d", "e"] -> [2, 2, 1, 1, 1]
        -- take only the lengths == x: x=2 -> [1, 1]
        -- and add 1 or 0 using length
        count x list = counterize . length . filter (==x) . (map length . group . sort) $ list
            where 
                counterize 0 = 0
                counterize x = 1

part2 :: [String] -> Maybe String
part2 (word:others) = case filter (findCommonExceptOne word) others of
                        [] -> part2 others -- iteratively go to next item
                        [item] -> Just . catMaybes $ zipWith getItem word item
                        _ -> Nothing
    where getItem x y   | x == y = Just x
                        | otherwise = Nothing

findCommonExceptOne :: String -> String -> Bool
-- get the number of differnt chars in the string. Keep only the strings that differs by 1
findCommonExceptOne xs ys = (==1) . length . filter not $ zipWith (==) xs ys

main = do
    content <- fmap (T.lines) $ TIO.readFile "ex2.txt"
    print . part1 $ map T.unpack content
    print . part2 $ map T.unpack content