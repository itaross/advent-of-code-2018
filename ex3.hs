import qualified Data.Text as T
import Data.List

data Rectangle = Rectangle {
    x :: Int,
    y :: Int,
    width :: Int,
    height :: Int
} deriving (Show)

adaptParse :: String -> [T.Text]
adaptParse = parseRow . T.pack

parseRow :: T.Text -> [T.Text]
parseRow row = list
    where 
        list = T.splitOn (T.pack " ") row
        rect = Rectangle (map read list)
        