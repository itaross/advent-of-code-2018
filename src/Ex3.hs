{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ex3 where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO
import System.Directory

import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Token

data Rectangle = Rectangle {
    uid :: Integer,
    x :: Integer,
    y :: Integer,
    width :: Integer,
    height :: Integer
} deriving (Show)

type Parser = Parsec T.Text ()

parseLineFabric = runParser rectangleParser () "Wrong Input"

rectangleParser :: Parser Rectangle
rectangleParser = do
  _ <- char '#'
  uid <- parseInteger
  _ <- string " @ "
  x <- parseInteger
  _ <- char ','
  y <- parseInteger
  _ <- string ": "
  width <- parseInteger
  _ <- char 'x'
  height <- parseInteger
  return $ Rectangle {..}

parseFabricFile :: [T.Text] -> [Rectangle]
parseFabricFile xs = case traverse parseLineFabric xs of
  Left e -> []
  Right xv -> xv 

parseInteger :: Parser Integer
parseInteger = fmap read $ many1 digit

solEx3 = do
  print =<< getCurrentDirectory
  content <- fmap (T.lines) $ TIO.readFile "src/ex3.txt"
  let rectangles = parseFabricFile content
  let listedRectangles = concatMap generateListFromRectangle rectangles

  -- part 1
  print . length . filter (>1) . fmap length . group . sort . fmap getDupleFrom4ple $ listedRectangles

  -- part 2
  -- sort the 4ples based on the second and third parameter (x, y)
  let sortedTuples = sortOn getDupleFrom4ple $ listedRectangles
  -- find the non overlapping rectangles
  let nonOverlappingPoints = fmap head . filter ((==1) . length) . groupBy (\t u -> (getDupleFrom4ple t) == (getDupleFrom4ple u)) . sortOn getDupleFrom4ple $ listedRectangles
  -- group the 4ples based on uid
  let groupedPoints = groupBy (\t u -> (getFirstFrom4ple t) == (getFirstFrom4ple u)) . sortOn getFirstFrom4ple $ nonOverlappingPoints
  -- get only those that have length = width * height ==> all the non overlapping points equal to the size of the rectangle
  -- that is all (only one) full non-overlapping rectangle
  let findAnswer = getFirstFrom4ple . head . head . filter (\f -> (getFourthFrom4ple (head f)) `equalityInt` (length f) ) $ groupedPoints
  print findAnswer


generateListFromRectangle :: Rectangle -> [(Integer, Integer, Integer, Integer)]
generateListFromRectangle (Rectangle {..}) = do
  xPoint <- [x..x + width - 1]
  yPoint <- [y..y + height - 1]
  return (uid, xPoint, yPoint, width * height)

getDupleFrom4ple :: (Integer, Integer, Integer, Integer) -> (Integer, Integer)
getDupleFrom4ple (_, a, b, _) = (a, b)

getFirstFrom4ple :: (Integer, Integer, Integer, Integer) -> Integer
getFirstFrom4ple (a, _, _, _) = a

getFourthFrom4ple :: (Integer, Integer, Integer, Integer) -> Integer
getFourthFrom4ple (_, _, _, a) = a

equalityInt :: Integer -> Int -> Bool
equalityInt a b = a == (toInteger b)