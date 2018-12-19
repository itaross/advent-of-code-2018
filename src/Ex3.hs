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
  print . length . filter (>1) . fmap length . group . sort $ listedRectangles

generateListFromRectangle :: Rectangle -> [(Integer, Integer)]
generateListFromRectangle (Rectangle {..}) = do
  xPoint <- [x..x + width - 1]
  yPoint <- [y..y + height - 1]
  return (xPoint, yPoint)