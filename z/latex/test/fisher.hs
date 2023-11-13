{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Random (randomRIO)


shuffle :: [Int] -> IO [Int]
shuffle [] = return []
shuffle xs = do
  let m = length xs - 1
  random <- randomRIO (0, m)
  let (left, (chosen:right)) = splitAt random xs
  shuffledRight <- shuffle (left ++ right)
  return (chosen : shuffledRight)



generateShuffledArray :: Int -> IO [Int]
generateShuffledArray n = do
  initialArray <- shuffle [0..n]
  return initialArray

generateArrayOfArrays :: Int -> Int -> IO [[Int]]
generateArrayOfArrays n m = 
  mapM (\_ -> generateShuffledArray m) [0..n]


main :: IO ()
main = do
  let iterations = 50
  let result = generateArrayOfArrays iterations 50
  array <- result
  let jsonData = object ["array" .=  array, "iterations" .= iterations]
      jsonFile = "arrayData.json"
  B.writeFile jsonFile (encode jsonData)


  