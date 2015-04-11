module Parsing
       (Instance,
        buildInstance,
        buildData,

        Class(
          Blues,
          Classical,
          Country,
          Disco,
          Hiphop,
          Jazz,
          Metal,
          Pop,
          Reggae,
          Rock
        ),

        DataSet,
        testing,
        training,
       )
       where

import qualified Data.Array.Repa as R
import System.Random
import Text.Parsec

data Class = Blues | Classical | Country | Disco |
             Hiphop | Jazz | Metal | Pop | Reggae | Rock
           deriving (Eq, Enum, Show)

type FeatureVector = R.Array R.U R.DIM1 Double
type WeightMatrix = R.Array R.U R.DIM2 Double
type DeltaMatrix = R.Array R.U R.DIM2 Double

type Instance = (Class, FeatureVector)

data DataSet = Data {testing :: [Instance],
                     training :: [Instance] }

buildData :: StdGen -> Int -> [] Instance -> DataSet
buildData gen size ls = Data test train
  where (g1,g2) = split gen
        test = tailcall g1 ls 0 testSize []
        train = tailcall g2 ls 0 trainSize []
        tailcall gen ls n max acc = if n <= max
                                    then let (idx, g') = next gen
                                         in tailcall g' ls (n + 1) max ((ls !! idx) : acc)
                                    else acc
        testSize = (length ls) - size
        trainSize = size
  
buildInstance :: Class -> String -> Maybe Instance
buildInstance c fileContent = parseContent fileContent >>= return . (,) c
--  return (c, inst)

               
parseContent :: String -> Maybe FeatureVector
parseContent string = case parse doubleList "" string of
                   Left err -> Nothing
                   Right ls -> Just . (R.fromListUnboxed (R.Z R.:. (length ls :: Int))) $ ls

--doubleList :: Parsec String [Double]
doubleList = do
  optional . char $ '-'
  
  d <- double
  char ',' >> spaces
  ds <- doubleList
  return (d:ds)

--double :: Parsec String Double
double = do
  front <- many digit
  char '.'
  end <- many digit
  return $ read (front ++ "." ++ end)
  

