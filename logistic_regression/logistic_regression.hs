
import qualified Data.Array.Repa as R 
import Text.Parsec
import Data.List
import System.Random
import System.IO

import Control.Monad (forM)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import Parsing

type Location = String

getWavFiles :: FilePath -> IO [FilePath]
getWavFiles filepath = do
  names <- getDirectoryContents filepath
  return $ filter (\x -> (not . isSuffixOf ".wav") x) names

--(:.) = R.(:.)
-- type FeatureMatrix = R.Array R.U R.DIM2 Double
-- type FeatureVector = R.Array R.U R.DIM1 Double
-- type ClassVector = R.Array R.U R.DIM1 Double
-- type WeightMatrix = R.Array R.U R.DIM2 Double
-- type DeltaMatrix = R.Array R.U R.DIM2 Double

-- type Instance = (Class, R.Array R.U R.DIM1 Double)

-- data TestSet = TestSet { testing :: [Instance],
--                          training :: [Instance] }

lambda, eta, eta_not :: Double
eta = 0.001
eta_not = 0.001
lambda = 0.001
n = 20.0

classes :: [] Class
classes = [Blues .. Rock]

t :: (Integral a) => a
t = genericLength classes

test_locations :: [(,) Class Location]
test_locations = zip [Blues .. Country] $ map ((++) "./music/") ["blues/", "classical/", "country/"]

class_locations :: [(,) Class Location]
class_locations = zip classes $ map ((++) "./music/") ["blues/", "classical/", "country/",
                                                       "disco/", "hiphop/", "jazz/",
                                                       "metal/", "pop/", "reggae/",
                                                       "rock/"
                                                      ]
 
tenFold :: Int ->  [] Instance -> [] DataSet
tenFold seed all_data = undefined
  where generator = mkStdGen seed

main :: IO ()
main = do
  wav_files <- mapM (\(c, dir) -> getWavFiles dir >>=  \x -> return (c,x)) class_locations :: IO [(Class, [FilePath])]
  let class_files = [(c,f) | (c, fs) <- wav_files, f <- fs] :: [] (Class, FilePath)
  class_contents <- mapM (\(c, file) -> (openFile file ReadMode >>=
                                         hGetContents >>=
                                         \x -> return (c,x))
                         ) class_files
  return ()

--   return ()
