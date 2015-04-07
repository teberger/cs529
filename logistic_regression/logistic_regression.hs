
import qualified Data.Array.Repa as R 
import Text.Parsec
import Data.List
import System.Random
import System.IO

import System.Directory (getDirectoryContents)

import Parsing

type Location = String

getWavFiles :: FilePath -> IO [FilePath]
getWavFiles filepath = do
  names <- getDirectoryContents filepath
  return $ filter (\x -> (not . isSuffixOf ".wav") x) names

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
test_locations = zip [Blues .. Country] $ map ("./music/" ++) ["blues/", "classical/", "country/"]

class_locations :: [(,) Class Location]
class_locations = zip classes $ map ("./music/" ++) ["blues/", "classical/", "country/",
                                                       "disco/", "hiphop/", "jazz/",
                                                       "metal/", "pop/", "reggae/",
                                                       "rock/"
                                                      ]
 
tenFold :: Int ->  [] Instance -> [] DataSet
tenFold seed all_data = undefined
  where generator = mkStdGen seed

main :: IO ()
main = do
  wav_files <- mapM (\(c, dir) -> getWavFiles dir >>=  \x -> return (c,x)) class_locations 

  let class_files = [(c,f) | (c, fs) <- wav_files, f <- fs]

  class_contents <- mapM (\(c, file) -> (openFile file ReadMode >>=
                                         hGetContents >>=
                                         \x -> return $ buildInstance c x)
                         ) class_files :: IO [Maybe Instance]
  return ()
