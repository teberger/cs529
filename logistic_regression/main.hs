module Main where

import Data.Maybe (catMaybes)
import Data.List

import System.IO (hGetContents, IOMode(ReadMode), openFile)

import Control.Monad.State.Lazy

import System.Directory (getDirectoryContents)

import Parsing
import LogisticRegression

type Location = String

getWavFiles :: FilePath -> IO [FilePath]
getWavFiles filepath = do
  names <- getDirectoryContents filepath
  return $ filter (\x -> (not . isSuffixOf ".wav") x) names

lambda, eta, eta_not :: Double
eta = 0.001
eta_not = 0.001
lambda = 0.001

n :: Int
n = 20

classes :: [] Class
classes = [Blues .. Rock]

t :: Int
t = length classes

test_locations :: [(,) Class Location]
test_locations = zip [Blues .. Country] $ map ("./music/" ++) ["blues/", "classical/", "country/"]

class_locations :: [(,) Class Location]
class_locations = zip classes $ map ("./music/" ++) ["blues/", "classical/", "country/",
                                                       "disco/", "hiphop/", "jazz/",
                                                       "metal/", "pop/", "reggae/",
                                                       "rock/"
                                                      ]

main :: IO ()
main = do
  wav_files <- mapM (\(c, dir) -> getWavFiles dir >>=  \x -> return (c,x)) class_locations 

  let class_files = [(c,f) | (c, fs) <- wav_files, f <- fs]

  class_contents <- mapM (\(c, file) -> (openFile file ReadMode >>=
                                         hGetContents >>=
                                         \x -> return $ buildInstance c x)
                         ) class_files :: IO [Maybe Instance]

  let instances = catMaybes class_contents
      result = evalStateT logisticRegression (initState instances)
  return ()


initState :: [Instance] -> LRState
initState = undefined
