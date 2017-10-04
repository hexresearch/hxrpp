{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import System.IO

import Text.Printf

mkDouble :: [Text] -> Double
mkDouble (a:b:_) = (na * 1e9 + nb) / 1e9
  where
    na = read (T.unpack a) :: Double
    nb = read (T.unpack b) :: Double

mkDouble x  = error $ "bad chunk " ++ (show x)

main = do
  ss <- (fmap (mkDouble . T.splitOn "." . head . drop 2 . T.words) . T.lines) <$> IO.hGetContents stdin

  let ss' = zipWith (-) (tail ss) (sort ss)

  let xx = map (\s -> floor ( (1500*8/s) / 1024 / 1024 ) ) ss' :: [Int]

--   forM_ ss' $ \s -> do
--     print $ floor ( (1500*8/s) / 1024 / 1024 )

--   let mm@(minT, maxT) = (minimum ss', maximum ss')

  let grid = (M.fromList $ zip [1,2 .. 1100] (repeat 0)) :: M.Map Int Int

  let xxx = catMaybes $ map (\x -> M.lookupGT x grid >>= \(k,_) -> return (k,1 :: Int) ) xx

  let rs = M.fromListWith (+) xxx

  forM_ (M.toList rs) $ \(k,v) -> do
    printf "%d %d\n" k v

--   mapM_ print (M.toList rs)


