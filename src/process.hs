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
import Data.Fixed

import Text.Printf



main = do
  ss <- makeDoubles  <$> IO.hGetContents stdin

--   let ss' = filter (< (18000) / 1e6) $ zipWith (-) (tail ss) (sort ss)
  let ss' = zipWith (-) (tail ss) ss

  let xx = map (\s -> realToFrac ( (1500*8/s) / 1000 / 1000 ) ) ss' :: [Double]

  let grid = (M.fromList $ zip ([10,20 .. 1000]) (repeat 0)) :: M.Map Int Int

  let xxx = catMaybes $ map (\x -> M.lookupGT (ceiling x) grid >>= \(k,_) -> return (k,1 :: Int) ) xx

  let rs = M.fromListWith (+) xxx

  forM_ (M.toList rs) $ \(k,v) -> do
    printf "%d %d\n" k v


  where
    makeDoubles = sort . fmap parseLine . filtLines . T.lines

    parseLine :: Text -> Double
    parseLine = mkDouble . T.splitOn "." . head . drop 2 . T.words
      where
        mkDouble :: [Text] -> Double
        mkDouble (a:b:_) = (na * 1e9 + nb) / 1e9
          where
            na = read (T.unpack a) :: Double
            nb = read (T.unpack b) :: Double

        mkDouble x  = error $ "bad chunk " ++ (show x)

    filtLines :: [Text] -> [Text]
    filtLines = filter (not . T.isPrefixOf "#")


