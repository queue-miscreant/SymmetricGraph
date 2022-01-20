{-# LANGUAGE BangPatterns #-}
module SymOut where

import Data.List
import Data.Graph
import Data.Array

import Control.Monad
import System.Environment

import Codec.Picture
import Codec.Picture.Types

import Symmetric
import Algebra
import Graph

heatmapI' arr = (uncurry $ generateImage (curry (arr!))) $ snd $ bounds arr

heatmapI :: (Num a, Integral a) => Array (Int, Int) a -> Image Pixel8
heatmapI arr = heatmapI' $ fmap (floor . (*255) . (/maxEl) . fromIntegral) arr where 
  maxEl = fromIntegral $ maximum $ elems arr

--heatmapGI = heatmapI . toAdjacency

heatmapGI :: Graph -> Image Pixel8
heatmapGI g = generateImage edgeBetween' graphSize graphSize where
  graphSize    = numNodes g
  edgeBetween' = ((\x -> if x then 255 else 0) .) . edgeBetween g

writeStages n = [writePng (show x ++ ".png") $ heatmapGI $ cayleyGraph sym y | (x,y) <- (zip [0..] $ inits (allTwos n))] where
  sym = seq 0 (symmetric n)

knownGraphs = [("k", kG), ("star", starG), ("path", pathG)]

writeEdges = do (graph:order:_) <- getArgs
                let g = maybe (error "Could not find graph") id $ (lookup graph knownGraphs)
                writeFile (graph++" "++order++" edges.txt") $ show $ toEdgeList $ factorialG $ g $ read order

showLevel n = forM_ knownGraphs $ (
  \(x,f) -> writePng (x++" "++show n++" plain.png") $ heatmapGI $ factorialG $ f n
  )

writeAdj = do (fam:order:_) <- getArgs
              let f = maybe (error "Could not find graph") id $ (lookup fam knownGraphs)
              !graph <- return $ factorialG $ f $ read order
              !count <- return $ numNodes graph --irrelevant value to strictly compute the ST result
              putStrLn "Graph in memory"
              !image <- return $ heatmapGI graph
              putStrLn "Image in memory"
              writePng (fam++" "++order++" plain.png") $ image
