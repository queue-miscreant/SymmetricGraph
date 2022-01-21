{-# LANGUAGE BangPatterns #-}
module Cayley.SymOut where

import Data.List
import Data.Graph
import Data.Array

import Control.Monad
import System.Environment

import Codec.Picture
import Codec.Picture.Types

import Cayley.Graph
import Cayley.Symmetric
import Cayley.GraphAlg

import Cayley.Examples


knownGraphs = [("k", kG), ("star", starG), ("path", pathG)]

--create a heatmap image from a matrix
--use heatmapGI for Graph heatmaps instead
heatmapI' :: Pixel e => Array (Int, Int) e -> Image e
heatmapI' arr = (uncurry $ generateImage (curry (arr!))) $ snd $ bounds arr

heatmapI :: (Num a, Integral a) => Array (Int, Int) a -> Image Pixel8
heatmapI arr = heatmapI' $ fmap (floor . (*255) . (/maxEl) . fromIntegral) arr where 
  maxEl = fromIntegral $ maximum $ elems arr

--create a heatmap image from a graph's adjacency matrix
heatmapGI :: Graph -> Image Pixel8
heatmapGI g = generateImage edgeBetween' graphSize graphSize where
  graphSize    = numNodes g
  edgeBetween' = ((\x -> if x then 255 else 0) .) . edgeBetween g

--for each stage of the generating set, write an adjacency matrix image
writeStages :: Graph -> [IO ()]
writeStages gr = [writePng (show x ++ ".png") $ graphImage y 
  | (x,y) <- (zip [0..] $ inits $ graphSwaps gr)] where
    n = numNodes gr 
    (_, from, sym) = symmetricTable' n
    toAlgebra = toAlgebra' from n
    graphImage = heatmapGI . cayleyTGraph sym . toAlgebra

--for every graph in `knownGraphs`, write its factorial graph of order n
writeFactOrder :: Int -> IO ()
writeFactOrder n = forM_ knownGraphs $ (
  \(x,f) -> writePng (x++" "++show n++" plain.png") $ heatmapGI $ factorialG $ f n
  )

--write edge list to text file
writeEdges :: IO ()
writeEdges = do (graph:order:_) <- getArgs
                let g     = maybe (error "Could not find graph") id $ (lookup graph knownGraphs)
                    edges = toEdgeList $ factorialG $ g $ read order
                writeFile (graph++" "++order++" edges.txt") $ show edges

--write adjacency matrix image as png
writeAdj :: IO ()
writeAdj = do (fam:order:_) <- getArgs
              let f = maybe (error "Could not find graph") id $ (lookup fam knownGraphs)
              !graph <- return $ factorialG $ f $ read order
              !count <- return $ numNodes graph --irrelevant value to strictly compute the ST result
              putStrLn "Graph in memory"
              !image <- return $ heatmapGI graph
              putStrLn "Image in memory"
              writePng (fam++" "++order++" plain.png") $ image
