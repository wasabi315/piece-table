{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List        ( intersperse )
import           Data.Function    ( (&) )

import           Data.PieceTable

main :: IO ()
main = mapM_ printPieceTable $ scanl (&) "Hello, world!" operations

operations :: [PieceTable -> PieceTable]
operations =
    [ insert 7 "silent "
    , delete 0 5
    , insert 0 "Good night"
    , delete 24 26
    , insert 100 "..."
    ]
