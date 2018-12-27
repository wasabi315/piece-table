{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List        ( intersperse )
import           Data.Function    ( (&) )

import           Data.PieceTable

main :: IO ()
main = mapM_ printPT $ scanl (&) "Hello, world!" operations

operations :: [PieceTable -> PieceTable]
operations =
    [ insert 7 "silent "
    , delete 0 5
    , insert 0 "Good night"
    , delete 26 24
    , insert 100 "..."
    ]
