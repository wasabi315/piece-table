{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Function ((&))
import Data.Text.PieceTable qualified as PT

main :: IO ()
main = mapM_ PT.printPT $ scanl (&) "Hello, world!" operations

operations :: [PT.PieceTable -> PT.PieceTable]
operations =
  [ PT.insert 7 "silent ",
    PT.delete 0 5,
    PT.insert 0 "Good night",
    PT.delete 26 24,
    PT.insert 100 "..."
  ]
