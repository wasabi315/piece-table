module Main where

import           Data.Function    ( (&) )
import           Lib

main :: IO ()
main = mapM_ print $ scanl (&) pt operations
  where
    pt = fromString "Hello, world!"

operations :: [PieceTable -> PieceTable]
operations =
    [ insert 7 "silent "
    , delete 0 5
    , insert 0 "Good night"
    , delete 24 26
    , insert 100 "..."
    ]
