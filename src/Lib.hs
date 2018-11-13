{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
--
-- Module   : Lib
-- Author   : wasabi (https://github.com/wasabi315)
--
-- Simple Piece Table implementation in Haskell.
--
-------------------------------------------------------------------------------

module Lib where

import           Data.FingerTree     ( ViewL(..) )
import qualified Data.FingerTree     as F
import           Data.Maybe          ( fromJust )
import           Data.Monoid         ( Sum(..) )
import qualified Data.Sequence       as S

-------------------------------------------------------------------------------
-- Types and Instances

-- FileType
data FileType
    = Orig
    | Add
  deriving ( Eq, Show )

-- Piece
data Piece = Piece
    { fileType :: !FileType
    , start    :: !Int
    , len      :: !Int
    }

instance F.Measured (Sum Int) Piece where
    measure = Sum . len

-- PieceTable
data PieceTable = PieceTable
    { table    :: Table
    , origFile :: S.Seq Char
    , addFile  :: S.Seq Char
    }

type Table = F.FingerTree (Sum Int) Piece

instance Show PieceTable where
    show PieceTable {..} = undefined

-------------------------------------------------------------------------------
-- Constructions and Deconstruction

empty :: PieceTable
empty = PieceTable
    { table    = F.empty
    , origFile = S.empty
    , addFile  = S.empty
    }

fromString :: String -> PieceTable
fromString str = empty
    { table    = F.singleton piece
    , origFile = S.fromList str
    }
  where
    piece = Piece
        { fileType = Orig
        , start    = 0
        , len      = length str
        }

toString :: PieceTable -> String
toString PieceTable {..} = undefined

-------------------------------------------------------------------------------
-- Operations

splitPiece :: Int -> Piece -> (Piece, Piece)
splitPiece at piece@Piece {..} =
    ( piece { len = at }
    , piece { start = start + len, len = len - at }
    )

splitTable :: Int -> Table -> (Table, Table)
splitTable at table = undefined

insert :: Int -> String -> PieceTable -> PieceTable
insert at str tbl@PieceTable {..} = tbl
    { table   = left <> F.singleton newPiece <> right
    , addFile = addFile <> S.fromList str
    }
  where
    (left, right) = splitTable at table
    newPiece      = Piece
        { fileType = Add
        , start    = S.length addFile
        , len      = length str
        }

delete :: Int -> Int -> PieceTable -> PieceTable
delete from to tbl@PieceTable {..} = tbl { table = left <> right }
  where
    (left, rest) = splitTable from table
    (_, right)   = splitTable (to - from) rest

