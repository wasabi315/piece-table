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

import           Data.FingerTree     ( ViewL(..), (|>), (<|) )
import qualified Data.FingerTree     as F
import           Data.Foldable       ( foldl', toList )
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

instance Show Piece where
    show Piece {..}
        =  show fileType ++ " "
        ++ show start ++ " "
        ++ show len

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
    show p@PieceTable {..}
        =  "String : " ++ toString p      ++ "\n"
        ++ "Original : " ++ toList origFile ++ "\n"
        ++ "Add : " ++ toList addFile  ++ "\n"
        ++ "Piece : \n"
        ++ foldMap (\p -> "  " ++ show p ++ "\n") table

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

slice :: Int -> Int -> S.Seq a -> S.Seq a
slice p l = S.take l . S.drop p

toSubstring :: Piece -> S.Seq Char -> S.Seq Char
toSubstring Piece {..} = slice start len

toString :: PieceTable -> String
toString PieceTable {..} = toList $ foldMap mkSubString table
  where
    mkSubString :: Piece -> S.Seq Char
    mkSubString p@Piece {..} = case fileType of
        Orig -> toSubstring p origFile
        Add  -> toSubstring p addFile

-------------------------------------------------------------------------------
-- Operations

splitPiece :: Int -> Piece -> (Piece, Piece)
splitPiece at piece@Piece {..} =
    ( piece { len = at }
    , piece { start = start + at, len = len - at }
    )

splitTable :: Int -> Table -> (Table, Table)
splitTable at table
    | diff == 0 = (ls, rs)
    | F.null rs = (ls, rs)
    | p :< rs' <- F.viewl rs = if diff > len p
        then (ls |> p, rs')
        else let (lp, rp) = splitPiece diff p
             in  (ls |> lp, rp <| rs')
  where
    (ls, rs) = F.split (Sum at <) table
    diff     = at - getSum (F.measure ls)

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
    (_, right)   = splitTable (to - from + 1) rest

