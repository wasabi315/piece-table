{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
--
-- Module   : Lib
-- Author   : wasabi315 (https://github.com/wasabi315)
--
-- Simple Piece Table implementation.
--
-------------------------------------------------------------------------------

module Lib where

import           Data.Foldable      as F
import           Data.Maybe         ( fromJust )
import qualified Data.Sequence      as S

-------------------------------------------------------------------------------
-- Types

type Position = Int

data FileType
    = Orig
    | Add
  deriving ( Eq, Show )

data Piece = Piece
    { fileType :: !FileType
    , start    :: !Position
    , size     :: !Int
    }

data PieceTable = PieceTable
    { table      :: S.Seq Piece
    , origBuffer :: S.Seq Char
    , addBuffer  :: S.Seq Char
    }

-------------------------------------------------------------------------------
-- Utils

slice :: Position -> Int -> S.Seq a -> S.Seq a
slice start size = S.take size . S.drop start

-------------------------------------------------------------------------------
-- Construction and Deconstrunction

empty :: PieceTable
empty = PieceTable
    { table      = S.empty
    , origBuffer = S.empty
    , addBuffer  = S.empty
    }

fromString :: String -> PieceTable
fromString str = empty
    { table      = S.singleton piece
    , origBuffer = S.fromList str
    }
  where
    len = length str
    piece = Piece
        { fileType = Orig
        , start    = 0
        , size     = len
        }

toString :: PieceTable -> String
toString PieceTable {..} = F.concat . fmap mkString $ table
  where
    mkString :: Piece -> String
    mkString Piece {..} = case fileType of
        Orig -> F.toList $ slice start size origBuffer
        Add  -> F.toList $ slice start size addBuffer

-------------------------------------------------------------------------------
-- Operations

insert :: String -> Position -> PieceTable -> PieceTable
insert str at tbl@PieceTable {..} = undefined

delete :: Position -> Position -> PieceTable -> PieceTable
delete from to tbl@PieceTable {..} = undefined

