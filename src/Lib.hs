{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
-------------------------------------------------------------------------------
--
-- Module   : Lib
-- Author   : wasabi (https://github.com/wasabi315)
--
-- Simple Piece Table implementation in Haskell.
--
-------------------------------------------------------------------------------

module Lib where

import           Data.FingerTree     ( ViewL(..), (|>), (<|), (><) )
import qualified Data.FingerTree     as F
import           Data.Foldable       ( toList )
import           Data.Monoid         ( Sum(..) )
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

-------------------------------------------------------------------------------
-- Types and Instances

-- FileType.
data FileType
    = Orig    -- Original File
    | Add     -- Add File
  deriving ( Eq, Show )

-- Each Piece points to a span in the original or add file.
data Piece = Piece
    { fileType :: FileType    -- Which file the Piece refers to.
    , start    :: Int         -- Offset into the the file.
    , len      :: Int         -- The length of the piece.
    }

-- Show instance for Piece.
-- Render the Piece for debugging.
instance Show Piece where
    show Piece {..} = unwords
        [ show fileType
        , show start
        , show len
        ]

-- Measured instance for Piece.
-- Each Piece is measured by its length.
instance F.Measured (Sum Int) Piece where
    measure = Sum . len

-- Type synonym for the FingerTree of the Piece.
type Table = F.FingerTree (Sum Int) Piece

-- Type synonym for the Text.
type OrigFile = T.Text

-- Type synonym for the Text.
type AddFile = T.Text

-- The PieceTable.
data PieceTable = PieceTable
    { table    :: Table       -- Sequence of the Piece.
    , origFile :: OrigFile    -- Original file (read only).
    , addFile  :: AddFile     -- Add file (append only).
    }

-- Show instance for PieceTable.
-- Render the PieceTable for debugging.
instance Show PieceTable where
    show p@PieceTable {..} = unlines $
        [ "Text sequence :"
        , indentEach $ toString p
        , "Original file :"
        , indentEach $ T.unpack origFile
        , "Add file :"
        , indentEach $ T.unpack addFile
        , "PieceTable:"
        , "  fileType start len"
        ] ++
        map (indent . show) (toList table)
      where
        indent = ("  " ++)
        indentEach = unlines . map indent . lines

-------------------------------------------------------------------------------
-- Constructions and Deconstruction

-- Empty PieceTable.
empty :: PieceTable
empty = PieceTable
    { table    = F.empty
    , origFile = T.empty
    , addFile  = T.empty
    }

-- Create PieceTable from String.
fromString :: String -> PieceTable
fromString str = empty
    { table    = F.singleton piece
    , origFile = T.pack str
    }
  where
    piece = Piece
        { fileType = Orig
        , start    = 0
        , len      = length str
        }

-- Read a file and return contents as PieceTable.
fromFile :: FilePath -> IO PieceTable
fromFile path = do
    txt <- TIO.readFile path
    let piece = Piece
            { fileType = Orig
            , start    = 0
            , len      = T.length txt
            }
    return $! empty
        { table    = F.singleton piece
        , origFile = txt
        }

-- Yield the substring of the file that the piece refers to.
toSubstring :: Piece -> T.Text -> T.Text
toSubstring Piece {..}= T.take len . T.drop start

-- Convert PieceTable to String.
toString :: PieceTable -> String
toString PieceTable {..} = T.unpack $ foldMap mkSubString table
  where
    mkSubString :: Piece -> T.Text
    mkSubString p@Piece {..} = case fileType of
        Orig -> toSubstring p origFile
        Add  -> toSubstring p addFile

-------------------------------------------------------------------------------
-- Operations

-- Split Piece at the specified position.
splitPiece :: Int -> Piece -> (Piece, Piece)
splitPiece at piece@Piece {..} =
    ( piece { len = at }
    , piece { start = start + at, len = len - at }
    )

-- Split Table at the specified position.
-- TODO : fix ugly code.
splitTable :: Int -> Table -> (Table, Table)
splitTable at table
    | diff == 0 = ps
    | F.null rs = ps
    | p :< rs' <- F.viewl rs = if diff > len p
        then (ls |> p, rs')
        else let (lp, rp) = splitPiece diff p in  (ls |> lp, rp <| rs')
  where
    ps@(ls, rs) = F.split (Sum at <) table
    diff        = at - getSum (F.measure ls)

-- Insert a String at the specified position in the given PieceTable.
insert :: Int -> String -> PieceTable -> PieceTable
insert at str tbl@PieceTable {..} = tbl
    { table   = left >< F.singleton newPiece >< right
    , addFile = addFile `T.append` T.pack str
    }
  where
    (left, right) = splitTable at table
    newPiece      = Piece
        { fileType = Add
        , start    = T.length addFile
        , len      = length str
        }

-- Delete String of the specified range from PieceTable.
delete :: Int -> Int -> PieceTable -> PieceTable
delete from to tbl@PieceTable {..} = tbl { table = left >< right }
  where
    (left, rest) = splitTable from table
    (_, right)   = splitTable (to - from + 1) rest

