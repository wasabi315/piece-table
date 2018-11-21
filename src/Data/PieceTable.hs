{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
-------------------------------------------------------------------------------
--
-- Module   : Data.PieceTable
-- Author   : wasabi (https://github.com/wasabi315)
--
-- Simple Piece Table implementation in Haskell.
--
-------------------------------------------------------------------------------

module Data.PieceTable where

import           Control.Lens        hiding ( (|>), (<|), (:<) )
import           Control.Monad       ( (<$!>) )
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
    { _fileType :: !FileType              -- Which file the Piece refers to.
    , _start    :: {-# UNPACK #-} !Int    -- Offset into the the file.
    , _len      :: {-# UNPACK #-} !Int    -- The length of the piece.
    }

makeLenses ''Piece

-- Show instance for Piece.
-- Render the Piece for debugging.
instance Show Piece where
    show Piece {..} = unwords
        [ show _fileType
        , show _start
        , show _len
        ]

-- Measured instance for Piece.
-- Each Piece is measured by its length.
instance F.Measured (Sum Int) Piece where
    measure = Sum . _len

-- Type synonym for the FingerTree of the Piece.
type Table = F.FingerTree (Sum Int) Piece

-- Type synonym for the Text.
type OrigFile = T.Text

-- Type synonym for the Text.
type AddFile = T.Text

-- The PieceTable.
data PieceTable = PieceTable
    { _table    :: !Table       -- Sequence of the Piece.
    , _origFile :: !OrigFile    -- Original file (read only).
    , _addFile  :: !AddFile     -- Add file (append only).
    }

makeLenses ''PieceTable

-- Show instance for PieceTable.
-- Render the PieceTable for debugging.
instance Show PieceTable where
    show p@PieceTable {..} = unlines $
        [ "Text sequence :"
        , indentEach $ toString p
        , "Original file :"
        , indentEach $ T.unpack _origFile
        , "Add file :"
        , indentEach $ T.unpack _addFile
        , "PieceTable:"
        , "  fileType start len"
        ] ++
        map (indent . show) (toList _table)
      where
        indent = ("  " ++)
        indentEach = unlines . map indent . lines

-------------------------------------------------------------------------------
-- Constructions and Deconstruction

-- Empty PieceTable.
empty :: PieceTable
empty = PieceTable
    { _table    = F.empty
    , _origFile = T.empty
    , _addFile  = T.empty
    }

-- Create PieceTable from Text.
fromText :: T.Text -> PieceTable
fromText txt = empty
    { _table    = F.singleton piece
    , _origFile = txt
    }
  where
    piece = Piece
        { _fileType = Orig
        , _start    = 0
        , _len      = T.length txt
        }

-- See fromText.
fromString :: String -> PieceTable
fromString = fromText . T.pack

-- Read a file and return contents as PieceTable.
fromFile :: FilePath -> IO PieceTable
fromFile path = fromText <$!> TIO.readFile path

-- Yield the substring of the file that the piece refers to.
toSubstring :: Piece -> T.Text -> T.Text
toSubstring p = T.take (p ^. len) . T.drop (p ^. start)

-- Convert PieceTable to String.
toString :: PieceTable -> String
toString pt = views table (T.unpack . foldMap mkSubstring) pt
  where
    mkSubstring :: Piece -> T.Text
    mkSubstring p = case p ^. fileType of
        Orig -> toSubstring p (pt ^. origFile)
        Add  -> toSubstring p (pt ^. addFile)

-------------------------------------------------------------------------------

-- Split Piece at the specified position.
splitPiece :: Int -> Piece -> (Piece, Piece)
splitPiece at piece =
    ( set len at piece
    , over start (+ at) . over len (subtract at) $ piece
    )

-- Split Table at the specified position.
splitTable :: Int -> Table -> (Table, Table)
splitTable at table
    | diff == 0 = ps
    | F.null rs = ps
    | p :< rs' <- F.viewl rs =
        let (l, r) = splitPiece diff p in (ls |> l, r <| rs')
  where
    ps@(ls, rs) = F.split (Sum at <) table
    diff        = at - getSum (F.measure ls)

interpose :: Table -> (Table, Table) -> Table
interpose m (l, r) = l >< m >< r

leftOf, rightOf :: Int -> Table -> Table
leftOf  i = fst . splitTable i
rightOf i = snd . splitTable i

-- Yield the slice of the Table.
slice :: Int -> Int -> Table -> Table
slice from to = leftOf (to - from) . rightOf from

-------------------------------------------------------------------------------
-- Editting Operations

-- Insert Text at the specified position in the given PieceTable.
insert :: Int -> T.Text -> PieceTable -> PieceTable
insert at txt tbl
    = tbl
    & table %~ interpose t . splitTable at
    & addFile <>~ txt
  where
    t = F.singleton Piece
        { _fileType = Add
        , _start    = T.length (tbl ^. addFile)
        , _len      = T.length txt
        }

-- See insert.
insert' :: Int -> String -> PieceTable -> PieceTable
insert' at = insert at . T.pack

-- Delete String of the specified range from PieceTable.
delete :: Int -> Int -> PieceTable -> PieceTable
delete from to = table %~ ((><) <$> leftOf from <*> rightOf to)

-- Yield the slice of the specified range of the Table.
copy :: Int -> Int -> PieceTable -> Table
copy from to = views table (slice from to)

-- Paste the Pieces to PieceTable.
paste :: Int -> Table -> PieceTable -> PieceTable
paste at m = table %~ interpose m . splitTable at

-- cut from to = copy from to &&& delete from to
cut :: Int -> Int -> PieceTable -> (Table, PieceTable)
cut from to pt = ( m, set table (l >< r) pt )
  where
    (l, mr) = splitTable from (pt ^. table)
    (m, r)  = splitTable (to - from) mr

