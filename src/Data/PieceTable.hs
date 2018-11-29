{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
-------------------------------------------------------------------------------
--
-- Module   : Data.PieceTable
-- Author   : wasabi (https://github.com/wasabi315)
--
-- Piece Table implementation in Haskell.
--
-------------------------------------------------------------------------------

module Data.PieceTable where

import           Control.Arrow
import           Control.Monad         ( (<$!>) )
import qualified Data.ByteString       as BS
import           Data.FingerTree       ( ViewL(..), (|>), (<|), (><) )
import qualified Data.FingerTree       as F
import           Data.Foldable         ( toList )
import           Data.Monoid           ( Sum(..) )
import           Data.String
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.IO          as TIO

-------------------------------------------------------------------------------
-- Types and Instances

-- FileType.
data FileType
    = Orig    -- Original File
    | Add     -- Add File
  deriving ( Eq, Show )

-- Each Piece points to a span in the original or add file.
data Piece = Piece
    { fileType :: !FileType              -- Which file the Piece refers to.
    , start    :: {-# UNPACK #-} !Int    -- Offset into the the file.
    , len      :: {-# UNPACK #-} !Int    -- The length of the piece.
    }
  deriving Show

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
    { table    :: !Table       -- Sequence of the Piece.
    , origFile :: !OrigFile    -- Original file (read only).
    , addFile  :: !AddFile     -- Add file (append only).
    }

instance Show PieceTable where
    show = show . toText

instance IsString PieceTable where
    fromString = fromText . T.pack

-------------------------------------------------------------------------------
-- Constructions and Deconstruction

-- Empty PieceTable.
empty :: PieceTable
empty = PieceTable
    { table    = F.empty
    , origFile = T.empty
    , addFile  = T.empty
    }

-- Create PieceTable from Text.
fromText :: T.Text -> PieceTable
fromText txt = empty
    { table    = F.singleton piece
    , origFile = txt
    }
  where
    piece = Piece
        { fileType = Orig
        , start    = 0
        , len      = T.length txt
        }

-- Yield the substring of the file that the piece refers to.
toSubstring :: Piece -> T.Text -> T.Text
toSubstring p = T.take (len p) . T.drop (start p)

-- Convert PieceTable to Text.
toText :: PieceTable -> T.Text
toText pt = foldMap mkSubstring (table pt)
  where
    mkSubstring :: Piece -> T.Text
    mkSubstring p = case fileType p of
        Orig -> toSubstring p (origFile pt)
        Add  -> toSubstring p (addFile pt)

toString :: PieceTable -> String
toString = T.unpack . toText

readFileWith :: FilePath -> (BS.ByteString -> T.Text) -> IO PieceTable
readFileWith path decoder = fromText . decoder <$> BS.readFile path

readFile :: FilePath -> IO PieceTable
readFile path = readFileWith path TE.decodeUtf8

-------------------------------------------------------------------------------

-- Split Piece at the specified position.
splitPiece :: Int -> Piece -> (Piece, Piece)
splitPiece at p@Piece {..} =
    ( p { len = at }
    , p { start = start + at, len = len - at }
    )

-- Split Table at the specified position.
splitTable :: Int -> Table -> (Table, Table)
splitTable at tbl = case F.search (\l _ -> Sum at < l) tbl of
    F.Position l m r ->
        let d = at - getSum (F.measure l)
        in  (l |>) *** (<| r) $ splitPiece d m
    F.OnLeft  -> (F.empty, tbl)
    F.OnRight -> (tbl, F.empty)
    F.Nowhere -> (tbl, F.empty)

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
insert at txt pt@PieceTable {..} = pt
    { table   = interpose p . splitTable at $ table
    , addFile = addFile `T.append` txt
    }
  where
    p = F.singleton Piece
        { fileType = Add
        , start    = T.length addFile
        , len      = T.length txt
        }

-- See insert.
insert' :: Int -> String -> PieceTable -> PieceTable
insert' at = insert at . T.pack

-- Delete String of the specified range from PieceTable.
delete :: Int -> Int -> PieceTable -> PieceTable
delete from to pt@PieceTable {..}
    = pt { table = leftOf from table >< rightOf to table }

-- Yield the slice of the specified range of the Table.
copy :: Int -> Int -> PieceTable -> Table
copy from to = slice from to . table

-- Paste the Pieces to PieceTable.
paste :: Int -> Table -> PieceTable -> PieceTable
paste at m pt@PieceTable {..}
    = pt { table = interpose m . splitTable at $ table }

-- cut from to = copy from to &&& delete from to
cut :: Int -> Int -> PieceTable -> (Table, PieceTable)
cut from to pt@PieceTable {..} = ( m, pt { table = l >< r } )
  where
    (l, mr) = splitTable from table
    (m, r)  = splitTable (to - from) mr

-------------------------------------------------------------------------------
-- Debugging

-- Show the PieceTable for debugging.
showPieceTable :: PieceTable -> String
showPieceTable p@PieceTable {..} = unlines $
    [ "Text sequence :"
    , indentEach $ T.unpack (toText p)
    , "Original file :"
    , indentEach $ T.unpack origFile
    , "Add file :"
    , indentEach $ T.unpack addFile
    , "PieceTable:"
    , "    fileType start len"
    ] ++
    map (indent . show) (toList table)
  where
    indent = ("    " ++)
    indentEach = unlines . map indent . lines

