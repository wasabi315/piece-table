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
import qualified Data.ByteString       as BS
import           Data.FingerTree       ( ViewL(..), (|>), (<|), (><) )
import qualified Data.FingerTree       as F
import           Data.Foldable         ( toList )
import           Data.Monoid           ( Sum(..) )
import           Data.String
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

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
    show = T.unpack . toText

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
    { table    = F.singleton p
    , origFile = txt
    }
  where
    p = Piece
        { fileType = Orig
        , start    = 0
        , len      = T.length txt
        }

-- Convert PieceTable to Text.
toText :: PieceTable -> T.Text
toText pt = foldMap mkSubstring (table pt)
  where
    mkSubstring :: Piece -> T.Text
    mkSubstring (Piece t s l) = T.take l . T.drop s $ case t of
        Orig -> origFile pt
        Add  -> addFile  pt

toString :: PieceTable -> String
toString = T.unpack . toText

readFileWith :: FilePath -> (BS.ByteString -> T.Text) -> IO PieceTable
readFileWith path decoder = fromText . decoder <$> BS.readFile path

readFile :: FilePath -> IO PieceTable
readFile path = readFileWith path TE.decodeUtf8

-------------------------------------------------------------------------------

(.>) :: Table -> Piece -> Table
t .> p = if len p == 0 then t else t |> p

(<.) :: Piece -> Table -> Table
p <. t = if len p == 0 then t else p <| t

-- Split Piece at the specified position.
splitPiece :: Int -> Piece -> (Piece, Piece)
splitPiece i (Piece f s l) = (Piece f s i, Piece f (s + i) (l - i))

-- Split Table at the specified position.
splitTable :: Int -> Table -> (Table, Table)
splitTable i t = case F.search (const . (Sum i <)) t of
    F.Position l m r ->
        let d = i - getSum (F.measure l)
        in  (l .>) *** (<. r) $ splitPiece d m
    F.OnLeft  -> (F.empty, t)
    F.OnRight -> (t, F.empty)
    F.Nowhere -> (t, F.empty)

interpose :: Table -> (Table, Table) -> Table
interpose m (l, r) = l >< m >< r

leftOf, rightOf :: Int -> Table -> Table
leftOf  i = fst . splitTable i
rightOf i = snd . splitTable i

-------------------------------------------------------------------------------
-- Editting Operations

-- Insert Text at the specified position in the given PieceTable.
insert :: Int -> T.Text -> PieceTable -> PieceTable
insert i t pt@PieceTable {..} = pt
    { table   = interpose p . splitTable i $ table
    , addFile = addFile `T.append` t
    }
  where
    p = F.singleton Piece
        { fileType = Add
        , start    = T.length addFile
        , len      = T.length t
        }

-- See insert.
insert' :: Int -> String -> PieceTable -> PieceTable
insert' i = insert i . T.pack

-- Delete String of the specified range from PieceTable.
delete :: Int -> Int -> PieceTable -> PieceTable
delete i j pt@PieceTable {..}
    | i < j  = pt { table = leftOf i table >< rightOf j table }
    | i == j = pt
    | otherwise  = delete j i pt

-- Yield the slice of the specified range of the Table.
copy :: Int -> Int -> PieceTable -> Table
copy i j pt
    | i < j = leftOf (j - i) . rightOf i $ table pt
    | otherwise = copy j i pt

-- Paste the Pieces to PieceTable.
paste :: Int -> Table -> PieceTable -> PieceTable
paste i m pt@PieceTable {..}
    = pt { table = interpose m . splitTable i $ table }

-- cut from to = copy from to &&& delete from to
cut :: Int -> Int -> PieceTable -> (Table, PieceTable)
cut i j pt@PieceTable {..}
    | i < j  = ( m, pt { table = l >< r } )
    | i == j = ( F.empty, pt )
    | otherwise  = cut j i pt
  where
    (l, mr) = splitTable i table
    (m, r)  = splitTable (j - i) mr

-------------------------------------------------------------------------------
-- Debugging

-- Show the PieceTable for debugging.
showPieceTable :: PieceTable -> String
showPieceTable pt@PieceTable {..} = unlines $
    [ "Text sequence :"
    , indentEach $ T.unpack (toText pt)
    , "Original file :"
    , indentEach $ T.unpack origFile
    , "Add file :"
    , indentEach $ T.unpack addFile
    , "PieceTable:"
    ] ++
    map (indent . show) (toList table)
  where
    indent = ("    " ++)
    indentEach = unlines . map indent . lines

