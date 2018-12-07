{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Control.Lens          hiding ( (|>), (<|) )
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
    { _fileType :: FileType              -- Which file the Piece refers to.
    , _start    :: {-# UNPACK #-} Int    -- Offset into the the file.
    , _len      :: {-# UNPACK #-} Int    -- The length of the piece.
    }
  deriving Show

makeLenses ''Piece

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
    { _table    :: Table       -- Sequence of the Piece.
    , _origFile :: OrigFile    -- Original file (read only).
    , _addFile  :: AddFile     -- Add file (append only).
    }

makeLenses ''PieceTable

instance Show PieceTable where
    show = T.unpack . toText

instance IsString PieceTable where
    fromString = fromText . T.pack

-------------------------------------------------------------------------------
-- Constructions and Deconstruction

-- Empty PieceTable.
empty :: PieceTable
empty = PieceTable F.empty T.empty T.empty

-- Create PieceTable from Text.
fromText :: T.Text -> PieceTable
fromText t = PieceTable (F.singleton p) t T.empty
  where
    p = Piece Orig 0 (T.length t)

-- Convert PieceTable to Text.
toText :: PieceTable -> T.Text
toText pt = foldMap mkSubstring $ pt ^. table
  where
    mkSubstring :: Piece -> T.Text
    mkSubstring (Piece t s l) = T.take l . T.drop s $ case t of
        Orig -> pt ^. origFile
        Add  -> pt ^. addFile

toString :: PieceTable -> String
toString = T.unpack . toText

readFileWith :: FilePath -> (BS.ByteString -> T.Text) -> IO PieceTable
readFileWith p dec = fromText . dec <$> BS.readFile p

readFile :: FilePath -> IO PieceTable
readFile p = readFileWith p TE.decodeUtf8

-------------------------------------------------------------------------------

(?>) :: Table -> Piece -> Table
t ?> p = if p ^. len == 0 then t else t |> p

(<?) :: Piece -> Table -> Table
p <? t = if p ^. len == 0 then t else p <| t

-- Split Piece at the specified position.
splitPiece :: Int -> Piece -> (Piece, Piece)
splitPiece i (Piece f s l) = ( Piece f s i, Piece f (s + i) (l - i) )

-- Split Table at the specified position.
splitTable :: Int -> Table -> (Table, Table)
splitTable i t = case F.search (const . (Sum i <)) t of
    F.Position l m r ->
        let d = i - getSum (F.measure l)
        in  (l ?>) *** (<? r) $ splitPiece d m
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
insert i t pt
    = pt
    & table %~ interpose p . splitTable i
    & addFile <>~ t
  where
     p = F.singleton Piece
         { _fileType = Add
         , _start    = T.length (pt ^. addFile)
         , _len      = T.length t
         }

-- Delete String of the specified range from PieceTable.
delete :: Int -> Int -> PieceTable -> PieceTable
delete i j pt
    | i < j     = pt & table %~ ((><) <$> leftOf i <*> rightOf j)
    | i == j    = pt
    | otherwise = delete j i pt

-------------------------------------------------------------------------------
-- Debugging

-- Show the PieceTable for debugging.
showPieceTable :: PieceTable -> String
showPieceTable pt = unlines $
    [ "Text sequence :"
    , indentEach $ T.unpack (toText pt)
    , "Original file :"
    , indentEach $ T.unpack (pt ^. origFile)
    , "Add file :"
    , indentEach $ T.unpack (pt ^. addFile)
    , "PieceTable:"
    ] ++
    map (indent . show) (toList (pt ^. table))
  where
    indent = ("    " ++)
    indentEach = unlines . map indent . lines

