{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Text.PieceTable
  ( PieceTable,
    empty,
    index,
    fromLazyText,
    toLazyText,
    toString,
    length,
    insert,
    delete,
    printPT,
  )
where

import Data.Coerce (coerce)
import Data.FingerTree qualified as F
import Data.Int (Int64)
import Data.Monoid (Sum (..))
import Data.String (IsString (..))
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Prelude hiding (length)

-------------------------------------------------------------------------------

data BufferType = File | Add deriving (Eq, Show)

-- A piece descripter points to a span in the file buffer or add buffer
data Piece = Piece
  { bufferType :: !BufferType, -- Which buffer
    start :: {-# UNPACK #-} !Int64, -- The offset in the buffer
    length :: {-# UNPACK #-} !Int64 -- The length of the piece
  }
  deriving (Show)

instance F.Measured (Sum Int64) Piece where
  measure p = coerce p.length
  {-# INLINE measure #-}

type Table = F.FingerTree (Sum Int64) Piece

data PieceTable = PieceTable
  { table :: Table,
    fileBuffer :: TL.Text, -- read only
    addBuffer :: TL.Text, -- append only
    addBufferLength :: {-# UNPACK #-} !Int64
  }

instance Show PieceTable where
  show = TL.unpack . toLazyText

instance IsString PieceTable where
  fromString = fromLazyText . TL.pack

-------------------------------------------------------------------------------

empty :: PieceTable
empty = PieceTable F.empty TL.empty TL.empty 0

fromLazyText :: TL.Text -> PieceTable
fromLazyText t = PieceTable (F.singleton p) t TL.empty 0
  where
    p = Piece File 0 (TL.length t)

toLazyText :: PieceTable -> TL.Text
toLazyText pt = foldMap (piece pt) pt.table

toString :: PieceTable -> String
toString = TL.unpack . toLazyText

-------------------------------------------------------------------------------

length :: PieceTable -> Int64
length p = coerce $ F.measure p.table
{-# INLINE length #-}

index :: PieceTable -> Int64 -> Maybe Char
index pt i =
  case F.search (\v _ -> i < coerce v) pt.table of
    F.Position _ (Piece t s _) _ -> Just $! flip TL.index (s - i) $ case t of
      File -> pt.fileBuffer
      Add -> pt.addBuffer
    _ -> Nothing

-------------------------------------------------------------------------------

cons :: Piece -> Table -> Table
cons p t =
  case p.length of
    0 -> t
    _ -> p F.<| t
{-# INLINE cons #-}

snoc :: Table -> Piece -> Table
snoc t p =
  case p.length of
    0 -> t
    _ -> t F.|> p
{-# INLINE snoc #-}

-- Get the span pointed by a given `Piece`
piece :: PieceTable -> Piece -> TL.Text
piece pt p =
  TL.take p.length . TL.drop p.start $ case p.bufferType of
    File -> pt.fileBuffer
    Add -> pt.addBuffer

-------------------------------------------------------------------------------

-- Split a `Piece` at a specified position
splitPiece :: Int64 -> Piece -> (Piece, Piece)
splitPiece i (Piece f s l) = (Piece f s i, Piece f (s + i) (l - i))
{-# INLINE splitPiece #-}

-- Split a `Table` at a specified position
splitTable :: Int64 -> Table -> (Table, Table)
splitTable i t = case F.search (\v _ -> i < coerce v) t of
  F.OnLeft -> (F.empty, t)
  F.OnRight -> (t, F.empty)
  F.Nowhere -> (t, F.empty)
  F.Position l p r -> (snoc l pl, cons pr r)
    where
      !d = i - coerce (F.measure l)
      !(!pl, !pr) = splitPiece d p

leftOf, rightOf :: Int64 -> Table -> Table
leftOf i = fst . splitTable i
rightOf i = snd . splitTable i

-------------------------------------------------------------------------------

-- Insert a `Text` at a specified position.
insert :: Int64 -> TL.Text -> PieceTable -> PieceTable
insert i t pt =
  pt
    { table = case splitTable i pt.table of
        (!l, !r) -> l <> cons p r,
      addBuffer = pt.addBuffer <> t,
      addBufferLength = pt.addBufferLength + len
    }
  where
    len = TL.length t
    p = Piece Add pt.addBufferLength len

-- Delete the substring of a specified range.
delete :: Int64 -> Int64 -> PieceTable -> PieceTable
delete i j pt =
  case compare i j of
    EQ -> pt
    LT -> pt {table = leftOf i pt.table <> rightOf j pt.table}
    GT -> pt {table = leftOf j pt.table <> rightOf i pt.table}

-------------------------------------------------------------------------------
-- Debugging

-- Print PieceTable for debugging.
printPT :: PieceTable -> IO ()
printPT pt = do
  putStrLn "----------------------------------------\n"
  putStrLn "Text sequence:"
  printTs . toLazyText $ pt
  putStrLn "File buffer:"
  printTs pt.fileBuffer
  putStrLn "Add buffer:"
  printTs pt.addBuffer
  putStrLn "Piece table:"
  mapM_ (putStrLn . ("    " ++) . show) pt.table
  putStrLn "\n----------------------------------------"
  where
    printTs = mapM_ (TL.putStrLn . ("    " <>)) . TL.lines
