{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Data.Text.PieceTable where

import Control.Arrow
import Control.Exception (try)
import Data.ByteString.Lazy qualified as BL
import Data.Coerce
import Data.FingerTree ((<|), (><), (|>))
import Data.FingerTree qualified as F
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.Monoid (Sum (..))
import Data.String
import Data.Text.Encoding.Error as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.Lazy.IO qualified as TLIO

-------------------------------------------------------------------------------
-- Types and Instances

data FileType
  = Original
  | Add
  deriving (Eq, Show)

-- Each Piece points to a span in the original or add file.
data Piece = Piece
  { fileType :: !FileType, -- Which file the Piece refers to.
    start :: {-# UNPACK #-} !Int64, -- Offset into the the file.
    length :: {-# UNPACK #-} !Int64 -- The length of the piece.
  }
  deriving (Show)

-- A piece is measured by its length.
instance F.Measured (Sum Int64) Piece where
  measure p = coerce p.length
  {-# INLINE measure #-}

type Table = F.FingerTree (Sum Int64) Piece

-- The PieceTable.
data PieceTable = PieceTable
  { table :: Table,
    originalFile :: TL.Text, -- read only.
    addFile :: TL.Text -- append only.
  }

instance Show PieceTable where
  show = TL.unpack . toText

instance IsString PieceTable where
  fromString = fromText . TL.pack

-------------------------------------------------------------------------------
-- Constructions and Deconstruction

-- Empty PieceTable.
empty :: PieceTable
empty = PieceTable F.empty TL.empty TL.empty

-- Create PieceTable from Text.
fromText :: TL.Text -> PieceTable
fromText t = PieceTable (F.singleton p) t TL.empty
  where
    p = Piece Original 0 (TL.length t)

-- Convert PieceTable to Text.
toText :: PieceTable -> TL.Text
toText pt = foldMap (pt <!>) pt.table

toString :: PieceTable -> String
toString = TL.unpack . toText

readFile :: FilePath -> IO (Either String PieceTable)
readFile p = BL.readFile p >>= go decs
  where
    go [] _ = pure (Left err)
    go (d : ds) b =
      try (pure (d b)) >>= \case
        Left (_ :: TEE.UnicodeException) -> go ds b
        Right t -> pure (Right (fromText t))
    err = "Could not guess the encoding of " ++ p
    decs =
      [ TLE.decodeUtf8,
        TLE.decodeUtf16LE,
        TLE.decodeUtf16BE,
        TLE.decodeUtf32LE,
        TLE.decodeUtf32BE
      ]

-------------------------------------------------------------------------------
-- Opetators

infixr 5 <?

infixl 5 ?>

infixl 9 <!>

(?>) :: Table -> Piece -> Table
t ?> p = if p.length == 0 then t else t |> p

(<?) :: Piece -> Table -> Table
p <? t = if p.length == 0 then t else p <| t

-- Get the substring that the piece represents.
(<!>) :: PieceTable -> Piece -> TL.Text
pt <!> Piece t s l = TL.take l . TL.drop s $ case t of
  Original -> pt.originalFile
  Add -> pt.addFile

-------------------------------------------------------------------------------

-- Split Piece at the specified position.
splitPiece :: Int64 -> Piece -> (Piece, Piece)
splitPiece i (Piece f s l) = (Piece f s i, Piece f (s + i) (l - i))

-- Split Table at the specified position.
splitTable :: Int64 -> Table -> (Table, Table)
splitTable i t = case F.search (const . (Sum i <)) t of
  F.Position l m r ->
    let d = i - coerce (F.measure l)
     in (l ?>) *** (<? r) $ splitPiece d m
  F.OnLeft -> (F.empty, t)
  F.OnRight -> (t, F.empty)
  F.Nowhere -> (t, F.empty)

leftOf, rightOf :: Int64 -> Table -> Table
leftOf i = fst . splitTable i
rightOf i = snd . splitTable i

-------------------------------------------------------------------------------
-- Editting Operations

-- Insert Text at the specified position in the given PieceTable.
insert :: Int64 -> TL.Text -> PieceTable -> PieceTable
insert i t pt =
  pt
    { table = uncurry (><) . fmap (p <?) . splitTable i $ pt.table,
      addFile = pt.addFile <> t
    }
  where
    p = Piece Add (TL.length $ pt.addFile) (TL.length t)

-- Delete String of the specified range from PieceTable.
delete :: Int64 -> Int64 -> PieceTable -> PieceTable
delete i j pt = case compare i j of
  LT -> pt {table = leftOf i pt.table >< rightOf j pt.table}
  EQ -> pt
  GT -> delete j i pt

-------------------------------------------------------------------------------
-- Debugging

-- Print PieceTable for debugging.
printPT :: PieceTable -> IO ()
printPT pt = do
  putStrLn "----------------------------------------\n"
  putStrLn "Text sequence:"
  printTs . toText $ pt
  putStrLn "Original file:"
  printTs pt.originalFile
  putStrLn "Add file:"
  printTs pt.addFile
  putStrLn "Table:"
  mapM_ (putStrLn . ("    " ++) . show) . toList $ pt.table
  putStrLn "\n----------------------------------------"
  where
    printTs :: TL.Text -> IO ()
    printTs = mapM_ (TLIO.putStrLn . ("    " <>)) . TL.lines
