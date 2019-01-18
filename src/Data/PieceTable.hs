{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
import           Control.Exception           ( try )
import           Control.Lens                hiding ( (|>), (<|) )
import qualified Data.ByteString.Lazy        as BL
import           Data.FingerTree             ( (|>), (<|), (><) )
import qualified Data.FingerTree             as F
import           Data.Foldable               ( toList )
import           Data.Int                    ( Int64 )
import           Data.Monoid                 ( Sum(..) )
import           Data.String
import           Data.Text.Encoding.Error    as TEE
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Encoding     as TLE
import qualified Data.Text.Lazy.IO           as TLIO

-------------------------------------------------------------------------------
-- Types and Instances

-- FileType.
data FileType
    = Orig    -- Original File
    | Add     -- Add File
  deriving ( Eq, Show )

-- Each Piece points to a span in the original or add file.
data Piece = Piece
    { _fileType :: FileType                -- Which file the Piece refers to.
    , _start    :: {-# UNPACK #-} Int64    -- Offset into the the file.
    , _len      :: {-# UNPACK #-} Int64    -- The length of the piece.
    }

makeLenses ''Piece

instance Show Piece where
    show (Piece f s l) = unwords [ show f, show s, show l ]

-- Measured instance for Piece.
-- Each Piece is measured by its length.
instance F.Measured (Sum Int64) Piece where
    measure = views len Sum

-- Type synonym for the FingerTree of the Piece.
type Table = F.FingerTree (Sum Int64) Piece

-- Type synonym for the Text.
type OrigFile = TL.Text
type AddFile  = TL.Text

-- The PieceTable.
data PieceTable = PieceTable
    { _table    :: Table       -- Sequence of the Piece.
    , _origFile :: OrigFile    -- Original file (read only).
    , _addFile  :: AddFile     -- Add file (append only).
    }

makeLenses ''PieceTable

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
    p = Piece Orig 0 (TL.length t)

-- Convert PieceTable to Text.
toText :: PieceTable -> TL.Text
toText = foldMap <$> (<!>) <*> view table

toString :: PieceTable -> String
toString = TL.unpack . toText

readFile :: FilePath -> IO (Either String PieceTable)
readFile p = BL.readFile p >>= go decs
  where
    go []     _ = pure (Left err)
    go (d:ds) b = try (pure (d b)) >>= \case
        Left  (_ :: TEE.UnicodeException) -> go ds b
        Right t -> pure (Right (fromText t))
    err = "Could not guess the encoding of " ++ p
    decs =
        [ TLE.decodeUtf8
        , TLE.decodeUtf16LE
        , TLE.decodeUtf16BE
        , TLE.decodeUtf32LE
        , TLE.decodeUtf32BE
        ]

-------------------------------------------------------------------------------
-- Opetators

infixr 5 <?
infixl 5 ?>
infixl 9 <!>

(?>) :: Table -> Piece -> Table
t ?> p = if p ^. len == 0 then t else t |> p

(<?) :: Piece -> Table -> Table
p <? t = if p ^. len == 0 then t else p <| t

-- Get the substring that the piece represents.
(<!>) :: PieceTable -> Piece -> TL.Text
pt <!> Piece t s l = TL.take l . TL.drop s $ case t of
    Orig -> pt ^. origFile
    Add  -> pt ^. addFile

-------------------------------------------------------------------------------

-- Split Piece at the specified position.
splitPiece :: Int64 -> Piece -> (Piece, Piece)
splitPiece i (Piece f s l) = ( Piece f s i, Piece f (s + i) (l - i) )

-- Split Table at the specified position.
splitTable :: Int64 -> Table -> (Table, Table)
splitTable i t = case F.search (const . (Sum i <)) t of
    F.Position l m r ->
        let d = i - getSum (F.measure l)
        in  (l ?>) *** (<? r) $ splitPiece d m
    F.OnLeft  -> (F.empty, t)
    F.OnRight -> (t, F.empty)
    F.Nowhere -> (t, F.empty)

leftOf, rightOf :: Int64 -> Table -> Table
leftOf  i = fst . splitTable i
rightOf i = snd . splitTable i

-------------------------------------------------------------------------------
-- Editting Operations

-- Insert Text at the specified position in the given PieceTable.
insert :: Int64 -> TL.Text -> PieceTable -> PieceTable
insert i t pt = pt &~ do
    let p = Piece Add (views addFile TL.length pt) (TL.length t)
    table %= uncurry (><) . fmap (p <?) . splitTable i
    addFile <>= t

-- Delete String of the specified range from PieceTable.
delete :: Int64 -> Int64 -> PieceTable -> PieceTable
delete i j pt = case compare i j of
    LT -> pt & table %~ ((><) <$> leftOf i <*> rightOf j)
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
    printTs . view origFile $ pt
    putStrLn "Add file:"
    printTs . view addFile $ pt
    putStrLn "Table:"
    mapM_ (putStrLn . ("    " ++) . show)  . toList . view table $ pt
    putStrLn "\n----------------------------------------"
  where
    printTs :: TL.Text -> IO ()
    printTs = mapM_ (TLIO.putStrLn . ("    " <>)) . TL.lines

