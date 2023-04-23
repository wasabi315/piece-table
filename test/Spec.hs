{-# LANGUAGE FlexibleInstances #-}

import Data.Int
import qualified Data.Text.Lazy as TL
import Data.Text.PieceTable
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck . verbose $
    forAll genIx $ \i ->
      forAll genText $ \t ->
        forAll genText $ \s ->
          propInsert i t s
  quickCheck . verbose $
    forAll genIx $ \i ->
      forAll genIx $ \j ->
        forAll genText $ \t ->
          propDelete i j t

genIx :: Gen Int64
genIx = arbitrary

genText :: Gen TL.Text
genText = fmap TL.pack arbitrary

ins :: Int64 -> TL.Text -> TL.Text -> TL.Text
ins i s t
  | i < 0 = t
  | i > TL.length t = t
  | otherwise = let (l, r) = TL.splitAt i t in l <> s <> r

propInsert :: Int64 -> TL.Text -> TL.Text -> Bool
propInsert i t pt =
  toText (insert i t (fromText pt))
    == ins i t pt

propDelete :: Int64 -> Int64 -> TL.Text -> Bool
propDelete i j pt =
  toText (delete i j (fromText pt))
    == del i j pt
  where
    del n m t = case compare n m of
      LT -> TL.take n pt <> TL.drop m t
      EQ -> t
      GT -> del m n t
