{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Data.Int
import Data.Text.Lazy qualified as TL
import Data.Text.PieceTable qualified as PT
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
  PT.toText (PT.insert i t (PT.fromText pt))
    == ins i t pt

propDelete :: Int64 -> Int64 -> TL.Text -> Bool
propDelete i j pt =
  PT.toText (PT.delete i j (PT.fromText pt))
    == del i j pt
  where
    del n m t = case compare n m of
      LT -> TL.take n pt <> TL.drop m t
      EQ -> t
      GT -> del m n t
