{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( findInsertLength
    , mergeReads
    , revComp
    , qualToChar
    , charToQual
    , seqMatch
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Char (ord, chr)
import Data.List (dropWhile)

findInsertLength :: B.ByteString -> B.ByteString -> Double -> Int -> Int -> Maybe Int
findInsertLength seq1 seq2 mismatchRate minOverlapSize minLength =
    let l1 = B.length seq1
        l2 = B.length seq2
        testLengths = [minLength..(l1 + l2 - minOverlapSize)]
        matchOverlap i =
            let (o1, o2) = if i <= l1 then
                               (B.take i seq1, B.take i . revComp $ seq2)
                           else
                               (B.drop (i - l2) seq1, revComp . B.drop (i - l1) $ seq2)
            in  seqMatch o1 o2 mismatchRate
        matchResults = dropWhile (not . snd) . zip testLengths . map matchOverlap $ testLengths
    in  case matchResults of
        [] -> Nothing
        l -> Just . fst . head $ l

mergeReads :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Int -> (B.ByteString, B.ByteString)
mergeReads seq1 qual1 seq2 qual2 insertLength =
    if insertLength <= B.length seq1 then
        let seq1Part = B.take insertLength seq1
            seq2Part = B.take insertLength . revComp $ seq2
            qual1Part = B.take insertLength qual1
            qual2Part = B.take insertLength . B.reverse $ qual2
        in  mergeZip (seq1Part, seq2Part) (qual1Part, qual2Part)
    else
        let (seqPrefix, seq1Part) = B.splitAt (insertLength - B.length seq2) seq1
            (seqSuffix, seq2Part) = (\(a, b) -> (revComp a, revComp b)) . B.splitAt (insertLength - B.length seq1) $ seq2
            (qualPrefix, qual1Part) = B.splitAt (insertLength - B.length seq2) qual1
            (qualSuffix, qual2Part) = (\(a, b) -> (B.reverse a, B.reverse b)) . B.splitAt (insertLength - B.length seq1) $ qual2
            (overlapSeq, overlapQual) = mergeZip (seq1Part, seq2Part) (qual1Part, qual2Part)
        in  (B.concat [seqPrefix, overlapSeq, seqSuffix], B.concat [qualPrefix, overlapQual, qualSuffix])

mergeZip :: (B.ByteString, B.ByteString) -> (B.ByteString, B.ByteString) -> (B.ByteString, B.ByteString)
mergeZip (seq1, seq2) (qual1, qual2) =
    let (seqList, qualList) = foldr go ([], []) $ zipSeq4
    in  (B.pack seqList, B.pack qualList)
  where
    go (s1, s2, q1, q2) (resSeq, resQual) =
        let (newS, newQ) =
                if s1 == s2 then
                    (s1, chr $ max (ord q1) (ord q2))
                else
                    if s1 == 'N' then (s2, q2) else
                        if s2 == 'N' then (s1, q1) else
                            case compare (ord q1) (ord q2) of
                                LT -> (s2, q1)
                                GT -> (s1, q2)
                                EQ -> (s1, q1)
        in  (newS:resSeq, newQ:resQual)
    zipSeq4 = zipWith (\(a, b) (c, d) -> (a, b, c, d)) (B.zip seq1 seq2) (B.zip qual1 qual2 )

revComp :: B.ByteString -> B.ByteString
revComp = B.map complementBase . B.reverse
  where
    complementBase c = case c of
        'A' -> 'T'
        'T' -> 'A'
        'C' -> 'G'
        'G' -> 'C'
        'N' -> 'N'
        _   -> error $ "unknown base " ++ [c]

qualToChar :: Int -> Char
qualToChar = chr . min 40 . (+33)

charToQual :: Char -> Int
charToQual = (-) 33 . ord

seqMatch :: B.ByteString -> B.ByteString -> Double -> Bool
seqMatch seq1 seq2 mismatchRate = 
    if B.length seq1 /= B.length seq2 then error "sequence lengths do not match" else
        let s = sum $ B.zipWith nucDiff seq1 seq2
        in  s / (fromIntegral $ B.length seq1) <= mismatchRate
  where
    nucDiff a b = if a == 'N' || b == 'N' || (a == b) then 0 else 1

