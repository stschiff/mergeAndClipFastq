{-# LANGUAGE OverloadedStrings #-}

import Lib (findInsertLength, mergeReads, revComp, qualToChar, charToQual, seqMatch)

import qualified Data.ByteString.Char8 as B
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ testCase "testing revComp" assertCorrectRevComp,
                            testCase "testing searchEndDistance" checkFindInsertLength,
                            testCase "testing mergeReads" assertCorrectMergeReads,
                            testCase "test seqMatch" testSeqMatch]

assertCorrectRevComp :: Assertion
assertCorrectRevComp = do
    assertEqual "revComp" "AAGGT" (revComp "ACCTT")
    assertEqual "revComp" "AANGT" (revComp "ACNTT")

checkFindInsertLength :: Assertion
checkFindInsertLength = do
    assertEqual "searchReadEndDist" (Just 6)  (findInsertLength "ACCTGCCTGC" "GCAGGTAATC" 0.01 4 5)
    assertEqual "searchReadEndDist" (Just 8)  (findInsertLength "ACCTGCCTGC" "AGCCAGGTCC" 0.01 4 5)
    assertEqual "searchReadEndDist" (Just 10) (findInsertLength "ACCTGCCTGC" "GCAGGCTGGT" 0.01 8 5)
    assertEqual "searchReadEndDist" (Just 13) (findInsertLength "ACCTGCCTGC" "TTTGCAGGCT" 0.01 4 5)
    assertEqual "searchReadEndDist" Nothing   (findInsertLength "ACCTGCCTGC" "TTGGTTGGCC" 0.01 4 5)

assertCorrectMergeReads :: Assertion
assertCorrectMergeReads = do
    let seq1 = "ACCTGCCTGC"
        seq2 = "GCTGGTAATC"
        qual1 = replicate 10 40
        qual2 = replicate 10 30
        (mergedSeq, mergedQual) = mergeReads seq1 (qualToText qual1) seq2 (qualToText qual2) 6
    assertEqual "mergedSeq" "ACCTGC" mergedSeq
    assertEqual "mergedQual" [40, 40, 30, 40, 40, 40] (textToQual mergedQual)
  where
    qualToText = B.pack . map qualToChar
    textToQual = map charToQual . B.unpack

testSeqMatch :: Assertion
testSeqMatch = do
    assertEqual "seqMatch" False (seqMatch "AACCT" "ACCCT" 0.05)
    assertEqual "seqMatch" True  (seqMatch "AACCT" "ACCCT" 0.3)
