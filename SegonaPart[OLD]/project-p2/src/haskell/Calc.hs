
-- Canvis:
--   Tractament d'errors.

{-# LANGUAGE OverloadedStrings #-}

module Calc where
import           Data.Foldable    -- exporta la funcio 'foldlM'
import           Data.Text (Text)

type CalcState a = [a]

data CalcEvent a = CalcEnter a
                 | CalcBin (a -> a -> a) | CalcUn (a -> a)
                 | CalcDup | CalcPop | CalcFlip

calcInit :: CalcState a
calcInit = []

calcTop :: CalcState a -> Either Text a
calcTop (x:_) = Right x
calcTop _     = Left "The stack is empty"

calcSolve :: [CalcEvent a] -> CalcState a -> Either Text (CalcState a)
calcSolve evs xs =
    foldlM (flip calcSolve1) xs evs

calcSolve1 :: CalcEvent a -> CalcState a -> Either Text (CalcState a)
calcSolve1 (CalcEnter n) xs =
    Right $ n : xs
calcSolve1 (CalcBin op) (x1:x2:xs) =
    Right $ op x2 x1 : xs
calcSolve1 (CalcBin _) _ =
    Left "A binary operator needs a stack with 2 operands"
calcSolve1 (CalcUn op) (x1:xs) =
    Right $ op x1 : xs
calcSolve1 (CalcUn _) _ =
    Left "An unary operator needs a stack with 1 operand"
calcSolve1 CalcDup (x1:xs) =
    Right $ x1 : x1 : xs
calcSolve1 CalcDup _ =
    Left "The dup operator needs a stack with 1 operand"
calcSolve1 CalcPop (x1:xs) =
    Right $ xs
calcSolve1 CalcPop _ =
    Left "The pop operator needs a stack with 1 operand"
calcSolve1 CalcFlip (x1:x2:xs) =
    Right $ x2 : x1 : xs
calcSolve1 CalcFlip _ =
    Left "The flip operator needs a stack with 2 operands"

