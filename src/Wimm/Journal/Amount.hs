-- |
-- Module      :  Wimm.Journal.Amount
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what an amount is.

module Wimm.Journal.Amount
    ( Amount,
      showAmount
    ) where

import qualified Data.Text as T
import Wimm.Journal.Currency

-- | All amount in the program are handled as Int since there is only one
-- currency. This means that 5.32$ is actually 532 in the program. This ensures
-- there is no rounding error or that an amount must be divided without any
-- fractional part. It is also faster and simpler than using more complex
-- packages like Data.Decimal
type Amount = Int

showAmount :: Currency -> Amount -> T.Text
showAmount curr = addSymbol . addDecimal
  where addDecimal :: Amount -> T.Text
        addDecimal = 
          if cNbOfDecimal curr == 0
          then T.pack . show
          else T.pack . snd . foldr alg (0, "") . show

        alg :: Char -> (Int, String) -> (Int, String)
        alg c (n, t) | n == cNbOfDecimal curr = (n + 1, c : T.unpack (cDecimalSep curr) ++ t)
        alg c (n, t) = (n + 1, c : t)

        addSymbol :: T.Text -> T.Text
        addSymbol amnt = 
          let space = if cSymbolInsertSpace curr then " " else ""
          in  if cSymbolBeforeAmount curr
              then T.concat [cSymbol curr, space, amnt]
              else T.concat [amnt, space, cSymbol curr]