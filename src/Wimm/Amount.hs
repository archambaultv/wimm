-- |
-- Module      :  Wimm.Amount
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental
--
-- This module defines what an amount is.

module Wimm.Amount
    ( Amount,
      showAmount
    ) where

import qualified Data.Text as T
import Wimm.Currency

-- | All amount in the program are handled as Int since there is only one
-- currency. This means that 5.32$ is actually 532 in the program. This ensures
-- there is no rounding error or that an amount must be divided without any
-- fractional part. It is also faster and simpler than using more complex
-- packages like Data.Decimal
type Amount = Int

showAmount :: Currency -> Amount -> T.Text
showAmount curr | cNbOfDecimal curr == 0 = T.pack . show
showAmount curr = T.pack . snd . foldr alg (0, "") . show
    where alg :: Char -> (Int, String) -> (Int, String)
          alg c (n, t) | n == cNbOfDecimal curr = (n + 1, c : T.unpack (cDecimalSep curr) ++ t)
          alg c (n, t) = (n + 1, c : t)