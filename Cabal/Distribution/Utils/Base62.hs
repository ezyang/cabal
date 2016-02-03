module Distribution.Utils.Base62 (hashToBase62) where

import GHC.Fingerprint ( Fingerprint(..), fingerprintString )
import Numeric ( showIntAtBase )
import Data.Char ( chr )

hashToBase62 :: String -> String
hashToBase62 s = showFingerprint $ fingerprintString s
  where
    showIntAtBase62 x = showIntAtBase 62 representBase62 x ""
    representBase62 x
        | x < 10 = chr (48 + x)
        | x < 36 = chr (65 + x - 10)
        | x < 62 = chr (97 + x - 36)
        | otherwise = '@'
    showFingerprint (Fingerprint a b) = showIntAtBase62 a ++ showIntAtBase62 b

