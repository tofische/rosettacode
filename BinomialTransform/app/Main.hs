module Main (main) where

import Control.Monad (forM_)
import Data (seqs)
import Lib (forward, inverse, self)

main :: IO ()
main = forM_ seqs testSeq
    where
        testSeq (n, s) = do
            print $ n <> ": " <> show s
            print $ "   Forward: " <> show (forward s)
            print $ "   Inverse: " <> show (inverse s)
            print $ "   Self: " <> show (self s)
            print $ "   Inverse of Forward: " <> show (inverse (forward s))
            print $ "   Self of Self: " <> show (self (self s))
