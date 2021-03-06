{-# OPTIONS -O2 -fasm -fbang-patterns #-}
--
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart.
-- Uses Word8 values to represent Bools, avoiding a bit-packing Array Bool
--

import System
import Foreign
import Data.ByteString.Unsafe
import Data.ByteString.Internal
import Text.Printf

main = do
    n <- getArgs >>= readIO . head
    mapM_ (\i -> sieve (10000 `shiftL` (n-i))) [0, 1, 2]

sieve n = do
    a <- create n $ \p -> memset p 0 (fromIntegral n) >> return ()
    r <- go n a 0 2
    printf "Primes up to %8d %8d\n" (n::Int) (r::Int)

go m !a !c !n
    | n == m    = return c
    | true a n  = go m a c (n+1)
    | otherwise = set (n+n)
  where
    set !j | j <= m    = false a j >> set (j+n)
           | otherwise = go m a (c+1) (n+1)

true  !a !n          = unsafeIndex a n == 1
false (PS fp _ _) !n = withForeignPtr fp $ \p -> pokeByteOff p n (1 :: Word8)

