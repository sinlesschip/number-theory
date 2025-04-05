-- | calclate modulus of two integers using the division algorithm 
mod' :: Int -> Int -> Int
mod' a n = r
  where
    q = a `div` n
    r = a - q * n

-- | The Euclidean algorithm for finding the highest common factor
--   of two integers.
hcf :: Int -> Int -> Int
hcf n 0 = n
hcf n r = hcf r (n `mod'` r)