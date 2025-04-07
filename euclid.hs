-- | The Euclidean algorithm for finding 
--   the highest common factor
--   of two integers.
hcf :: Integer -> Integer -> Integer
hcf a 0 = a
hcf a b = hcf b (a `mod` b)

-- | Bezout’s identity:
--   Suppose that a and b are integers, not both 0, and let d be their
--   highest common factor. Then there are integers v and w such that
--   av + bw = d.

--   Below, we will calculate v and w using backwards substitution,
--   its best to use the Extended Eucliden Algorithm, but this is just
--   for demonstration

-- | calclate steps in euclidean algorithm
euclidSteps :: Integer -> Integer -> [(Integer, Integer)]
euclidSteps a b 
    | b == 0 = []
    | otherwise = let q = a `div` b 
                      r = a - q * b
                  in (q, r) : euclidSteps b r 

-- | perform back substitution 
backSubstitute :: [(Integer, Integer)] -> (Integer, Integer)
backSubstitute steps = result steps (1, 0) (0, 1)
  where
    result [] (x0, _) (y0, _) = (x0, y0)
    result ((q, _):rest) (x0, x1) (y0, y1) = 
      result rest (x1, x0 - q * x1) (y1, y0 - q * y1)

-- | all together, finding bezout identity using backsub  
bezoutBackSub :: Integer -> Integer -> (Integer, Integer)
bezoutBackSub a b = 
    let steps = euclidSteps a b 
    in backSubstitute steps