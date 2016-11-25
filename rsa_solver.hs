primes :: Int -> [Int]
primes n = eratos n [2 .. n]

eratos :: Int -> [Int] -> [Int]
eratos _ []    = []
eratos n (h:t)
  | n >= h*h  = h : (eratos n $ filter (\x->(mod x h) /= 0) t)
  | otherwise = h : t

solve_pq :: Int -> Maybe (Int, Int)
solve_pq n = let harf_primes = (primes (div n 2))
                 solve_pq' []    = Nothing
                 solve_pq' (h:t) =
                   if h*h <= n then
                   case (divMod n h) of
                     (q,r) | (r==0) && (elem q (h:t)) -> Just (h,q)
                           | otherwise                -> solve_pq' t
                   else
                     Nothing
             in solve_pq' harf_primes

euclid :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
euclid (x1,y1,z1) (x2,y2,z2)
  | z2 == 1 && y2 > 0  = (x2,y2,z2)
  | z2 == 1 && y2 < 0  = ((x1-x2),(y1-y2),z1-z2)
  | otherwise          = let q = div z1 z2 in euclid (x2,y2,z2) ((x1-q*x2),(y1-q*y2),z1-q*z2)

solve_private_from_public :: (Int, Int) -> Maybe (Int, Int)
solve_private_from_public (e, n) =
  case solve_pq n of
    Just (p,q) -> (\(_,d,_)-> Just (d,n)) (euclid (1,0,(p-1)*(q-1)) (0,1,e))
    Nothing    -> Nothing

encrypt :: Integer -> (Integer, Integer) -> Integer
encrypt message (e, n) = mod (message ^ e) n

decrypt :: Integer -> (Integer, Integer) -> Integer
decrypt message (d, n) = mod (message ^ d) n
