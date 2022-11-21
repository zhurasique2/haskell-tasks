isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
            where s = floor $ sqrt $ fromIntegral n
            
primesR :: Integral a => a -> a -> [a]
primesR a b | even a = filter isPrime [a+1,a+3..b]
            | True   = filter isPrime [a,a+2..b]
            
goldbach n = head [(x,y) | x <- primesR 2 (n-2),
                           let y = n-x, isPrime y]
                           
goldbachList lb ub = map goldbach $ [even_lb,even_lb+2..ub]
  where even_lb = max ((lb+1) `div` 2 * 2) 4
goldbachList' lb ub mv = filter (\(a,b) -> a > mv && b > mv) $
                         goldbachList lb ub