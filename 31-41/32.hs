gcd 0 y = y
gcd x y = gcd' (y `mod` x) x
myGCD x y | x < 0     = myGCD (-x) y
          | y < 0     = myGCD x (-y)
          | y < x     = gcd y x
          | otherwise = gcd x y