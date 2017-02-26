
ext_euclides :: Integer -> Integer -> [Integer]
ext_euclides a b = ext_euclides' a b

ext_euclides' :: Integer -> Integer -> [Integer]
ext_euclides' a 0 = [a, 1, 0]
ext_euclides' 0 b = [b, 0, 1]
ext_euclides' a b = [d, m, n - (a `div` b) * m]
    where
        [d,n,m] = ext_euclides' b (a `mod` b)

ext_euclides 4864 3458

inverse :: Integer -> Integer -> Integer
inverse a b = ext_euclides a b !! 1 `mod` b

inverse 2 5

big_pow :: Integer -> Integer -> Integer -> Integer
big_pow _ 0 _ = 1
big_pow a b n = pow a b 1 n

pow :: Integer -> Integer -> Integer -> Integer -> Integer
pow _ 0 p _ = p
pow a b p n 
        | b `mod` 2 == 1 = pow (a*a `mod` n) (b `div` 2) ((p * a) `mod` n) n
        | otherwise      = pow (a*a `mod` n) (b `div` 2) p n
        
big_pow 5 35 7

miller-rabin :: Integer -> Bool
miller-rabin n = 
