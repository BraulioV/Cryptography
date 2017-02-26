ext_euclides :: Integer -> Integer -> [Integer]
ext_euclides a b = ext_euclides' a b

ext_euclides' :: Integer -> Integer -> [Integer]
ext_euclides' a 0 = [a, 1, 0]
ext_euclides' 0 b = [b, 0, 1]
ext_euclides' a b = [d, m, n - (a `div` b) * m]
    where
        [d,n,m] = ext_euclides' b (a `mod` b)

