import System.Random
import System.IO.Unsafe
import Data.List

ext_euclides :: Integral a => a -> a -> [a]
ext_euclides a b = ext_euclides' a b

ext_euclides' :: Integral a => a -> a -> [a]
ext_euclides' a 0 = [a, 1, 0]
ext_euclides' 0 b = [b, 0, 1]
ext_euclides' a b = [d, m, n - (a `div` b) * m]
    where
        [d,n,m] = ext_euclides' b (a `mod` b)


inverse :: Integral a => a -> a -> a
inverse a b = ext_euclides a b !! 1 `mod` b


big_pow :: Integral a => a -> a -> a -> a
big_pow _ 0 _ = 1
big_pow a b n = pow a b 1 n


pow :: Integral a => a -> a -> a -> a -> a
pow _ 0 p _ = p
pow a b p n 
        | b `mod` 2 == 1 = pow (a*a `mod` n) (b `div` 2) ((p * a) `mod` n) n
        | otherwise      = pow (a*a `mod` n) (b `div` 2) p n

        
bifactor :: Integral a => a -> [a]
bifactor num = bifactor' num 0

bifactor' :: Integral a => a -> a -> [a]
bifactor' 0 s = [s, 1]
bifactor' a0 s 
    | a0 `mod` 2 == 0 = bifactor' (a0 `div` 2) (s + 1)
    | otherwise       = [s, a0]


miller_rabin :: (Integral a, Random a) => a -> Bool
miller_rabin p = test_mr p l
    where
        s_u = bifactor (p - 1)
        a = unsafePerformIO $ randomRIO (2, p - 2)
        l = map (\x -> big_pow a ((2^x)*s_u !! 1) p) [0..s_u !! 0]

        test_mr :: (Integral a) => a -> [a] -> Bool
        test_mr p l
            | (head l) == 1 || (head l) == (p - 1) = True
            | (p - 1) `elem` l && (last l /= p - 1) = True
            | not (1 `elem` l) = False
            | 1 `elem` l && (last $ takeWhile (/= 1) l) /= (p - 1) = False
            | otherwise = False


miller_rabin_test :: (Integral a, Random a) => a -> Int -> Bool
miller_rabin_test p n = and $ replicate n (miller_rabin p)