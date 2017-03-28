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
inverse a b = ext_euclides a b !! 1 

big_pow :: Integral a => a -> a -> a -> a
big_pow _ 0 _ = 1
big_pow a b n = pow a b 1 n

pow :: Integral a => a -> a -> a -> a -> a
pow _ 0 p _ = p
pow a b p n 
        | b `mod` 2 == 1 = pow (a*a `mod` n) (b `div` 2) ((p * a) `mod` n) n
        | otherwise      = pow (a*a `mod` n) (b `div` 2) p n

-- Descompone un número p tal que p = 2^s * u
bifactor :: Integral a => a -> [a]
bifactor num = bifactor' num 0

bifactor' :: Integral a => a -> a -> [a]
bifactor' 0 s = [s, 1]
bifactor' a0 s 
    | a0 `mod` 2 == 0 = bifactor' (a0 `div` 2) (s + 1)  -- si a | b => s+=1 y comprobamos con a / 2
    | otherwise       = [s, a0]                         -- en caso contario, devolvemos s y u


miller_rabin :: (Integral a, Random a) => a -> Bool
miller_rabin p 
    | p == 2 || p == 3                   = True  -- primos menores que 5
    | p == 4 || p < 2 || p `mod` 2 == 0  = False -- es 4 o menor que 4 
    | otherwise                          = test_mr p l
    where
        s_u = bifactor (p - 1)  -- Descomponemos p - 1 = 2^s * u --> [s, u]
        a = unsafePerformIO $ randomRIO (2, p - 2) -- obtenemos una semilla aleatoria para el test
        l = map (\x -> big_pow a (2^x * s_u !! 1) p) [0..s_u !! 0] -- y construimos la lista

        test_mr :: (Integral a) => a -> [a] -> Bool
        test_mr p l
            -- Primer elemento de la lista es 1 o -1
            | (head l) == 1 || (head l) == (p - 1)                 = True  -- Probablemente primo
            -- Si -1 está en la lista y no es el último elemento
            | (p - 1) `elem` l && (last l /= p - 1)                = True  -- Probablemente primo
            -- Ninguna de las potencias es igual a 1
            | not (1 `elem` l)                                     = False -- No es primo
            -- Si aparece un 1 en la lista no precedido de un -1
            | 1 `elem` l && (last $ takeWhile (/= 1) l) /= (p - 1) = False -- No es primo
            -- En otro caso
            | otherwise                                            = False -- No es primo

miller_rabin_test :: (Integral a, Random a) => a -> Int -> Bool
-- Realiza un and con las n salidas del test de Miller-Rabin
miller_rabin_test p n = and $ replicate n (miller_rabin p)  

-- Función para devolver el índice como un entero
-- en vez de un Maybe Int con elemIndex
indexOf :: (Integral a) => a -> [a] -> a
indexOf y xs = index y xs 0
    
index :: (Integral a) => a -> [a] -> a -> a
index _ [] n            = -1 
index y (x:xs) n
            | y /= x    = index y xs n + 1 
            | otherwise = n

baby_pass_giant_pass :: (Integral a, Random a) => a -> a -> a -> [a]
baby_pass_giant_pass _ 1 _ = [0] -- Si b == 1, devolvemos un 0
baby_pass_giant_pass a b p 
    -- Si es primo, devolvemos los ks que cumplen a^k = b en Z_p
    | (miller_rabin_test p 5) = ks
    | otherwise               = []
    where
        s = ceiling $ sqrt $ fromIntegral (p-1)
        -- Calculamos el paso gigante => L
        big_pass = map (\x -> big_pow a (x * s) p) [1..s] 
        -- Calculamos el paso pequeño => l
        low_pass = map (\x -> (b * a^x) `mod` p ) [0..s - 1] 
        -- Realizamos la intersección de L y l y 
        -- calculamos para la lista resultante
        -- para cada k en ks => k = cs - r
        ks = map (\x -> ((indexOf x big_pass) + 1) * s - 
            (indexOf x low_pass)) (intersect big_pass low_pass)
        

jacobi :: Integral a => a -> a -> a
jacobi a p 
    | a > p      = jacobi (a `mod` p) p
    | u > 0      = ((-1) ^ (((p^2) - 1) `div` 8)) * (jacobi' s p)
    | otherwise  = jacobi' s p
        where
            u_s = bifactor a
            u   = head u_s
            s   = last u_s
            jacobi' :: Integral a => a -> a -> a
            jacobi' a p 
                | a == 0      = 0
                | a == 1      = 1
                | a == -1     = (-1)^((p - 1) `div` 2)
                | a `mod` 2 /= 0 && p `mod` 2 /= 0 = jacobi p a
                | otherwise                        = jacobi a p


sqrts_mod :: (Integral b) => b -> b -> [b]
sqrts_mod a p = sort [sqr, p - sqr]
    where
        sqr = sqrt_mod a p


sqrt_mod :: (Integral b) => b -> b -> b
sqrt_mod a p
    | jacobi a p /= 1 = error "No existen raíces para a mod p" 
    | u == 1          = big_pow a ((p + 1) `div` 4) p
    | otherwise       = search_residual a u s n p
        where 
            u_s = bifactor (p - 1)
            u = head u_s
            s = last u_s
            n =  1 + (last $ takeWhile  (\x -> (jacobi x p) == 1) [2..p - 1])

search_residual :: (Integral b) => b -> b -> b -> b -> b -> b
search_residual a u s n p = res r b j inv_a u p
    where
        r     = big_pow a ((s + 1) `div` 2) p
        b     = big_pow n s p
        j     = 0
        inv_a = inverse a p
        res :: (Integral b) => b -> b -> b -> b -> b -> b -> b
        res r b j inv_a u p 
            | j > (u - 2) = r
            | (big_pow (inv_a * r^2) (2^(u - 2 -j)) p) == (p - 1) = res ((r * b) `mod` p) (b^2) (j+1) inv_a u p
            | otherwise = res r (b^2) (j+1) inv_a u p
            
