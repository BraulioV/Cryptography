module Main (main) where

    import Criterion.Main
    import System.Random
    import AritmeticaModular

    -- baby step, giant step benchmark 
    bsgs :: (Integral a, Random a) => (a, a, a) -> a
    bsgs (a, c, p) = baby_step_giant_step a c p

    prime_sqrt :: (Integral a) => (a,a) -> [a]
    prime_sqrt (a,p) = sqrts_mod a p

    sqrtsModN :: (Integral a, Random a) => (a,a,a) -> [a]
    sqrtsModN (r,p,q) = sqrts_mod_n r p q

    rhoPollard :: (Integral a, Random a) => a -> [a]
    rhoPollard n = rho_pollard n 1

    -- Our benchmark harness.
    main = defaultMain [
        -- bgroup "miller_rabin_test" [ bench "46381" $ whnf miller_rabin_test (46381 :: Integer)
                   -- , bench "768479" $ whnf miller_rabin_test (768479 :: Integer)
                   -- , bench "9476407" $ whnf miller_rabin_test (9476407 :: Integer)
                   -- , bench "36780481" $ whnf miller_rabin_test (36780481 :: Integer)
                   -- , bench "562390847" $ whnf miller_rabin_test (562390847 :: Integer)
                   -- , bench "1894083629" $ whnf miller_rabin_test (1894083629 :: Integer)
                   -- , bench "65398261921" $ whnf miller_rabin_test (65398261921 :: Integer)
                   -- , bench "364879542899" $ whnf miller_rabin_test (364879542899 :: Integer)
                   -- , bench "8590365927553" $ whnf miller_rabin_test (8590365927553 :: Integer)
                   -- , bench "28564333765949" $ whnf miller_rabin_test (28564333765949 :: Integer)
                   -- , bench "123456789101119" $ whnf miller_rabin_test (123456789101119 :: Integer)
                   -- ],
        bgroup "shanks" [ bench "46381" $ whnf bsgs ((123456 :: Integer), (1749924 :: Integer), (46381 :: Integer))
                    , bench "768479" $ whnf bsgs ((123456 :: Integer), (1749924 :: Integer), (768479 :: Integer))
                    , bench "9476407" $ whnf bsgs ((123456 :: Integer), (1749925 :: Integer), (9476407 :: Integer))
                    , bench "36780481" $ whnf bsgs ((123456 :: Integer), (1749925 :: Integer), (36780481 :: Integer))
                    , bench "562390847" $ whnf bsgs ((123456 :: Integer), (1749926 :: Integer), (562390847 :: Integer))
                    , bench "1894083629" $ whnf bsgs ((123456 :: Integer), (1749924 :: Integer), (1894083629 :: Integer))
                    , bench "65398261921" $ whnf bsgs ((123456 :: Integer), (1749925 :: Integer), (65398261921 :: Integer))
                    , bench "364879542899" $ whnf bsgs ((123456 :: Integer), (1749925 :: Integer), (364879542899 :: Integer))
                    ]
        -- bgroup "sqrt mod prime" [ bench "46381" $ whnf prime_sqrt ((123456 :: Integer), (46381 :: Integer))
        --            , bench "768479" $ whnf prime_sqrt ((123457 :: Integer), (768479 :: Integer))
        --            , bench "9476407" $ whnf prime_sqrt ((123456 :: Integer), (9476407 :: Integer))
        --            , bench "36780481" $ whnf prime_sqrt ((123456 :: Integer), (36780481 :: Integer))
        --            , bench "562390847" $ whnf prime_sqrt ((123456 :: Integer), (562390847 :: Integer))
        --            , bench "1894083629" $ whnf prime_sqrt ((123456 :: Integer), (1894083629 :: Integer))
        --            , bench "65398261921" $ whnf prime_sqrt ((123457 :: Integer), (65398261921 :: Integer))
        --            , bench "364879542899" $ whnf prime_sqrt ((123457 :: Integer), (364879542899 :: Integer))
        --            , bench "8590365927553" $ whnf prime_sqrt ((123456 :: Integer), (8590365927553 :: Integer))
        --            , bench "28564333765949" $ whnf prime_sqrt ((123460 :: Integer), (28564333765949 :: Integer))
        --            , bench "123456789101119" $ whnf prime_sqrt ((123458 :: Integer), (123456789101119 :: Integer))
        --             ],
        -- bgroup "sqrts mod n" [ bench "46381" $ whnf sqrtsModN ((123456 :: Integer), (46381 :: Integer), (46381 :: Integer))
        --            , bench "768479" $ whnf sqrtsModN ((123457 :: Integer), (46381 :: Integer), (768479 :: Integer))
        --            , bench "9476407" $ whnf sqrtsModN ((123456 :: Integer), (46381 :: Integer), (9476407 :: Integer))
        --            , bench "36780481" $ whnf sqrtsModN ((123456 :: Integer), (46381 :: Integer), (36780481 :: Integer))
        --            , bench "562390847" $ whnf sqrtsModN ((123456 :: Integer), (46381 :: Integer), (562390847 :: Integer))
        --            , bench "1894083629" $ whnf sqrtsModN ((123456 :: Integer), (46381 :: Integer), (1894083629 :: Integer))
        --            , bench "65398261921" $ whnf sqrtsModN ((123457 :: Integer), (46381 :: Integer), (65398261921 :: Integer))
        --            , bench "364879542899" $ whnf sqrtsModN ((123457 :: Integer), (46381 :: Integer), (364879542899 :: Integer))
        --            , bench "8590365927553" $ whnf sqrtsModN ((123456 :: Integer), (46381 :: Integer), (8590365927553 :: Integer))
        --            , bench "28564333765949" $ whnf sqrtsModN ((123460 :: Integer), (46381 :: Integer), (28564333765949 :: Integer))
        --            , bench "123456789101119" $ whnf sqrtsModN ((123458 :: Integer), (46381 :: Integer), (123456789101119 :: Integer))
        --             ],
        -- bgroup "fermat" [ bench "46382" $ whnf fermat (46382 :: Integer)
        --            , bench "7684781" $ whnf fermat (7684781 :: Integer)
        --            , bench "9476408" $ whnf fermat (9476408 :: Integer)
        --            , bench "36780482" $ whnf fermat (36780482 :: Integer)
        --            , bench "562390848" $ whnf fermat (562390848 :: Integer)
        --            , bench "1894083630" $ whnf fermat (1894083630 :: Integer)
        --            , bench "65398261923" $ whnf fermat (65398261923 :: Integer)
        --            , bench "364879542897" $ whnf fermat (364879542897 :: Integer)
        --            , bench "8590365927554" $ whnf fermat (8590365927554 :: Integer)
        --            , bench "28564333765948" $ whnf fermat (28564333765948 :: Integer)
        --            , bench "123456789101121" $ whnf fermat (123456789101121 :: Integer)
        --            ],
        -- bgroup "rho" [ bench "46382" $ whnf rhoPollard (46382 :: Integer)
        --            , bench "768481" $ whnf rhoPollard (768481 :: Integer)
        --            , bench "9476408"  $ whnf rhoPollard (9476408 :: Integer)
        --            , bench "36780482" $ whnf rhoPollard (36780482 :: Integer)
        --            , bench "562390848" $ whnf rhoPollard (562390848 :: Integer)
        --            , bench "1894083630" $ whnf rhoPollard (1894083630 :: Integer)
        --            , bench "65398261923" $ whnf rhoPollard (65398261923 :: Integer)
        --            , bench "364879542897" $ whnf rhoPollard (364879542897 :: Integer)
        --            , bench "8590365927554" $ whnf rhoPollard (8590365927554 :: Integer)
        --            , bench "28564333765948" $ whnf rhoPollard (28564333765948 :: Integer)
        --            , bench "123456789101121" $ whnf rhoPollard (123456789101121 :: Integer)
        --            ]
        ]
