from timeit import timeit

N_TEST = 1000

if __name__ == "__main__":
    print("Ejecución del Test de Miller Rabin")
    # print("miller_rabin_test(46381) = ", timeit('AritmeticaModular.miller_rabin_test(46381)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(768479) = ", timeit('AritmeticaModular.miller_rabin_test(768479)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(9476407) = ", timeit('AritmeticaModular.miller_rabin_test(9476407)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(36780481) = ", timeit('AritmeticaModular.miller_rabin_test(36780481)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(562390847) = ", timeit('AritmeticaModular.miller_rabin_test(562390847)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(1894083629) = ", timeit('AritmeticaModular.miller_rabin_test(1894083629)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(65398261921) = ", timeit('AritmeticaModular.miller_rabin_test(65398261921)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(364879542899) = ", timeit('AritmeticaModular.miller_rabin_test(364879542899)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(8590365927553) = ", timeit('AritmeticaModular.miller_rabin_test(8590365927553)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(28564333765949) = ", timeit('AritmeticaModular.miller_rabin_test(28564333765949)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    # print("miller_rabin_test(12345678910111) = ", timeit('AritmeticaModular.miller_rabin_test(12345678910111)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")

    print("Ejecución del Baby Pass - Giant Pass")
    print("baby_pass_giant_pass(123456, 1749924, 46381) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 46381)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 768479) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 768479)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 9476407) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 9476407)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 36780481) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 36780481)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 562390847) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 562390847)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 1894083629) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 1894083629)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 65398261921) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 65398261921)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 364879542899) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 364879542899)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 8590365927553) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 8590365927553)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 28564333765949) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 28564333765949)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")
    print("baby_pass_giant_pass(123456, 1749924, 12345678910111) = ", timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924, 12345678910111)', setup="import AritmeticaModular", number=N_TEST)/N_TEST, " s.")