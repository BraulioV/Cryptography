import numpy as np
from timeit import timeit
from sys import argv

N_TEST = 1000

if __name__ == "__main__":
    if len(argv) != 2:
        raise AttributeError("Use: python main.py \"BENCHMARK\"")

    cases = [46381, 768479, 9476407, 36780481, 562390847, 1894083629, 65398261921, 
             364879542899, 8590365927553, 28564333765949, 12345678910111, ]

    if argv[1] == "MR":
        print("Ejecución del Test de Miller Rabin")
        results = np.zeros(shape=(11, 2), dtype = np.float)
        
        for i in range(len(cases)):
            results[i,:] = [cases[i], timeit('AritmeticaModular.miller_rabin_test('+ str(cases[i]) + ')', 
                setup="import AritmeticaModular", number=N_TEST)/N_TEST]

    elif argv[1] == "BPGP":
        print("Ejecución del Baby Pass - Giant Pass")
        baby_times = np.zeros((11, 2), dtype = np.float)
        for i in range(len(cases)):
            results[i,:] = [cases[i], timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924,' +
             str(cases[i]) +')', setup="import AritmeticaModular", number=N_TEST)]

    elif argv[1] == "Jacobi":
        print("Ejecución del símbolo de Jacobi")
        results = np.zeros((11, 2), dtype = np.float)
        for i in range(len(cases)):
            results[i,:] = [cases[i], timeit('AritmeticaModular.Jacobi(1749924, ' 
                + str(cases[i]) + ')', setup="import AritmeticaModular", number=N_TEST)]

    elif argv[1] == "SQRT":
        print("Ejecución de las raíces modulares")
        results = np.zeros((11, 2), dtype = np.float)
        for i in range(len(cases)):
            results[i,:] = [cases[i], timeit('AritmeticaModular.Jacobi(123456, ' + str(cases[i]) + ', ' 
                + str(cases[-(i+1)]) + ')', setup="import AritmeticaModular", 
                number=N_TEST)]

    else:
        raise AttributeError("\nNot a suitable option. Please, choose one of:\n\
            \tMR: Miller-Rabin Benchmark \n\
            \tBPGP: Baby-Pass-Giant-Pass Benchmark\n\
            \tJacobi: Jacobi's Symbol Benchmark\n\
            \tSQRT: Modular Square Roots Benchmark")

    print(results)