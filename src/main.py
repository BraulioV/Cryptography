import pandas as pd
from timeit import timeit
from sys import argv
from AritmeticaModular import *

from ggplot import *

N_TEST = 1000

if __name__ == "__main__":
    if len(argv) != 2:
        raise AttributeError("Use: python main.py \"BENCHMARK\"")
    
    cases = ['46381', '768479', '9476407', '36780481', '562390847', '1894083629', '65398261921',
             '364879542899', '8590365927553', '28564333765949', '12345678910111', ]

    # results = pd.DataFrame(columns = {'Case', 'Time'}, dtype=(int, float))
    results = pd.DataFrame(columns={'Case', 'Time'})
    if argv[1] == "MR":
        print("Ejecución del Test de Miller Rabin")
    
        for i in range(len(cases)):

            results.loc[i] = [cases[i], timeit('AritmeticaModular.miller_rabin_test('+ cases[i] + ')',
                setup="import AritmeticaModular", number=N_TEST)/N_TEST]
    
    elif argv[1] == "BPGP":
        print("Ejecución del Baby Pass - Giant Pass")
        for i in range(3):
            results.loc[i] = [cases[i], timeit('AritmeticaModular.baby_pass_giant_pass(123456, 1749924,' +
             cases[i] +')', setup="import AritmeticaModular", number=N_TEST)]
    
    elif argv[1] == "Jacobi":
        print("Ejecución del símbolo de Jacobi")
        for i in range(len(cases)):
            results.loc[i] = [cases[i], timeit('AritmeticaModular.Jacobi(1749924, '
                + cases[i] + ')', setup="import AritmeticaModular", number=N_TEST)]
    
    elif argv[1] == "SQRT":
        print("Ejecución de las raíces modulares")

        for i in range(len(cases)):
            results.loc[i] = [cases[i], timeit('AritmeticaModular.Jacobi(123456, ' + cases[i] + ', '
                + cases[-(i+1)] + ')', setup="import AritmeticaModular",
                number=N_TEST)]
    
    else:
        raise AttributeError("\nNot a suitable option. Please, choose one of:\n\
            \tMR: Miller-Rabin Benchmark \n\
            \tBPGP: Baby-Pass-Giant-Pass Benchmark\n\
            \tJacobi: Jacobi's Symbol Benchmark\n\
            \tSQRT: Modular Square Roots Benchmark")
    print(results)

    print(ggplot(aes(x='Case', y='Time'), results) + geom_bar() + theme(axis_text_x = element_text(angle = 310)))