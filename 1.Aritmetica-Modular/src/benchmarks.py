import pandas as pd
from timeit import timeit
from sys import argv
from AritmeticaModular import *


N_TEST = 5

cases = [46381, 768479, 9476407, 36780481, 562390847, 1894083629, 65398261921,
         364879542899, 8590365927553, 28564333765949, 12345678910111]
explist = [1749924,1749924,1749925,1749925,1749926,1749924,1749925,1749925]

def create_empty_df():
    return pd.DataFrame(columns={'Case', 'Time'}, dtype=(int, float))


def MR_Benchamrk():

    print("Ejecución del Test de Miller Rabin")

    results = create_empty_df()

    for i in range(len(cases)):

        results.loc[i] = [str(cases[i]), timeit('AritmeticaModular.miller_rabin_test(' + str(cases[i]) + ')',
                                           setup="import AritmeticaModular", number=N_TEST) / N_TEST]

    return results


def BSGS_Benchmark():

    print("Ejecución del Baby Pass - Giant Pass")

    results = create_empty_df()
    for i in range(len(explist)):
        results.loc[i] = [str(cases[i]), timeit('AritmeticaModular.baby_step_giant_step(123456,' + 
                                            str(explist[i] % cases[i]) + ',' + str(cases[i]) + 
                                            ')', setup="import AritmeticaModular", number=N_TEST)]

    return results


def Jacobi_Benchmark():

    print("Ejecución del símbolo de Jacobi")

    results = create_empty_df()

    for i in range(len(cases)):
        results.loc[i] = [str(cases[i]), timeit('AritmeticaModular.Jacobi(1749924, '
                                           + str(cases[i]) + ')', setup="import AritmeticaModular", number=N_TEST)]

    print(results)

    return results


def ModSqrt_Benchmark():

    print("Ejecución de las raíces modulares")

    results = create_empty_df()
    reverse_cases = cases[::-1]
    for i in range(len(cases)):
        results.loc[i] = [str(cases[i]), timeit('AritmeticaModular.sqrts_mod_n(123456, '
                                           + str(cases[i]) + ', ' +
                                           str(reverse_cases[i]) + ')',
                                           setup="import AritmeticaModular", number=N_TEST)]

    return results

def Fermat_Benchmark():

    print("Ejecución del test de fermat")

    results = create_empty_df()

    for i in range(5):
        print("case ", i)
        results.loc[i] = [str(cases[i]), timeit('AritmeticaModular.Fermat('+ str(cases[i] + 1) + ')',
                                           setup="import AritmeticaModular", number=N_TEST)]

    return results

def Pollard_Benchmark():

    print("Ejecución de las raíces modulares")

    results = create_empty_df()

    for i in range(len(cases)):
        print("caso =",i)
        results.loc[i] = [str(cases[i]), timeit('AritmeticaModular.ρ_Pollard('+ str(cases[i]+1) + ')',
                                           setup="import AritmeticaModular", number=N_TEST)]

    return results

    # print(ggplot(results, aes(x="Case", weight="Time")) + geom_bar())


if __name__ == "__main__":
    if len(argv) != 2:
        raise AttributeError("Use: python main.py \"BENCHMARK\"")

    if argv[1] == "MR":
        df = MR_Benchamrk()   

    elif argv[1] == "BSGS":
        df = BSGS_Benchmark()

    elif argv[1] == "Jacobi":
        df = Jacobi_Benchmark()

    elif argv[1] == "SQRT":
        df = ModSqrt_Benchmark()

    elif argv[1] == "Fermat":
        df = Fermat_Benchmark()

    elif argv[1] == "Pollard":
        df = Pollard_Benchmark()
        

    else:
        raise AttributeError("\nNot a suitable option. Please, choose one of:\n\
            \tMR: Miller-Rabin Benchmark \n\
            \tBSGS: Baby-Pass-Giant-Pass Benchmark\n\
            \tJacobi: Jacobi's Symbol Benchmark\n\
            \tSQRT: Modular Square Roots Benchmark")

    df.to_csv(
            index=False, path_or_buf='../1.Aritmetica-Modular/' + argv[1] + '.csv')