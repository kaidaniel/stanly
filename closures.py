import pandas as pd

from module_a.submodule_b import ab_f, ab_g

def f(df, x, y: int, f): pd.DataFrame:
  return lambda g: g(df(f))

main_g = f(ab_g(), 1, "xyz", ab_f)

df = main_g(ab_g)(123)
