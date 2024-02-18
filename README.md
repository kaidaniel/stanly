# Stanly

**:construction: `stanly` is [unfinished](#to-do) and does not work as described here :construction:**

A static analyser for programs using the [pandas](https://pandas.pydata.org) library.

Let's say you want to calculate how much money company made. You start writing the following program:

```python
# file: my_app/main.py
from pandas import read_sql
from my_app.database import con
df = read_sql("sales", con)
# print(df[???].sum())
```

To make the commented-out statement work, you need to replace the `???` with the name of a column giving the price of the item sold. But you might not know how that column is called.

Another function offers a clue:

```python
# file: my_app/sales_by_department.py
from pandas import read_sql
from my_app.database import con
def sales_by_department():
  df = read_sql("sales", con)
  return df.groupby("department")["amount"].sum()
```

If `sales_by_department` is free of bugs, then the "sales" table has at least two columns: "department" and "amount". So the column you are looking for is probably called "amount".

`stanly` collects information like this from anywhere in your program so that you don't have to look for functions like `sales_by_department` yourself:

```console
$ stanly my_app/main.py
`df` on line 5:  df = read_sql("sales", con)
refers to a `pandas.DataFrame` with at least 2 columns:
- department
- amnt
```

## Capabilities

```python
# file: symbolic_variables.py
def f(x):
  df = read_sql("table", con)[x]
  return df

def h(y):
  if (y):
    f("a")

def g(x):
  f(x)

g("b")
```

```console
$ stanly symbolic_variables.py
`df` on line 5: df = read_sql("table", con)[x]
refers to a `pandas.Series` called either of:
- a
- b
assuming `x` refers to either of: `{"a", "b"}`
```

## How it works

`stanly` is an [abstract interpreter](https://en.wikipedia.org/wiki/Abstract_interpretation) extending the idea in [Darais et al., 2017](https://dl.acm.org/doi/abs/10.1145/3110256) with support for dynamic records and special rules covering the [pandas](https://pandas.pydata.org) library.

Input python programs are analysed in steps:

1. **Translation** (`*.py` ⇒ `*.stanly-lang`)  
Over-approximating the input program in a small functional language.  
The over-approximation is intended to be sound assuming the input program raises no exceptions.
2. **Interpretation** (`*.stanly-lang` ⇒ `*.stanly-sema`)  
Calculating the collecting semantics[^1] of the translated program by abstract interpretation.
3. **Summarisation** (`*.stanly-sema` ⇒ output)  
Whenever a `pandas.DataFrame` could be bound to a variable, report an over-approximation of all the possible bound objects.

## To-Do

1. Finish ghc implementation of Darais et al, 2017.
2. Add dictionaries (define/access/delete key, merge) and strings.
3. Write python -> IR lowering.
4. Generalise dictionaries to `dictionary * type-tag` (DataFrame = `dict * "pandas.DataFrame"`).
5. Handle reference variables (`read_sql` returns a reference to external data).
6. Handle incomplete programs (treat function arguments symbolically).
7. Write pandas -> IR lowering.

[^1]: A collecting semantics is a set containing all the results a program might possibly have.  For example, the collecting semantics of the python program `0 if x else 1` with `x` is the set `{0, 1}`.
