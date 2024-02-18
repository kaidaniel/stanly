# Stanly

A static analyser for programs using the [`pandas`](https://pandas.pydata.org) library.

Let's say you execute the following program:

```python
# file: my_app/main.py
from pandas import read_sql
from my_app.database import con
# `read_sql` downloads lots of data and may take a long time. 
df = read_sql("sales", con)
print(df["amount"].sum())
```

, and say it results in:

```python
Traceback (most recent call last):
  ...
KeyError: 'amount'
```

To fix the error, you need to know which columns are defined for `df`.

Another function offers a clue:

```python
# file: my_app/sales_by_department.py
from pandas import read_sql
from my_app.database import con
def sales_by_department():
  df = read_sql("sales", con)
  return df.groupby("department")["amnt"].sum()
```

If `sales_by_department` is free of bugs, then the "sales" table has at least two columns: "department" and "amnt". So the bug in `my_app/main.py` is probably
that we spelled the word "amount" different from how it is spelled in the "sales" table.

`stanly` collects information like this from anywhere in your program so that you don't have to look for functions like `sales_by_department` yourself:

```console
$ stanly my_app/main.py
12  df = read_sql("sales", con)
variable `df` is a `pandas.DataFrame` with at least 2 columns:
department, amnt
```

## To-Do

1. Finish ghc implementation of Darais et al, 2017.
2. Add dictionaries (define/access/delete key, merge) and strings.
3. Write python -> IR lowering.
4. Generalise dictionaries to DataFrames (dictionary + type tag).
5. Handle externally defined dictionaries (e.g. modeling external tables).
6. Write pandas -> IR lowering.
