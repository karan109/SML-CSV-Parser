import pandas as pd
pd.set_option('max_columns', None)
pd.set_option('max_rows', None)
df = pd.read_csv('himym.asv', delimiter = '&')
df2 = pd.read_csv('himym.csv', delimiter = ',')
df3 = pd.read_csv('himym.ssv', delimiter = ';')
df4 = pd.read_csv('himym.vsv', delimiter = '|')
df5 = pd.read_csv('../out.txt', delimiter = ';')
print(df.equals(df2) and df.equals(df3) and df.equals(df4) and df.equals(df5))