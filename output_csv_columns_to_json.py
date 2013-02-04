import json
import numpy as np
import pandas as pd
import re

SAVE_DIR_NAME = ''

df = pd.read_csv('table1.csv')

col_names = list(df.columns)
ref_col = 'date'

for col_name in col_names:
	
	pairs = []
	col_vals = list(df[col_name])
	ref_vals = list(df[ref_col])
	for ref_val in ref_vals:
        pair_val = col_vals[ref_vals.index(ref_val)]
        pairs.append([ref_val, int(pair_val)])

    f_name = re.sub(r'\s+', '_', col_name.lower())
    f_name = re.sub(r",|'|\.", '', f_name)
    f = open(SAVE_DIR_NAME + f_name + '.js', 'w')
    f.write(json.dumps(dates_values))
    f.close()