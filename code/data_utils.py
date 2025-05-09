from config import *
import pandas as pd

def load_targets():
    dfs = []
    for i in SOURCE_DICT.keys():
        df_target = pd.read_csv(f'https://raw.githubusercontent.com/KITmetricslab/RESPINOW-Hub/main/data/{SOURCE_DICT[i]}/{i}/target-{SOURCE_DICT[i]}-{i}.csv')
        df_target = df_target.rename(columns={'value': 'target'})
        df_target['source'] = SOURCE_DICT[i]
        df_target['disease'] = i
        dfs.append(df_target)
    return pd.concat(dfs, ignore_index=True)

def add_target(df):
    df_target = load_targets()
    df_combined = df.merge(df_target, how='left', 
                           left_on= ['source', 'disease', 'location', 'age_group', 'target_end_date'], 
                           right_on=['source', 'disease', 'location', 'age_group', 'date'])
    return df_combined

def add_median(df):
    df_median = df[df['quantile'] == 0.5].copy()
    df_median['type'] = 'median'
    return pd.concat([df, df_median], ignore_index=True)

def load_submissions(include_target=True, include_median=True):
    df = pd.read_csv('../data/submissions.csv')
    if include_target:
        df = add_target(df)
    if include_median:
        df = add_median(df)
    return df