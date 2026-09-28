
from supervised.automl import AutoML
import pandas as pd
import numpy as np
from sklearn.metrics import accuracy_score, roc_auc_score
import argparse
import polars as pl
import datatable as dt
import sys
import os
import shutil
from sklearn.utils import resample
import matplotlib.pyplot as plt
from sklearn.model_selection import KFold
from sklearn.inspection import PartialDependenceDisplay, partial_dependence, permutation_importance
from catboost import CatBoostClassifier, Pool
import shap
import json

gender_list = [
    "all_tp0", "all_tp1", "all_tp2", "all_tp3",
    # Match
    "all_tp0_match", "all_tp1_match", "all_tp2_match", "all_tp3_match",
    "eth_white_tp0_match", "eth_black_tp0_match", "eth_asian_tp0_match", "eth_chinese_tp0_match", "eth_southasian_tp0_match", "eth_african_tp0_match", "eth_caribbean_tp0_match",
    "gen_silent_tp0_match", "gen_boom1_tp0_match", "gen_boom2_tp0_match", "gen_genx_tp0_match",
    "income_1_tp0_match", "income_2_tp0_match", "income_3_tp0_match", "income_4_tp0_match", "income_5_tp0_match"             
    ]

tp_list = [
    0,1,2,3,
    # Match
    0,1,2,3,
    0,0,0,0,0,0,0,
    0,0,0,0,
    0,0,0,0,0
]

# Load the behavioral data
df = dt.fread("../impute_input/results/df_imputed_tp_all_combined.tsv").to_pandas()
ids_tp0 = df[df['instanceID'] == 0]['ID']

# For every gender score
for g in range(len(gender_list)):
    print(gender_list[g])
    os.makedirs(f'./results/preds_{gender_list[g]}', exist_ok=True)
    os.makedirs(f'./results/preds_{gender_list[g]}/perm', exist_ok=True)
    
    # Select IDs NOT used in gender score
    ids_gender = dt.fread(f"../gender_score_new/results/{gender_list[g]}/id_list.txt").to_pandas()
    ids_not_gender = ids_tp0[~ids_tp0.isin(ids_gender["ID"])]
    
    # For every imputed dataset, fold, and seed...
    for impdf in range(1,6):
        df_tmp = dt.fread(f"../impute_input/results/df_imputed_tp_0_m{impdf}.tsv").to_pandas()
        df_tmp = df_tmp[df_tmp["ID"].isin(ids_not_gender)]
        bool_cols = df_tmp.select_dtypes(include=['bool']).columns
        df_tmp[bool_cols] = df_tmp[bool_cols].astype(int)
        
        for fold in range(0,5):
            
            for s in range(1,6):
                
                # Load model trained on ppl with specific ethnicity
                print(f'Fold {fold+1} - Impdf {impdf} - Seed {s}')
                model = CatBoostClassifier()
                model.load_model(os.path.join(f"../gender_score_new/results/{gender_list[g]}/perm/impdf_{impdf}_seed_{s}", f"learner_fold_{fold}.catboost"))
                idx_male = list(model.classes_).index(1)
                X_tmp = df_tmp[df_tmp.columns[7:]]
                y_tmp = df_tmp['sex_31']
                y_tmp = y_tmp.map({'Male': 1, 'Female': 0})
                pool = Pool(X_tmp, label=y_tmp)
                preds = model.predict_proba(pool)
                preds = pd.DataFrame({'ID': df_tmp['ID'], 'target': y_tmp, 'prediction_0_for_Female_1_for_Male': preds[:,idx_male]})
                preds.to_csv(f"./results/preds_{gender_list[g]}/perm/preds_fold{fold}_impdf{impdf}_seed{s}.tsv", sep="\t", na_rep="NA", header=True, index=False)

