
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
from joblib import Parallel, delayed
import multiprocessing as mp

tp = int(sys.argv[1])
impute_m = int(sys.argv[2])
seed_num = int(sys.argv[3])
name_pred = str(sys.argv[4])
name_ana = sys.argv[5]

tmp_dir = f"/dev/shm/parent41/{name_ana}_impdf_{impute_m}_seed_{seed_num}"
out_dir = f"./results/{name_ana}/perm/impdf_{impute_m}_seed_{seed_num}"

shutil.rmtree(out_dir, ignore_errors=True)
shutil.rmtree(tmp_dir, ignore_errors=True)

os.makedirs(out_dir, exist_ok=True)
os.makedirs(tmp_dir, exist_ok=True)

var_mapping = dt.fread("variable_mapping.tsv").to_pandas()

# Load the behavioral data
df_comp = dt.fread(f"../impute_input/results/df_imputed_tp_{tp}_m{impute_m}.tsv").to_pandas()

# Subset with list of included participants
ids = dt.fread(f"results/{name_ana}/id_list.txt").to_pandas()
df = df_comp[df_comp['ID'].isin(ids['ID'])]

ids_all = dt.fread(f"results/{name_ana}/id_list_all.txt").to_pandas()
df_left_out = df_comp[~df_comp['ID'].isin(ids['ID']) & df_comp['ID'].isin(ids_all['ID'])]

print(f"Number of subjects: {df.shape[0]}")
print(f"Left-out subjects in timepoint: {df_left_out.shape[0]}")

# Convert boolean columns to integer (0 or 1)
bool_cols = df.select_dtypes(include=['bool']).columns
df[bool_cols] = df[bool_cols].astype(int)

bool_cols = df_left_out.select_dtypes(include=['bool']).columns
df_left_out[bool_cols] = df_left_out[bool_cols].astype(int)

# Select predictors (X)
if name_pred == "all":
    X = df[df.columns[7:]]
elif name_pred == "diet":
    X = df.loc[:, df.columns.isin(var_mapping.loc[var_mapping.Category == "Diet", "Variable"])]
elif name_pred == "lifestyle":
    X = df.loc[:, df.columns.isin(var_mapping.loc[var_mapping.Category == "Lifestyle", "Variable"])]
elif name_pred == "social":
    X = df.loc[:, df.columns.isin(var_mapping.loc[var_mapping.Category == "Social", "Variable"])]
elif name_pred == "socioeconomic":
    X = df.loc[:, df.columns.isin(var_mapping.loc[var_mapping.Category == "Socioeconomic", "Variable"])]

# Select target (y)
y = df['genetic_sex_22001']

# Initialize and run AutoML
automl = AutoML(mode="Perform",
                train_ensemble=False,
                explain_level=1,
                algorithms=["CatBoost"],
                results_path=tmp_dir,
                features_selection=False,
                golden_features=False,
                validation_strategy={"validation_type":"kfold","k_folds":5,"shuffle":True,"stratify":True,"random_seed":seed_num},
                random_state=seed_num, 
                n_jobs = max(1, mp.cpu_count() // 4))
automl.fit(X, y)

y = y.map({'Male': 1, 'Female': 0})

# Select best model
automl_models = dt.fread(f"{tmp_dir}/leaderboard.csv").to_pandas()
automl_models = automl_models[automl_models['model_type'] == "CatBoost"]
best_model = automl_models.loc[automl_models['metric_value'].idxmin(), 'name']

print(f"Best model = {best_model}")

with open(f'{tmp_dir}/{best_model}/framework.json', 'r') as file:
    best_model_param = json.load(file)

best_model_param = best_model_param['params']

# Copy-paste out-of-fold predictions and models from best model
shutil.copy(f"{tmp_dir}/{best_model}/predictions_out_of_folds.csv", f"{out_dir}/predictions_out_of_folds.csv")
shutil.copy(f"{tmp_dir}/{best_model}/learner_fold_0.catboost", f"{out_dir}/")
shutil.copy(f"{tmp_dir}/{best_model}/learner_fold_1.catboost", f"{out_dir}/")
shutil.copy(f"{tmp_dir}/{best_model}/learner_fold_2.catboost", f"{out_dir}/")
shutil.copy(f"{tmp_dir}/{best_model}/learner_fold_3.catboost", f"{out_dir}/")
shutil.copy(f"{tmp_dir}/{best_model}/learner_fold_4.catboost", f"{out_dir}/")

# Load CatBoost models
model_files = [f for f in os.listdir(f"{out_dir}") if f.endswith(".catboost")]
model_files.sort(key=lambda x: int(x.split('_')[-1].split('.')[0]))

# Get prediction probabilities for excluded participants in downsampling step
# Final prob = average of the 5 models from the CV

left_out_predictions = {idx: [] for idx in df_left_out['ID']}

if name_pred == "all":
    X_left_out = df_left_out[df_left_out.columns[7:]]
elif name_pred == "diet":
    X_left_out = df_left_out.loc[:, df_left_out.columns.isin(var_mapping.loc[var_mapping.Category == "Diet", "Variable"])]
elif name_pred == "lifestyle":
    X_left_out = df_left_out.loc[:, df_left_out.columns.isin(var_mapping.loc[var_mapping.Category == "Lifestyle", "Variable"])]
elif name_pred == "social":
    X_left_out = df_left_out.loc[:, df_left_out.columns.isin(var_mapping.loc[var_mapping.Category == "Social", "Variable"])]
elif name_pred == "socioeconomic":
    X_left_out = df_left_out.loc[:, df_left_out.columns.isin(var_mapping.loc[var_mapping.Category == "Socioeconomic", "Variable"])]

y_left_out = df_left_out['genetic_sex_22001']

for m, model_file in enumerate(model_files):
    print(f"Fold {m+1}")
    
    # Load model
    print(f"Load model")
    model = CatBoostClassifier()
    model.load_model(os.path.join(f"{out_dir}", model_file))
    pool = Pool(X, label=y)
    
    # Predict excluded participants
    preds = model.predict_proba(X_left_out)[:, 1]  # Probability of positive class
    for i, idx in enumerate(df_left_out['ID']):
        left_out_predictions[idx].append(preds[i])

# Average predictions across folds
avg_preds_left_out = {idx: np.mean(preds) for idx, preds in left_out_predictions.items()}

# Combine results
results_left_out = pd.DataFrame({
        'ID': list(avg_preds_left_out.keys()),
        'pred': list(avg_preds_left_out.values()),
        'train': 'left_out'
    }).sort_values('ID').reset_index(drop=True)

results_balanced = dt.fread(f"{out_dir}/predictions_out_of_folds.csv").to_pandas()
results_balanced = pd.DataFrame({
        'ID': list(df['ID']),
        'pred': list(results_balanced['prediction_0_for_Female_1_for_Male']),
        'train': 'train'
    }).sort_values('ID').reset_index(drop=True)

results = pd.concat([results_balanced, results_left_out]).sort_values('ID').reset_index(drop=True)
results.to_csv(f"{out_dir}/all_predictions.csv", sep=",", header=True, index=False)

# Interpretable ML

def process_fold(m, model_file, X, y, folds, df, out_dir):
    print(f"Fold {m+1}")
    
    # Load model
    print(f"Load model")
    model = CatBoostClassifier()
    model.load_model(os.path.join(f"{out_dir}", model_file))
    
    # Select out-of-fold predictions
    X_val = X.iloc[folds[m],:]
    y_val = y.iloc[folds[m]]
    pool = Pool(X_val, label=y_val)
    
    # Feature interactions
    print(f"Feature interactions")
    interactions_tmp = model.get_feature_importance(type="Interaction", data=Pool(X_val, label=y_val))
    interactions_tmp = pd.DataFrame(interactions_tmp, columns=['Feature1', 'Feature2', 'Score'])
    interactions_tmp = interactions_tmp.sort_values(by="Score", ascending=False)
    interactions_tmp['Feature1'] = [X.columns[int(i)] for i in interactions_tmp['Feature1']]
    interactions_tmp['Feature2'] = [X.columns[int(i)] for i in interactions_tmp['Feature2']]
    interactions_tmp.to_csv(f'{out_dir}/interactions_fold{m+1}.tsv', sep="\t", na_rep="NA", header=True, index=False)
    
    # Shap values
    print(f"Shap values")
    shap_model = shap.TreeExplainer(model, data = shap.sample(X_val, 10), model_output='probability')
    shap_exp_value = pd.DataFrame({"expected_value": [shap_model.expected_value]})
    shap_exp_value.to_csv(f'{out_dir}/shap_expectedval_fold{m+1}.tsv', sep="\t", na_rep="NA", header=True, index=False)
    shap_prob = shap_model.shap_values(X_val, check_additivity=False)
    shap_prob = pd.DataFrame(shap_prob, columns=list(X.columns))
    shap_prob['ID'] = df['ID'].iloc[folds[m]].reset_index(drop=True)
    shap_prob.to_csv(f'{out_dir}/shap_prob_fold{m+1}.tsv', sep="\t", na_rep="NA", header=True, index=False)
    
    print(f"Shap values on held out data")
    shap_model = shap.TreeExplainer(model, data = shap.sample(X_left_out, 10), model_output='probability')
    shap_prob = shap_model.shap_values(X_left_out, check_additivity=False)
    shap_prob = pd.DataFrame(shap_prob, columns=list(X_left_out.columns))
    shap_prob['ID'] = df_left_out['ID'].reset_index(drop=True)
    shap_prob.to_csv(f'{out_dir}/shap_prob_fold{m+1}_heldout.tsv', sep="\t", na_rep="NA", header=True, index=False)

# Load fold indices
folds = [np.load(f'{tmp_dir}/folds/fold_{i}_validation_indices.npy') for i in range(0, 5)]

n_jobs = 5

results = Parallel(n_jobs=n_jobs, backend='loky')(
    delayed(process_fold)(m, model_file, X, y, folds, df, out_dir)
    for m, model_file in enumerate(model_files)
)
