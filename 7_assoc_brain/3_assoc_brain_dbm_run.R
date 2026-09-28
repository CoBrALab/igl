
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(Metrics)
library(RColorBrewer)
library(pROC)
library(caret)
library(PRROC)
library(grid)
library(RMINC)
library(MRIcrotome)
library(parallel)
library(splines)

args = commandArgs(trailingOnly=TRUE)

name_ana = args[1]
print(name_ana)

#region Load data

# Inclusions
inclusions = as.data.frame(fread("../../QC/inclusions_dbm.txt"))
colnames(inclusions) = "ID"

# DBM
dbm_path_rel = as.data.frame(list.files(path="../../../UKB/DBM_2mm", pattern="*_ses-2_jacobians_rel_2mm.mnc", full.names=TRUE, recursive=TRUE))
dbm_path_abs = as.data.frame(list.files(path="../../../UKB/DBM_2mm", pattern="*_ses-2_jacobians_abs_2mm.mnc", full.names=TRUE, recursive=TRUE))
df_brain = cbind(dbm_path_rel, dbm_path_abs)

colnames(df_brain) = c("dbm_path_rel", "dbm_path_abs")

rm(dbm_path_rel, dbm_path_abs)

# Confounds data
df_confounds = as.data.frame(fread("../../../UKB/Analyses/clean_brain_tab/results/brain_tab_clean_wide.tsv"))
brain_mapping = as.data.frame(fread("../../../UKB/Analyses/clean_brain_tab/results/column_mapping.tsv"))

# Gender scores
df_gender = as.data.frame(fread("../gender_score_new/results/all_tp2/gender_score_combined.tsv"))

# Assessment center
df_center = as.data.frame(fread("../../../UKB/tabular/df_body_cov/UKBB_body_cov_wide.tsv"))

#endregion

#region Clean data

# Confounds

list_confound_vars = c(
        "24419", #Measure of head motion in T1 structural image
        "25756", #Scanner lateral (X) brain position
        "25757", #Scanner transverse (Y) brain position
        "25758", #Scanner longitudinal (Z) brain position
        "25759", #Scanner table position
        "25000" #Volumetric scaling from T1 head image to standard space
    )

df_confounds = df_confounds %>% select(ID, all_of(list_confound_vars))

# Brain data

df_brain = df_brain %>% 
    mutate(ID=str_extract(dbm_path_rel, "(?<=sub-)[^_]+")) %>%
    select(c(ID, dbm_path_rel, dbm_path_abs)) %>%
    filter(!grepl("ses-3", dbm_path_rel)) %>%
    glimpse()

# Gender scores

df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    glimpse()

# Assessment center
df_center = df_center %>%
    filter(InstanceID == 2) %>%
    rename(ID = "SubjectID", height = "Standing height_50", assessment_center = "UK Biobank assessment centre_54", weight = "Weight_21002") %>%
    select(ID, assessment_center, height, weight) %>%
    mutate(assessment_center = factor(assessment_center)) %>%
    filter(complete.cases(.)) %>%
    glimpse()

#endregion

#region Run models

# Merge dataframes
df_gender = df_gender %>% 
    left_join(df_confounds, by="ID") %>%
    left_join(df_center, by="ID") %>%
    rename(
        motion_t1 = "24419",
        scanner_x = "25756",
        scanner_y = "25757",
        scanner_z = "25758",
        scanner_pos = "25759"
    ) %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male"))) %>%
    glimpse()

df_brain_gender = merge(df_gender, df_brain, by="ID")
df_brain_gender = merge(inclusions, df_brain_gender, by="ID")

df_brain_gender = df_brain_gender %>% filter(complete.cases(.))

rm(df_gender, df_brain, df_confounds, df_center, brain_mapping, list_confound_vars, inclusions)

# Run models

# Relative jacobians
if (name_ana == "rel_males") {
    lm_model = mincLmer(dbm_path_rel ~ Gender_score + bs(Age, degree = 3, df = 4) + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), mask = "../../../UKB/temporary_template/Mask_2mm.mnc", data = df_brain_gender %>% filter(Sex == "Male"))
}
if (name_ana == "rel_females") {
    lm_model = mincLmer(dbm_path_rel ~ Gender_score + bs(Age, degree = 3, df = 4) + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), mask = "../../../UKB/temporary_template/Mask_2mm.mnc", data = df_brain_gender %>% filter(Sex == "Female"))
}
if (name_ana == "rel_inter") {
    lm_model = mincLmer(dbm_path_rel ~ Gender_score*Sex + bs(Age, degree = 3, df = 4) + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), mask = "../../../UKB/temporary_template/Mask_2mm.mnc", data = df_brain_gender)
}

# Absolute jacobians
if (name_ana == "abs_males") {
    lm_model = mincLmer(dbm_path_abs ~ Gender_score + bs(Age, degree = 3, df = 4) + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), mask = "../../../UKB/temporary_template/Mask_2mm.mnc", data = df_brain_gender %>% filter(Sex == "Male"))
}
if (name_ana == "abs_females") {
    lm_model = mincLmer(dbm_path_abs ~ Gender_score + bs(Age, degree = 3, df = 4) + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), mask = "../../../UKB/temporary_template/Mask_2mm.mnc", data = df_brain_gender %>% filter(Sex == "Female"))
}
if (name_ana == "abs_inter") {
    lm_model = mincLmer(dbm_path_abs ~ Gender_score*Sex + bs(Age, degree = 3, df = 4) + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), mask = "../../../UKB/temporary_template/Mask_2mm.mnc", data = df_brain_gender)
}

lm_model = mincLmerEstimateDF(lm_model)
saveRDS(lm_model, file = paste0("./results/lm_dbm_",name_ana,".rds"))

#endregion
