
library(data.table)
library(tidyverse)
library(dplyr)
library(stringr)
library(hms)
library(splines)
library(viridis)
library(patchwork)
library(mgcv)
library(grid)
library(effects)
library(lme4)
library(lmerTest)

dir.create("./results/indiv", showWarnings = FALSE)
dir.create("./visualization/indiv", showWarnings = FALSE)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

# Gender scores
df_gender = as.data.frame(fread("../gender_score_new/results/all_tp2/gender_score_combined.tsv"))
df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    glimpse()

# Organ data
df_organ = as.data.frame(fread("../../../UKB/Analyses/clean_organ_imaging/results/organ_tab_clean_wide.tsv"))
organ_mapping = as.data.frame(fread("../../../UKB/Analyses/clean_organ_imaging/results/column_mapping.tsv"))

# Covariates
df_cov = as.data.frame(fread("../../../UKB/tabular/df_body_cov/UKBB_body_cov_wide.tsv"))
df_cov = df_cov %>%
    filter(InstanceID == 2) %>%
    rename(ID = "SubjectID", height = "Standing height_50", assessment_center = "UK Biobank assessment centre_54", weight = "Weight_21002") %>%
    select(ID, assessment_center, height, weight) %>%
    mutate(assessment_center = factor(assessment_center)) %>%
    filter(complete.cases(.)) %>%
    glimpse()

#endregion

#region Associations with gender

# name_field = "10P Liver PDFF (proton density fat fraction)"

assoc_gender = function(name_field) {
    var_mapping = organ_mapping %>% filter(field_name == name_field)
    
    df_lm <<- df_organ %>% 
        select(ID, all_of(name_field)) %>% # Select organ var
        left_join(df_gender, by="ID") %>%
        left_join(df_cov, by="ID") %>%
        rename(Organ = all_of(name_field)) %>%
        filter(complete.cases(.))

    if (nrow(df_lm) < 5) { return("Fewer than 5 subjects with data") }
    if ( is.numeric(df_lm$Organ) == FALSE ) { return("Non-numeric brain variable") }

    # Run linear models
    # Some features were only measured at one assessment center
    if (length(unique(df_lm$assessment_center)) < 2) {
        lm_males = summary(lm(scale(Organ) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + height + weight, data = df_lm %>% filter(Sex == "Male")))
        lm_females = summary(lm(scale(Organ) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + height + weight, data = df_lm %>% filter(Sex == "Female")))
        lm_inter = summary(lm(scale(Organ) ~ Sex + Gender_score + Gender_score*Sex + bs(Age, degree = 3, df = 4) + height + weight, data = df_lm))
        lm_plot = lm(Organ ~ Gender_score * Sex + bs(Age, degree = 3, df = 4) + height + weight, data = df_lm)
    } else {
        lm_males = summary(lmer(scale(Organ) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + height + weight + (1|assessment_center), data = df_lm %>% filter(Sex == "Male")))
        lm_females = summary(lmer(scale(Organ) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + height + weight + (1|assessment_center), data = df_lm %>% filter(Sex == "Female")))
        lm_inter = summary(lmer(scale(Organ) ~ Sex + Gender_score + Gender_score*Sex + bs(Age, degree = 3, df = 4) + height + weight + (1|assessment_center), data = df_lm))
        lm_plot = lmer(Organ ~ Gender_score * Sex + bs(Age, degree = 3, df = 4) + height + weight + (1|assessment_center), data = df_lm)
    }

    # Save results
    results_tmp = data.frame(
        categ_num = var_mapping$categ_num,
        categ_name = var_mapping$categ_name,
        Organ = var_mapping$field_name,
        n_females = length(lm_females$residuals),
        n_males = length(lm_males$residuals),
        n_all = length(lm_inter$residuals),
        coef_males = lm_males$coefficients["scale(Gender_score)","Estimate"],
        pval_males = lm_males$coefficients["scale(Gender_score)","Pr(>|t|)"],
        coef_females = lm_females$coefficients["scale(Gender_score)","Estimate"],
        pval_females = lm_females$coefficients["scale(Gender_score)","Pr(>|t|)"],
        coef_inter = lm_inter$coefficients["SexMale:Gender_score","Estimate"],
        pval_inter = lm_inter$coefficients["SexMale:Gender_score","Pr(>|t|)"],
        coef_sexdiff = lm_inter$coefficients["SexMale", "Estimate"],
        pval_sexdiff = lm_inter$coefficients["SexMale", "Pr(>|t|)"],
        coef_igl_female = lm_inter$coefficients["Gender_score", "Estimate"],
        coef_igl_male = lm_inter$coefficients["Gender_score", "Estimate"] + lm_inter$coefficients["SexMale:Gender_score", "Estimate"]
    )

    return(results_tmp)
}

# Run for all variables
results = list()

i = 1
for (c in 2:length(colnames(df_organ))) {
    print(colnames(df_organ)[c])
    results[[i]] = assoc_gender(colnames(df_organ)[c])
    i = i + 1
}

results = Filter(is.data.frame, results)
results = do.call(rbind, results)

results = results %>% filter(complete.cases(.))

fwrite(results, "./results/results_alltab.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

#endregion
