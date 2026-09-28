
library(data.table)
library(effsize)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(forcats)
library(cowplot)
library(patchwork)
library(effects)
library(grid)
library(ggplotify)
library(splines)

dir.create("./visualization/indiv_cov_birth", showWarnings=FALSE)
dir.create("./visualization/shap_indiv_cov_birth", showWarnings=FALSE)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

# Load
df_raw = as.data.frame(fread("../impute_input/results/df_imputed_tp_all_combined.tsv"))
df_raw = df_raw %>%
    filter(instanceID == 0) %>%
    select(ID, Date_of_attending_assessment_centre_53_0) %>%
    rename(Date_assessment_centre = "Date_of_attending_assessment_centre_53_0") %>%
    glimpse()

firstocc = as.data.frame(fread("../../../UKB/Analyses/clean_firstocc/results/firstocc_ses0_final.tsv"))
firstocc = firstocc %>%
    mutate(
        DX_date = as.Date(DX_date),
        chapter = factor(chapter),
        categ = factor(categ)
    ) %>%
    select(-c(Sex, Year_birth, Month_birth, date_birth, age_at_dx)) %>%
    glimpse()

df_live_births = as.data.frame(fread("../../../UKB/tabular/df_live_births/UKBB_live_births_wide.tsv"))
df_live_births = df_live_births %>%
    rename(ID = "SubjectID", number_live_births_2734 = "Number of live births_2734") %>%
    filter(InstanceID == 0) %>%
    select(c(ID, number_live_births_2734)) %>%
    glimpse()

df_gender = as.data.frame(fread("../gender_score_new/results/all_tp0/gender_score_combined.tsv"))
df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    left_join(df_raw, by="ID") %>%
    left_join(df_live_births, by="ID") %>%
    glimpse()

var_df = as.data.frame(fread("../gender_score_new/variable_mapping.tsv"))

#endregion

#region Associations between gender and diagnoses

dx_list = unique(firstocc %>% filter(chapter == "Pregnancy, childbirth and the puerperium") %>% pull(icd_code))
dx_list <- dx_list[dx_list != ""]

results = list()
i=1
for (d in 1:length(dx_list)) {

    print(d)

    dx_icd = dx_list[d]
    dx_name = unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(dx_name))
    dx_chapter = as.character(unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(chapter)))
    dx_categ = as.character(unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(categ)))
    dx_n = sum((df_gender$ID %in% (firstocc %>% filter(icd_code == dx_icd) %>% pull(ID))) == TRUE)
    
    print(dx_name)

    if (dx_n > 5) {

        df_dx = firstocc %>%
            filter(icd_code == dx_icd) %>%
            left_join(df_gender, by="ID") %>%
            mutate(days_gender_dx = as.numeric(difftime(as.Date(DX_date), as.Date(Date_assessment_centre), units = "days"))) %>% 
            filter(complete.cases(.)) %>%
            mutate(Group = "DX") %>% 
            select(ID, Date_assessment_centre, Sex, Age, Gender_score, Group, number_live_births_2734)
        
        ids_not_controls = unique(firstocc %>% filter(chapter == dx_chapter) %>% pull(ID))

        df_hc = df_gender %>%
            filter(!ID %in% ids_not_controls) %>%
            mutate(Group = "HC") %>%
            select(ID, Date_assessment_centre, Sex, Age, Gender_score, Group, number_live_births_2734)

        df_all = rbind(df_dx, df_hc)
        df_all$Group = factor(df_all$Group, levels=c("HC", "DX"))

        # Female
        if (length(df_dx %>% filter(Sex == "Female") %>% pull(ID)) > 5) {
            lm_females = glm(Group ~ Gender_score + Age + number_live_births_2734, data = df_all %>% filter(Sex == "Female"), family = binomial)
            lm_females_coef = summary(lm_females)$coefficients
            lm_females_or = as.data.frame(exp(cbind(OR = coef(lm_females), confint(lm_females))))

        } else {
            lm_females_coef = matrix(data=NA, nrow=4, ncol=4)
            lm_females_or = as.data.frame(matrix(data=NA, nrow=4, ncol=3))
            colnames(lm_females_or) = c("OR", "2.5 %", "97.5 %")
        }

        # Male
        if (length(df_dx %>% filter(Sex == "Male") %>% pull(ID)) > 5) {
            lm_males = glm(Group ~ Gender_score + Age + number_live_births_2734, data = df_all %>% filter(Sex == "Male"), family = binomial)
            lm_males_coef = summary(lm_males)$coef
            lm_males_or = as.data.frame(exp(cbind(OR = coef(lm_males), confint(lm_males))))

        } else {
            lm_males_coef = matrix(data=NA, nrow=4, ncol=4)
            lm_males_or = as.data.frame(matrix(data=NA, nrow=4, ncol=3))
            colnames(lm_males_or) = c("OR", "2.5 %", "97.5 %")
        }

        # Save results
        results[[i]] = data.frame(
            icd_code = dx_icd,
            diagnosis = dx_name,
            chapter = dx_chapter,
            categ = dx_categ,
            n_dx_males = nrow(df_all %>% filter(Sex == "Male" & Group == "DX")),
            n_dx_females = nrow(df_all %>% filter(Sex == "Female" & Group == "DX")),
            n_hc_males = nrow(df_all %>% filter(Sex == "Male" & Group == "HC")),
            n_hc_females = nrow(df_all %>% filter(Sex == "Female" & Group == "HC")),
            coef_males = lm_males_coef[2,1],
            pval_males = lm_males_coef[2,4],
            or_males = lm_males_or[2,1],
            or_males_lower = lm_males_or[2,2],
            or_males_upper = lm_males_or[2,3],
            coef_females = lm_females_coef[2,1],
            pval_females = lm_females_coef[2,4],
            or_females = lm_females_or[2,1],
            or_females_lower = lm_females_or[2,2],
            or_females_upper = lm_females_or[2,3]
        )

        i = i + 1
    }
}

# Save
results = do.call(rbind, results)
fwrite(results, "./results/results_dx_glm_cov_births.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

print("DONE!")

#endregion

#region Associations between Black IGL and diagnoses

gender_list = c("eth_white_tp0_match", "eth_black_tp0_match", "eth_asian_tp0_match")

# Load data

df_gender = list()
for (g in 1:length(gender_list)) {
    df_gender[[g]] = as.data.frame(fread(paste0("../pred_other_models/results/preds_",gender_list[g],"/preds_combined.tsv")))
}
df_gender = do.call(rbind, df_gender)

df_gender_tp0 = as.data.frame(fread("../gender_score_new/results/all_tp0/gender_score_combined.tsv"))
df_gender_tp0 = df_gender_tp0 %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    mutate(train = "0", Probability_male_sd = "0", model="all_tp0") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    select(colnames(df_gender)) %>%
    glimpse()

df_gender = rbind(df_gender, df_gender_tp0)

df_gender = df_gender %>%
    arrange(ID, model) %>%
    distinct(ID, model, .keep_all = TRUE) %>%
    mutate(
        model = factor(model, levels=c("all_tp0", gender_list)),
        Sex = factor(Sex)
    ) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    select(ID, Age, Sex, model, Gender_score) %>%
    left_join(df_live_births, by="ID") %>%
    filter(model == "eth_black_tp0_match") %>%
    glimpse()

# Run associations for black IGL

dx_list = unique(firstocc %>% filter(chapter == "Pregnancy, childbirth and the puerperium") %>% pull(icd_code))
dx_list <- dx_list[dx_list != ""]

results = list()
i=1
for (d in 1:length(dx_list)) {

    print(d)

    dx_icd = dx_list[d]
    dx_name = unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(dx_name))
    dx_chapter = as.character(unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(chapter)))
    dx_categ = as.character(unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(categ)))
    dx_n = sum((df_gender$ID %in% (firstocc %>% filter(icd_code == dx_icd) %>% pull(ID))) == TRUE)
    
    print(dx_name)

    if (dx_n > 5) {

        df_dx = firstocc %>%
            filter(icd_code == dx_icd) %>%
            left_join(df_gender, by="ID") %>%
            filter(complete.cases(.)) %>%
            mutate(Group = "DX") %>% 
            select(ID, Sex, Age, Gender_score, Group, number_live_births_2734)
        
        ids_not_controls = unique(firstocc %>% filter(chapter == dx_chapter) %>% pull(ID))

        df_hc = df_gender %>%
            filter(!ID %in% ids_not_controls) %>%
            mutate(Group = "HC") %>%
            select(ID, Sex, Age, Gender_score, Group, number_live_births_2734)

        df_all = rbind(df_dx, df_hc)
        df_all$Group = factor(df_all$Group, levels=c("HC", "DX"))

        # Female
        if (length(df_dx %>% filter(Sex == "Female") %>% pull(ID)) > 5) {
            lm_females = glm(Group ~ Gender_score + Age + number_live_births_2734, data = df_all %>% filter(Sex == "Female"), family = binomial)
            lm_females_coef = summary(lm_females)$coefficients
            lm_females_or = as.data.frame(exp(cbind(OR = coef(lm_females), confint(lm_females))))

        } else {
            lm_females_coef = matrix(data=NA, nrow=4, ncol=4)
            lm_females_or = as.data.frame(matrix(data=NA, nrow=4, ncol=3))
            colnames(lm_females_or) = c("OR", "2.5 %", "97.5 %")
        }

        # Male
        if (length(df_dx %>% filter(Sex == "Male") %>% pull(ID)) > 5) {
            lm_males = glm(Group ~ Gender_score + Age + number_live_births_2734, data = df_all %>% filter(Sex == "Male"), family = binomial)
            lm_males_coef = summary(lm_males)$coef
            lm_males_or = as.data.frame(exp(cbind(OR = coef(lm_males), confint(lm_males))))

        } else {
            lm_males_coef = matrix(data=NA, nrow=4, ncol=4)
            lm_males_or = as.data.frame(matrix(data=NA, nrow=4, ncol=3))
            colnames(lm_males_or) = c("OR", "2.5 %", "97.5 %")
        }

        # Save results
        results[[i]] = data.frame(
            icd_code = dx_icd,
            diagnosis = dx_name,
            chapter = dx_chapter,
            categ = dx_categ,
            n_dx_males = nrow(df_all %>% filter(Sex == "Male" & Group == "DX")),
            n_dx_females = nrow(df_all %>% filter(Sex == "Female" & Group == "DX")),
            n_hc_males = nrow(df_all %>% filter(Sex == "Male" & Group == "HC")),
            n_hc_females = nrow(df_all %>% filter(Sex == "Female" & Group == "HC")),
            coef_males = lm_males_coef[2,1],
            pval_males = lm_males_coef[2,4],
            or_males = lm_males_or[2,1],
            or_males_lower = lm_males_or[2,2],
            or_males_upper = lm_males_or[2,3],
            coef_females = lm_females_coef[2,1],
            pval_females = lm_females_coef[2,4],
            or_females = lm_females_or[2,1],
            or_females_lower = lm_females_or[2,2],
            or_females_upper = lm_females_or[2,3]
        )

        i = i + 1
    }
}

# Save
results = do.call(rbind, results)
fwrite(results, "./results/results_dx_glm_black_cov_births.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

print("DONE!")

#endregion
