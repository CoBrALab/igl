
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

dir.create("./visualization/glm_ethnicity", showWarnings=FALSE)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

# First occurences
firstocc = as.data.frame(fread("../../../UKB/Analyses/clean_firstocc/results/firstocc_ses0_final.tsv"))
firstocc = firstocc %>%
    mutate(
        DX_date = as.Date(DX_date),
        chapter = factor(chapter),
        categ = factor(categ)
    ) %>%
    select(-c(Sex, Year_birth, Month_birth, date_birth, age_at_dx)) %>%
    glimpse()

# IGL with different matched ethnicity models

gender_list = c("eth_white_tp0_match", "eth_black_tp0_match", "eth_asian_tp0_match")

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
    glimpse()

#endregion

#region Associations between gender and diagnoses

dx_list = unique(firstocc$icd_code)
dx_list <- dx_list[dx_list != ""]

results = list()
i=1
for (d in 1:length(dx_list)) {

    print(d)

    dx_icd = dx_list[d]
    dx_name = unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(dx_name))
    dx_chapter = as.character(unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(chapter)))
    dx_categ = as.character(unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(categ)))
    dx_n = sum((unique(df_gender$ID) %in% (firstocc %>% filter(icd_code == dx_icd) %>% pull(ID))) == TRUE)
    
    print(dx_name)

    if (dx_n > 5) {
        
        df_dx = firstocc %>%
            filter(icd_code == dx_icd) %>%
            left_join(df_gender, by="ID") %>%
            filter(complete.cases(.)) %>%
            mutate(Group = "DX") %>% 
            select(ID, Sex, Age, Gender_score, Group, model)

        ids_not_controls = unique(firstocc %>% filter(chapter == dx_chapter) %>% pull(ID))

        df_hc = df_gender %>%
            filter(!ID %in% ids_not_controls) %>%
            mutate(Group = "HC") %>%
            select(ID, Sex, Age, Gender_score, Group, model) %>%
            glimpse()

        df_all = rbind(df_dx, df_hc)
        df_all$Group = factor(df_all$Group, levels=c("HC", "DX"))

        # Female
        if (length(unique(df_dx %>% filter(Sex == "Female") %>% pull(ID))) > 5) {
            # White
            lm_females_white = glm(Group ~ Gender_score + Age, data = df_all %>% filter(Sex == "Female", model=="eth_white_tp0_match"), family = binomial)
            lm_females_white_coef = summary(lm_females_white)$coefficients
            lm_females_white_or = as.data.frame(exp(cbind(OR = coef(lm_females_white), confint(lm_females_white))))
            # Black
            lm_females_black = glm(Group ~ Gender_score + Age, data = df_all %>% filter(Sex == "Female", model=="eth_black_tp0_match"), family = binomial)
            lm_females_black_coef = summary(lm_females_black)$coefficients
            lm_females_black_or = as.data.frame(exp(cbind(OR = coef(lm_females_black), confint(lm_females_black))))
            # Asian
            lm_females_asian = glm(Group ~ Gender_score + Age, data = df_all %>% filter(Sex == "Female", model=="eth_asian_tp0_match"), family = binomial)
            lm_females_asian_coef = summary(lm_females_asian)$coefficients
            lm_females_asian_or = as.data.frame(exp(cbind(OR = coef(lm_females_asian), confint(lm_females_asian))))
            # Ethnicity interaction
            lm_females_inter = glm(Group ~ Gender_score*model + Age, data = df_all %>% filter(Sex == "Female"), family = binomial)
            lm_females_inter_coef = summary(lm_females_inter)$coefficients
        } else {
            # White
            lm_females_white_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_females_white_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_females_white_coef) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
            rownames(lm_females_white_coef) = c("(Intercept)", "Gender_score", "Age")
            colnames(lm_females_white_or) = c("OR", "2.5 %", "97.5 %")
            rownames(lm_females_white_or) = c("(Intercept)", "Gender_score", "Age")
            # Black
            lm_females_black_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_females_black_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_females_black_coef) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
            rownames(lm_females_black_coef) = c("(Intercept)", "Gender_score", "Age")
            colnames(lm_females_black_or) = c("OR", "2.5 %", "97.5 %")
            rownames(lm_females_black_or) = c("(Intercept)", "Gender_score", "Age")
            # Asian
            lm_females_asian_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_females_asian_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_females_asian_coef) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
            rownames(lm_females_asian_coef) = c("(Intercept)", "Gender_score", "Age")
            colnames(lm_females_asian_or) = c("OR", "2.5 %", "97.5 %")
            rownames(lm_females_asian_or) = c("(Intercept)", "Gender_score", "Age")
            # Ethnicity interaction
            lm_females_inter_coef = matrix(data=NA, nrow=9, ncol=4)
            colnames(lm_females_inter_coef) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
            rownames(lm_females_inter_coef) = c(
                "(Intercept)", "Gender_score",
                "modeleth_white_tp0_match", "modeleth_black_tp0_match", "modeleth_asian_tp0_match",
                "Age", "Gender_score:modeleth_white_tp0_match", "Gender_score:modeleth_black_tp0_match", "Gender_score:modeleth_asian_tp0_match"
            )
        }

        # Male
        if (length(unique(df_dx %>% filter(Sex == "Male") %>% pull(ID))) > 5) {
            # White
            lm_males_white = glm(Group ~ Gender_score + Age, data = df_all %>% filter(Sex == "Male", model=="eth_white_tp0_match"), family = binomial)
            lm_males_white_coef = summary(lm_males_white)$coefficients
            lm_males_white_or = as.data.frame(exp(cbind(OR = coef(lm_males_white), confint(lm_males_white))))
            # Black
            lm_males_black = glm(Group ~ Gender_score + Age, data = df_all %>% filter(Sex == "Male", model=="eth_black_tp0_match"), family = binomial)
            lm_males_black_coef = summary(lm_males_black)$coefficients
            lm_males_black_or = as.data.frame(exp(cbind(OR = coef(lm_males_black), confint(lm_males_black))))
            # Asian
            lm_males_asian = glm(Group ~ Gender_score + Age, data = df_all %>% filter(Sex == "Male", model=="eth_asian_tp0_match"), family = binomial)
            lm_males_asian_coef = summary(lm_males_asian)$coefficients
            lm_males_asian_or = as.data.frame(exp(cbind(OR = coef(lm_males_asian), confint(lm_males_asian))))
            # Ethnicity interaction
            lm_males_inter = glm(Group ~ Gender_score*model + Age, data = df_all %>% filter(Sex == "Male"), family = binomial)
            lm_males_inter_coef = summary(lm_males_inter)$coefficients
        } else {
            # White
            lm_males_white_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_males_white_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_males_white_coef) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
            rownames(lm_males_white_coef) = c("(Intercept)", "Gender_score", "Age")
            colnames(lm_males_white_or) = c("OR", "2.5 %", "97.5 %")
            rownames(lm_males_white_or) = c("(Intercept)", "Gender_score", "Age")
            # Black
            lm_males_black_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_males_black_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_males_black_coef) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
            rownames(lm_males_black_coef) = c("(Intercept)", "Gender_score", "Age")
            colnames(lm_males_black_or) = c("OR", "2.5 %", "97.5 %")
            rownames(lm_males_black_or) = c("(Intercept)", "Gender_score", "Age")
            # Asian
            lm_males_asian_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_males_asian_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_males_asian_coef) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
            rownames(lm_males_asian_coef) = c("(Intercept)", "Gender_score", "Age")
            colnames(lm_males_asian_or) = c("OR", "2.5 %", "97.5 %")
            rownames(lm_males_asian_or) = c("(Intercept)", "Gender_score", "Age")
            # Ethnicity interaction
            lm_males_inter_coef = matrix(data=NA, nrow=9, ncol=4)
            colnames(lm_males_inter_coef) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
            rownames(lm_males_inter_coef) = c(
                "(Intercept)", "Gender_score",
                "modeleth_white_tp0_match", "modeleth_black_tp0_match", "modeleth_asian_tp0_match",
                "Age", "Gender_score:modeleth_white_tp0_match", "Gender_score:modeleth_black_tp0_match", "Gender_score:modeleth_asian_tp0_match"
            )
        }

        # Save results
        results[[i]] = data.frame(
            icd_code = dx_icd,
            diagnosis = dx_name,
            chapter = dx_chapter,
            categ = dx_categ,
            n_dx_males = length(unique(df_all %>% filter(Sex == "Male" & Group == "DX") %>% pull(ID))),
            n_dx_females = length(unique(df_all %>% filter(Sex == "Female" & Group == "DX") %>% pull(ID))),
            n_hc_males = length(unique(df_all %>% filter(Sex == "Male" & Group == "HC") %>% pull(ID))),
            n_hc_females = length(unique(df_all %>% filter(Sex == "Female" & Group == "HC") %>% pull(ID))),
            # Males
            # White
            coef_males_white = lm_males_white_coef["Gender_score","Estimate"],
            pval_males_white = lm_males_white_coef["Gender_score","Pr(>|z|)"],
            or_males_white = lm_males_white_or["Gender_score","OR"],
            or_males_lower_white = lm_males_white_or["Gender_score","2.5 %"],
            or_males_upper_white = lm_males_white_or["Gender_score","97.5 %"],
            # Black
            coef_males_black = lm_males_black_coef["Gender_score","Estimate"],
            pval_males_black = lm_males_black_coef["Gender_score","Pr(>|z|)"],
            or_males_black = lm_males_black_or["Gender_score","OR"],
            or_males_lower_black = lm_males_black_or["Gender_score","2.5 %"],
            or_males_upper_black = lm_males_black_or["Gender_score","97.5 %"],
            # Asian
            coef_males_asian = lm_males_asian_coef["Gender_score","Estimate"],
            pval_males_asian = lm_males_asian_coef["Gender_score","Pr(>|z|)"],
            or_males_asian = lm_males_asian_or["Gender_score","OR"],
            or_males_lower_asian = lm_males_asian_or["Gender_score","2.5 %"],
            or_males_upper_asian = lm_males_asian_or["Gender_score","97.5 %"],
            # Females
            # White
            coef_females_white = lm_females_white_coef["Gender_score","Estimate"],
            pval_females_white = lm_females_white_coef["Gender_score","Pr(>|z|)"],
            or_females_white = lm_females_white_or["Gender_score","OR"],
            or_females_lower_white = lm_females_white_or["Gender_score","2.5 %"],
            or_females_upper_white = lm_females_white_or["Gender_score","97.5 %"],
            # Black
            coef_females_black = lm_females_black_coef["Gender_score","Estimate"],
            pval_females_black = lm_females_black_coef["Gender_score","Pr(>|z|)"],
            or_females_black = lm_females_black_or["Gender_score","OR"],
            or_females_lower_black = lm_females_black_or["Gender_score","2.5 %"],
            or_females_upper_black = lm_females_black_or["Gender_score","97.5 %"],
            # Asian
            coef_females_asian = lm_females_asian_coef["Gender_score","Estimate"],
            pval_females_asian = lm_females_asian_coef["Gender_score","Pr(>|z|)"],
            or_females_asian = lm_females_asian_or["Gender_score","OR"],
            or_females_lower_asian = lm_females_asian_or["Gender_score","2.5 %"],
            or_females_upper_asian = lm_females_asian_or["Gender_score","97.5 %"],
            # Interactions
            coef_inter_males_white = lm_males_inter_coef["Gender_score:modeleth_white_tp0_match","Estimate"],
            coef_inter_males_black = lm_males_inter_coef["Gender_score:modeleth_black_tp0_match","Estimate"],
            coef_inter_males_asian = lm_males_inter_coef["Gender_score:modeleth_asian_tp0_match","Estimate"],
            coef_inter_females_white = lm_females_inter_coef["Gender_score:modeleth_white_tp0_match","Estimate"],
            coef_inter_females_black = lm_females_inter_coef["Gender_score:modeleth_black_tp0_match","Estimate"],
            coef_inter_females_asian = lm_females_inter_coef["Gender_score:modeleth_asian_tp0_match","Estimate"],
            pval_inter_males_white = lm_males_inter_coef["Gender_score:modeleth_white_tp0_match","Pr(>|z|)"],
            pval_inter_males_black = lm_males_inter_coef["Gender_score:modeleth_black_tp0_match","Pr(>|z|)"],
            pval_inter_males_asian = lm_males_inter_coef["Gender_score:modeleth_asian_tp0_match","Pr(>|z|)"],
            pval_inter_females_white = lm_females_inter_coef["Gender_score:modeleth_white_tp0_match","Pr(>|z|)"],
            pval_inter_females_black = lm_females_inter_coef["Gender_score:modeleth_black_tp0_match","Pr(>|z|)"],
            pval_inter_females_asian = lm_females_inter_coef["Gender_score:modeleth_asian_tp0_match","Pr(>|z|)"]
        )

        i = i + 1

    }
}

# Save
results = do.call(rbind, results)
fwrite(results, "./results/results_dx_glm_ethnicity.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

print("DONE!")

#endregion
