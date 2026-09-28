
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
library(stringr)

dir.create("./visualization/indiv", showWarnings=FALSE)

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
df_gender = as.data.frame(fread("../gender_score_new/results/all_tp0/gender_score_combined.tsv"))
df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    glimpse()

df_pharma_atc = as.data.frame(fread("../../../UKB/Analyses/clean_pharma/results/df_code_map_compressed.tsv"))
df_pharma_atc = df_pharma_atc %>%
    filter(InstanceID == 0) %>%
    select(-c(InstanceID, Sex_31, Age_21003)) %>%
    glimpse()

df_pharma_other = as.data.frame(fread("../../../UKB/Analyses/clean_pharma/results/df_other_med_variables.tsv"))
df_pharma_other = df_pharma_other %>%
    filter(InstanceID == 0) %>%
    select(-c(InstanceID, Sex_31, Age_21003, Number_of_antibiotics_taken_in_last_3_months_6671_0, Number_of_treatments_medications_taken_137_0)) %>%
    glimpse()

atc_codes = as.data.frame(fread("../../../UKB/Analyses/clean_pharma/medication_atc_categories_clean.csv"))
atc_codes = atc_codes %>%
    mutate(
        Name_1 = tolower(Name_1),
        Name_2 = tolower(Name_2),
        Name_3 = tolower(Name_3)
    ) %>%
    mutate(
        Name_1_clean = gsub("[(),-/.]", "", Name_1),
        Name_2_clean = gsub("[(),-/.]", "", Name_2),
        Name_3_clean = gsub("[(),-/.]", "", Name_3)
    ) %>%
    glimpse()

#endregion

#region Associations with ATC codes

list_atc = colnames(df_pharma_atc)[-1]

results = list()
i = 1
for (a in 1:length(list_atc)) {
    print(a)

    atc_level = as.numeric(str_sub(strsplit(list_atc[a], "_")[[1]][1], -1, -1))
    atc_val = strsplit(list_atc[a], "_")[[1]][2]

    atc_n = sum(df_pharma_atc %>% filter(ID %in% df_gender$ID) %>% pull(list_atc[a]))

    if (atc_val != "none") {
        atc_name = tolower(unique(atc_codes %>%
            select(paste0("Level_", atc_level), paste0("Name_", atc_level)) %>%
            rename(level = paste0("Level_", atc_level), name = paste0("Name_", atc_level)) %>%
            filter(level==atc_val) %>%
            pull(name)))
        
        atc_name_clean = tolower(unique(atc_codes %>%
            select(paste0("Level_", atc_level), paste0("Name_", atc_level, "_clean")) %>%
            rename(level = paste0("Level_", atc_level), name = paste0("Name_", atc_level, "_clean")) %>%
            filter(level==atc_val) %>%
            pull(name)))
    } else {
        atc_name = "none"
        atc_name_clean = "none"
    }

    print(atc_name)

    # Control group: does not take medication in same category (ATC level 1)
    if (atc_name == "none") {
        atc_level1_control = "none"
    } else if (atc_level != 1) {
        atc_level1_control = str_sub(atc_val, 1, 1)
    } else {
        atc_level1_control = atc_val
    }

    if (atc_n > 5) {
        
        df_med = df_pharma_atc %>% 
            filter(ID %in% df_gender$ID) %>% 
            select(ID, all_of(list_atc[a]), all_of(paste0("level1_", atc_level1_control))) %>%
            left_join(df_gender, by="ID") %>%
            rename(
                med = list_atc[a],
                med_control = paste0("level1_", atc_level1_control)
            )
        
        if (atc_level == 1) { df_med = df_med %>% mutate(med = med_control) }
    
        df_med = df_med %>%
            mutate(case_control = case_when(
                med == 1 ~ "Case",
                med_control == 0 ~ "Control"
            )) %>%
            filter(!is.na(case_control)) %>%
            mutate(case_control = factor(case_control, levels=c("Control", "Case")))

        # Females
        if (length(df_med %>% filter(case_control == "Case", Sex == "Female") %>% pull(ID)) > 5) {
            lm_females = glm(case_control ~ Gender_score + Age, data = df_med %>% filter(Sex == "Female"), family = binomial)
            lm_females_coef = summary(lm_females)$coefficients
            lm_females_or = as.data.frame(exp(cbind(OR = coef(lm_females), confint(lm_females))))
        } else {
            lm_females_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_females_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_females_or) = c("OR", "2.5 %", "97.5 %")
        }

        # Males
        if (length(df_med %>% filter(case_control == "Case", Sex == "Male") %>% pull(ID)) > 5) {
            lm_males = glm(case_control ~ Gender_score + Age, data = df_med %>% filter(Sex == "Male"), family = binomial)
            lm_males_coef = summary(lm_males)$coefficients
            lm_males_or = as.data.frame(exp(cbind(OR = coef(lm_males), confint(lm_males))))
        } else {
            lm_males_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_males_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_males_or) = c("OR", "2.5 %", "97.5 %")
        }

        # Interaction
        if ((length(df_med %>% filter(case_control == "Case", Sex == "Male") %>% pull(ID)) > 5) & (length(df_med %>% filter(case_control == "Case", Sex == "Female") %>% pull(ID)) > 5)) {
            lm_inter = summary(glm(case_control ~  Sex + Gender_score + Gender_score*Sex + Age, data = df_med, family = binomial))
        }

        # Save results
        results[[i]] = data.frame(
            ATC_level = atc_level,
            ATC_code = atc_val,
            ATC_name = atc_name,
            n_case_male = length(df_med %>% filter(case_control == "Case", Sex == "Male") %>% pull(ID)),
            n_case_female = length(df_med %>% filter(case_control == "Case", Sex == "Female") %>% pull(ID)),
            n_control_male = length(df_med %>% filter(case_control == "Control", Sex == "Male") %>% pull(ID)),
            n_control_female = length(df_med %>% filter(case_control == "Control", Sex == "Female") %>% pull(ID)),
            coef_males = lm_males_coef[2,1],
            pval_males = lm_males_coef[2,4],
            or_males = lm_males_or[2,1],
            or_males_lower = lm_males_or[2,2],
            or_males_upper = lm_males_or[2,3],
            coef_females = lm_females_coef[2,1],
            pval_females = lm_females_coef[2,4],
            or_females = lm_females_or[2,1],
            or_females_lower = lm_females_or[2,2],
            or_females_upper = lm_females_or[2,3],
            coef_inter = lm_inter$coefficients["SexMale:Gender_score","Estimate"],
            pval_inter = lm_inter$coefficients["SexMale:Gender_score","Pr(>|z|)"],
            coef_sexdiff = lm_inter$coefficients["SexMale", "Estimate"],
            pval_sexdiff = lm_inter$coefficients["SexMale", "Pr(>|z|)"],
            coef_igl_female = lm_inter$coefficients["Gender_score", "Estimate"],
            coef_igl_male = lm_inter$coefficients["Gender_score", "Estimate"] + lm_inter$coefficients["SexMale:Gender_score", "Estimate"]
        )
        
        i = i + 1
    }
}

results = do.call(rbind, results)
fwrite(results, "./results/results_glm.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

#endregion

#region Associations with other medication variables

list_other = colnames(df_pharma_other)[-1]

results = list()
i = 1

for (a in 1:length(list_other)) {
    print(a)
    print(list_other[a])

    other_n = sum(df_pharma_other %>% filter(ID %in% df_gender$ID) %>% pull(list_other[a]), na.rm=TRUE)

    if (other_n > 5) {

        df_tmp = df_pharma_other %>%
            filter(ID %in% df_gender$ID) %>% 
            select(ID, all_of(list_other[a])) %>%
            left_join(df_gender, by="ID") %>%
            rename(med = list_other[a]) %>%
            filter(!is.na(med)) %>%
            mutate(case_control = case_when(
                med == 1 ~ "Case",
                med == 0 ~ "Control"
            )) %>%
            mutate(case_control = factor(case_control, levels=c("Control", "Case")))

        # Females
        if (length(df_tmp %>% filter(case_control == "Case", Sex == "Female") %>% pull(ID)) > 5) {
            lm_females = glm(case_control ~ Gender_score + Age, data = df_tmp %>% filter(Sex == "Female"), family = binomial)
            lm_females_coef = summary(lm_females)$coefficients
            lm_females_or = as.data.frame(exp(cbind(OR = coef(lm_females), confint(lm_females))))
        } else {
            lm_females_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_females_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_females_or) = c("OR", "2.5 %", "97.5 %")
        }

        # Males
        if (length(df_tmp %>% filter(case_control == "Case", Sex == "Male") %>% pull(ID)) > 5) {
            lm_males = glm(case_control ~ Gender_score + Age, data = df_tmp %>% filter(Sex == "Male"), family = binomial)
            lm_males_coef = summary(lm_males)$coefficients
            lm_males_or = as.data.frame(exp(cbind(OR = coef(lm_males), confint(lm_males))))
        } else {
            lm_males_coef = matrix(data=NA, nrow=3, ncol=4)
            lm_males_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
            colnames(lm_males_or) = c("OR", "2.5 %", "97.5 %")
        }

        # Interaction
        if ((length(df_tmp %>% filter(case_control == "Case", Sex == "Male") %>% pull(ID)) > 5) & (length(df_tmp %>% filter(case_control == "Case", Sex == "Female") %>% pull(ID)) > 5)) {
            lm_inter = summary(glm(case_control ~  Sex + Gender_score + Gender_score*Sex + Age, data = df_med, family = binomial))
        }

        # Save results
        results[[i]] = data.frame(
            var_name = list_other[a],
            med_name = sub(".*_", "", list_other[a]),
            n_case_male = length(df_tmp %>% filter(case_control == "Case", Sex == "Male") %>% pull(ID)),
            n_case_female = length(df_tmp %>% filter(case_control == "Case", Sex == "Female") %>% pull(ID)),
            n_control_male = length(df_tmp %>% filter(case_control == "Control", Sex == "Male") %>% pull(ID)),
            n_control_female = length(df_tmp %>% filter(case_control == "Control", Sex == "Female") %>% pull(ID)),
            coef_males = lm_males_coef[2,1],
            pval_males = lm_males_coef[2,4],
            or_males = lm_males_or[2,1],
            or_males_lower = lm_males_or[2,2],
            or_males_upper = lm_males_or[2,3],
            coef_females = lm_females_coef[2,1],
            pval_females = lm_females_coef[2,4],
            or_females = lm_females_or[2,1],
            or_females_lower = lm_females_or[2,2],
            or_females_upper = lm_females_or[2,3],
            coef_inter = lm_inter$coefficients["SexMale:Gender_score","Estimate"],
            pval_inter = lm_inter$coefficients["SexMale:Gender_score","Pr(>|z|)"],
            coef_sexdiff = lm_inter$coefficients["SexMale", "Estimate"],
            pval_sexdiff = lm_inter$coefficients["SexMale", "Pr(>|z|)"],
            coef_igl_female = lm_inter$coefficients["Gender_score", "Estimate"],
            coef_igl_male = lm_inter$coefficients["Gender_score", "Estimate"] + lm_inter$coefficients["SexMale:Gender_score", "Estimate"]
        )
        
        i = i + 1

    }
}

results = do.call(rbind, results)
fwrite(results, "./results/results_other.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

#endregion
