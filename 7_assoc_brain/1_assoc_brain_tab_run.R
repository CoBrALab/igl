
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
library(effects)
library(lme4)
library(lmerTest)
library(splines)

dir.create("./results/alltab", showWarnings = FALSE)
dir.create("./visualization/alltab", showWarnings = FALSE)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

# Brain data
df_brain = as.data.frame(fread("../../../UKB/Analyses/clean_brain_tab/results/brain_tab_clean_wide.tsv"))
brain_mapping = as.data.frame(fread("../../../UKB/Analyses/clean_brain_tab/results/column_mapping.tsv"))

# Gender scores
df_gender = as.data.frame(fread("../gender_score_new/results/all_tp2/gender_score_combined.tsv"))

df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    glimpse()

# Baseline data
var_df = as.data.frame(fread("../gender_score_new/variable_mapping.tsv"))
df_raw = as.data.frame(fread("../impute_input/results/df_imputed_tp_all_combined.tsv"))

# Covariates
df_cov = as.data.frame(fread("../../../UKB/tabular/df_body_cov/UKBB_body_cov_wide.tsv"))
df_cov = df_cov %>%
    filter(InstanceID == 2) %>%
    rename(ID = "SubjectID", height = "Standing height_50", assessment_center = "UK Biobank assessment centre_54", weight = "Weight_21002") %>%
    select(ID, assessment_center, height, weight) %>%
    mutate(assessment_center = factor(assessment_center)) %>%
    filter(complete.cases(.)) %>%
    glimpse()

# QC data
inclusions = list()
inclusions[['T1']] = as.data.frame(fread("../../QC/inclusions_t1.txt"))
inclusions[['T2-FLAIR']] = as.data.frame(fread("../../QC/inclusions_flair.txt"))
inclusions[['DWI']] = as.data.frame(fread("../../QC/inclusions_dwi.txt"))

#endregion

#region Clean data

# Brain data
list_confound_vars = c(
        "24419", #Measure of head motion in T1 structural image
        "25756", #Scanner lateral (X) brain position
        "25757", #Scanner transverse (Y) brain position
        "25758", #Scanner longitudinal (Z) brain position
        "25759", #Scanner table position
        "25000" #Volumetric scaling from T1 head image to standard space
    )

df_confounds = df_brain %>% select(ID, all_of(list_confound_vars))
df_brain = df_brain %>% select(-all_of(list_confound_vars))

# Baseline data

df_raw = df_raw %>% mutate(
        Date_of_attending_assessment_centre_53_0 = as.Date(Date_of_attending_assessment_centre_53_0),
        sex_31 = factor(sex_31, levels=c("Female", "Male")),
        genetic_sex_22001 = factor(genetic_sex_22001, levels=c("Female", "Male")),
        own_rent_accommodation_680 = factor(own_rent_accommodation_680),
        job_walking_standing_806 = factor(job_walking_standing_806),
        job_physical_work_816 = factor(job_physical_work_816),
        job_shift_work_826 = factor(job_shift_work_826),
        freq_friend_family_visits_1031 = factor(freq_friend_family_visits_1031),
        lenth_mobile_phone_use_1110 = factor(lenth_mobile_phone_use_1110),
        getting_up_in_morning_1170 = factor(getting_up_in_morning_1170),
        sleep_chronotype_1180 = factor(sleep_chronotype_1180),
        nap_during_day_1190 = factor(nap_during_day_1190),
        sleeplessness_1200 = factor(sleeplessness_1200),
        daytime_sleeping_narcolepsy_1220 = factor(daytime_sleeping_narcolepsy_1220),
        oily_fish_intake_1329 = factor(oily_fish_intake_1329),
        non_oily_fish_intake_1339 = factor(non_oily_fish_intake_1339),
        processed_meat_intake_1349 = factor(processed_meat_intake_1349),
        poultry_intake_1359 = factor(poultry_intake_1359),
        beef_intake_1369 = factor(beef_intake_1369),
        lamb_mutton_intake_1379 = factor(lamb_mutton_intake_1379),
        pork_intake_1389 = factor(pork_intake_1389),
        cheese_intake_1408 = factor(cheese_intake_1408),
        salt_added_1478 = factor(salt_added_1478),
        variation_diet_1548 = factor(variation_diet_1548),
        alcohol_intake_freq_1558 = factor(alcohol_intake_freq_1558),
        able_to_confide_2110 = factor(able_to_confide_2110),
        play_computer_games_2237 = factor(play_computer_games_2237),
        use_uv_protection_2267 = factor(use_uv_protection_2267),
        job_satisfaction_4537 = factor(job_satisfaction_4537),
        health_satisfaction_4548 = factor(health_satisfaction_4548),
        family_relationship_satisfaction_4559 = factor(family_relationship_satisfaction_4559),
        friendship_satisfaction_4570 = factor(friendship_satisfaction_4570),
        finances_satisfaction_4581 = factor(finances_satisfaction_4581),
        access_private_healthcare_4674 = factor(access_private_healthcare_4674),
        smoking_status_20116 = factor(smoking_status_20116),
        alcohol_drinker_status_20117 = factor(alcohol_drinker_status_20117),
        edu_6138_Alevels_ASlevels_equivalent = factor(edu_6138_Alevels_ASlevels_equivalent),
        edu_6138_CSE_equivalent = factor(edu_6138_CSE_equivalent),
        edu_6138_college_unviersity_degree = factor(edu_6138_college_unviersity_degree),
        edu_6138_NVQ_HND_HNC_equivalent = factor(edu_6138_NVQ_HND_HNC_equivalent),
        edu_6138_no_qualifications = factor(edu_6138_no_qualifications),
        edu_6138_Olevels_GCSEs_equivalent = factor(edu_6138_Olevels_GCSEs_equivalent),
        edu_6138_other_professional_quali_nursing_teaching = factor(edu_6138_other_professional_quali_nursing_teaching),
        social_activity_6160_adult_education_class = factor(social_activity_6160_adult_education_class),
        social_activity_6160_no_social_activity = factor(social_activity_6160_no_social_activity),
        social_activity_6160_other_group_activity = factor(social_activity_6160_other_group_activity),
        social_activity_6160_pub_social_club = factor(social_activity_6160_pub_social_club),
        social_activity_6160_religious_group = factor(social_activity_6160_religious_group),
        social_activity_6160_sports_club_gym = factor(social_activity_6160_sports_club_gym),
        employement_6142_unpaid_voluntary_work = factor(employement_6142_unpaid_voluntary_work),
        employement_6142_full_part_time_student = factor(employement_6142_full_part_time_student),
        employement_6142_paid_or_self_employed = factor(employement_6142_paid_or_self_employed),
        employement_6142_looking_after_home_family = factor(employement_6142_looking_after_home_family),
        employement_6142_retired = factor(employement_6142_retired),
        employement_6142_unable_to_work_sickness_disability = factor(employement_6142_unable_to_work_sickness_disability),
        employement_6142_unemployed = factor(employement_6142_unemployed)
    ) %>%
    filter(instanceID == 2)

#endregion

#region Associations of every variable with gender

# num_field = "25009" #Volume of brain, grey+white matter (normalised for head size)

assoc_gender = function(num_field) {
    var_mapping = brain_mapping %>% filter(FieldID == num_field)
    
    if (var_mapping$modality %in% c("T1", "T2-FLAIR", "DWI")) { 
        inc_tmp = inclusions[[var_mapping$modality]] 
    } else { inc_tmp = inclusions[['T1']] }

    df_lm <<- df_brain %>% 
        select(ID, all_of(num_field)) %>% # Select brain var
        filter(ID %in% inc_tmp$V1) %>% # Remove QC exclusions
        left_join(df_raw %>% select(ID), by="ID") %>%
        left_join(df_gender, by="ID") %>%
        left_join(df_confounds, by="ID") %>%
        left_join(df_cov, by="ID") %>%
        rename(
            Brain = all_of(num_field),
            motion_t1 = "24419",
            scanner_x = "25756",
            scanner_y = "25757",
            scanner_z = "25758",
            scanner_pos = "25759",
            ICV = "25000"
            ) %>%
        filter(complete.cases(.))

    if (nrow(df_lm) < 5) { return("Fewer than 5 subjects with data") }
    if ( is.numeric(df_lm$Brain) == FALSE ) { return("Non-numeric brain variable") }

    # Run linear models
    lm_inter = summary(lmer(scale(Brain) ~ Sex + Gender_score + Gender_score*Sex + bs(Age, degree = 3, df = 4) + ICV + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), data = df_lm))
    lm_males = summary(lmer(scale(Brain) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + ICV + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), data = df_lm %>% filter(Sex == "Male")))
    lm_females = summary(lmer(scale(Brain) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + ICV + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), data = df_lm %>% filter(Sex == "Female")))

    # Store results
    results_i = data.frame(
            FieldID = var_mapping %>% pull(FieldID),
            Field_title = var_mapping %>% pull(ukb_field_title),
            fmrib_name = var_mapping %>% pull(fmrib_name),
            Category = var_mapping %>% pull(Category),
            Modality = var_mapping %>% pull(modality),
            n_females = nrow(df_lm %>% filter(Sex == "Female")),
            n_males = nrow(df_lm %>% filter(Sex == "Male")),
            n_all = nrow(df_lm),
            coef_males = lm_males$coefficients["scale(Gender_score)", "Estimate"],
            pval_males = lm_males$coefficients["scale(Gender_score)", "Pr(>|t|)"],
            coef_females = lm_females$coefficients["scale(Gender_score)", "Estimate"],
            pval_females = lm_females$coefficients["scale(Gender_score)", "Pr(>|t|)"],
            coef_inter = lm_inter$coefficients["SexMale:Gender_score","Estimate"],
            pval_inter = lm_inter$coefficients["SexMale:Gender_score","Pr(>|t|)"],
            coef_sexdiff = lm_inter$coefficients["SexMale", "Estimate"],
            pval_sexdiff = lm_inter$coefficients["SexMale", "Pr(>|t|)"],
            coef_igl_female = lm_inter$coefficients["Gender_score", "Estimate"],
            coef_igl_male = lm_inter$coefficients["Gender_score", "Estimate"] + lm_inter$coefficients["SexMale:Gender_score", "Estimate"]
        )
    
    return(results_i)
}

# Run for all variables
results = list()

i = 1
for (c in 2:length(colnames(df_brain))) {
    results[[i]] = assoc_gender(colnames(df_brain)[c])
    i = i + 1
}

results = Filter(is.data.frame, results)
results = do.call(rbind, results)

results = results %>% filter(complete.cases(.))

fwrite(results, "./results/alltab/results_alltab.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

#endregion

