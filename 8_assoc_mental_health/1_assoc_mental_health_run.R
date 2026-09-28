
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
library(ggbump)
library(stringr)
library(effects)
library(lme4)
library(lmerTest)
library(splines)
library(RNOmni)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

dir.create("./visualization/indiv", showWarnings=FALSE)

#region Load data

# Load
df_mental = as.data.frame(fread("../../../UKB/Analyses/clean_mental_health/results/mental_health_recoded.tsv"))
mental_var_mapping = as.data.frame(fread("../../../UKB/Analyses/clean_mental_health/variable_mapping.tsv"))
df_mental = df_mental %>%
    mutate(
        InstanceID = factor(InstanceID, levels=c(0,1,2,3)),
        Sex_31_0 = factor(Sex_31_0, levels=c("Female", "Male")),
        Mood_swings_1920_0 = factor(Mood_swings_1920_0, levels=c(0,1)),
        Miserableness_1930_0 = factor(Miserableness_1930_0, levels=c(0,1)),
        Irritability_1940_0 = factor(Irritability_1940_0, levels=c(0,1)),
        Sensitivity_hurt_feelings_1950_0 = factor(Sensitivity_hurt_feelings_1950_0, levels=c(0,1)),
        Fedup_feelings_1960_0 = factor(Fedup_feelings_1960_0, levels=c(0,1)),
        Nervous_feelings_1970_0 = factor(Nervous_feelings_1970_0, levels=c(0,1)),
        Worrier_anxious_feelings_1980_0 = factor(Worrier_anxious_feelings_1980_0, levels=c(0,1)),
        Tense_highly_strung_1990_0 = factor(Tense_highly_strung_1990_0, levels=c(0,1)),
        Worry_too_long_after_embarrassment_2000_0 = factor(Worry_too_long_after_embarrassment_2000_0, levels=c(0,1)),
        Suffer_from_nerves_2010_0 = factor(Suffer_from_nerves_2010_0, levels=c(0,1)),
        Loneliness_isolation_2020_0 = factor(Loneliness_isolation_2020_0, levels=c(0,1)),
        Guilty_feelings_2030_0 = factor(Guilty_feelings_2030_0, levels=c(0,1)),
        Risk_taking_2040_0 = factor(Risk_taking_2040_0, levels=c(0,1)),
        Frequency_of_depressed_mood_in_last_2_weeks_2050_0 = factor(Frequency_of_depressed_mood_in_last_2_weeks_2050_0, levels=c(0,1,2,3)),
        Frequency_of_unenthusiasm_disinterest_in_last_2_weeks_2060_0 = factor(Frequency_of_unenthusiasm_disinterest_in_last_2_weeks_2060_0, levels=c(0,1,2,3)),
        Frequency_of_tenseness_restlessness_in_last_2_weeks_2070_0 = factor(Frequency_of_tenseness_restlessness_in_last_2_weeks_2070_0, levels=c(0,1,2,3)),
        Frequency_of_tiredness_lethargy_in_last_2_weeks_2080_0 = factor(Frequency_of_tiredness_lethargy_in_last_2_weeks_2080_0, levels=c(0,1,2,3)),
        Seen_GP_for_nerves_anxiety_tension_or_depression_2090_0 = factor(Seen_GP_for_nerves_anxiety_tension_or_depression_2090_0, levels=c(0,1)),
        Seen_a_psychiatrist_for_nerves_anxiety_tension_or_depression_2100_0 = factor(Seen_a_psychiatrist_for_nerves_anxiety_tension_or_depression_2100_0, levels=c(0,1)),
        Happiness_4526_0 = factor(Happiness_4526_0, levels=c(0,1,2,3,4,5)),
        Work_job_satisfaction_4537_0 = factor(Work_job_satisfaction_4537_0, levels=c(0,1,2,3,4,5)),
        Health_satisfaction_4548_0 = factor(Health_satisfaction_4548_0, levels=c(0,1,2,3,4,5)),
        Family_relationship_satisfaction_4559_0 = factor(Family_relationship_satisfaction_4559_0, levels=c(0,1,2,3,4,5)),
        Friendships_satisfaction_4570_0 = factor(Friendships_satisfaction_4570_0, levels=c(0,1,2,3,4,5)),
        Financial_situation_satisfaction_4581_0 = factor(Financial_situation_satisfaction_4581_0, levels=c(0,1,2,3,4,5)),
        Ever_depressed_for_a_whole_week_4598_0 = factor(Ever_depressed_for_a_whole_week_4598_0, levels=c(0,1)),
        Ever_unenthusiastic_disinterested_for_a_whole_week_4631_0 = factor(Ever_unenthusiastic_disinterested_for_a_whole_week_4631_0, levels=c(0,1)),
        Ever_manic_hyper_for_2_days_4642_0 = factor(Ever_manic_hyper_for_2_days_4642_0, levels=c(0,1)),
        Ever_highly_irritable_argumentative_for_2_days_4653_0 = factor(Ever_highly_irritable_argumentative_for_2_days_4653_0, levels=c(0,1)),
        Length_of_longest_manic_irritable_episode_5663_0 = factor(Length_of_longest_manic_irritable_episode_5663_0, levels=c(0,1,2)),
        Severity_of_manic_irritable_episodes_5674_0 = factor(Severity_of_manic_irritable_episodes_5674_0, levels=c(0,1)),
        Illness_6145_illness_injury_assault_to_yourself = factor(Illness_6145_illness_injury_assault_to_yourself, levels=c(1,2), labels=c(0,1)),
        Illness_6145_none = factor(Illness_6145_none, levels=c(1,2), labels=c(0,1)),
        Illness_6145_illness_injury_assault_to_relative = factor(Illness_6145_illness_injury_assault_to_relative, levels=c(1,2), labels=c(0,1)),
        Illness_6145_death_relative = factor(Illness_6145_death_relative, levels=c(1,2), labels=c(0,1)),
        Illness_6145_separation_divorce = factor(Illness_6145_separation_divorce, levels=c(1,2), labels=c(0,1)),
        Illness_6145_financial_difficulties = factor(Illness_6145_financial_difficulties, levels=c(1,2), labels=c(0,1)),
        Illness_6145_death_spouse_partner = factor(Illness_6145_death_spouse_partner, levels=c(1,2), labels=c(0,1)),
        Manic_6156_all_symptoms = factor(Manic_6156_all_symptoms, levels=c(0,1)),
        Manic_6156_more_active = factor(Manic_6156_more_active, levels=c(0,1)),
        Manic_6156_none = factor(Manic_6156_none, levels=c(0,1)),
        Manic_6156_more_talkative = factor(Manic_6156_more_talkative, levels=c(0,1)),
        Manic_6156_less_sleep = factor(Manic_6156_less_sleep, levels=c(0,1)),
        Manic_6156_more_creative = factor(Manic_6156_more_creative, levels=c(0,1)),
        bipolar_depression_20126_none = factor(bipolar_depression_20126_none, levels=c(0,1)),
        bipolar_depression_20126_depression_moderate = factor(bipolar_depression_20126_depression_moderate, levels=c(0,1)),
        bipolar_depression_20126_depression_severe = factor(bipolar_depression_20126_depression_severe, levels=c(0,1)),
        bipolar_depression_20126_bipolar_1 = factor(bipolar_depression_20126_bipolar_1, levels=c(0,1)),
        bipolar_depression_20126_single_depressive_episode = factor(bipolar_depression_20126_single_depressive_episode, levels=c(0,1)),
        bipolar_depression_20126_bipolar_2 = factor(bipolar_depression_20126_bipolar_2, levels=c(0,1)),   
    ) %>%
    filter(InstanceID == 0) %>%
    glimpse()

df_gender = as.data.frame(fread("../gender_score_new/results/all_tp0/gender_score_combined.tsv"))
df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    glimpse()

df_cov = as.data.frame(fread("../../../UKB/tabular/df_body_cov/UKBB_body_cov_wide.tsv"))
df_cov = df_cov %>%
    filter(InstanceID == 0) %>%
    rename(ID = "SubjectID", height = "Standing height_50", assessment_center = "UK Biobank assessment centre_54", weight = "Weight_21002") %>%
    select(ID, assessment_center, height, weight) %>%
    mutate(assessment_center = factor(assessment_center)) %>%
    filter(complete.cases(.)) %>%
    glimpse()

#endregion

#region Associations between gender and mental health

# mental_var = "Mood_swings_1920_0"

assoc_gender = function(mental_var) {
    
    mapping_mental_var = mental_var_mapping %>% filter(Variable == mental_var)

    df_tmp <<- df_mental %>%
        rename(ID = SubjectID) %>%
        select(ID, all_of(mental_var)) %>%
        rename(Mental = all_of(mental_var)) %>%
        inner_join(df_gender, by="ID") %>%
        left_join(df_cov, by="ID") %>%
        mutate(Mental = case_when(is.factor(Mental) ~ as.numeric(Mental) - 1, TRUE ~ as.numeric(Mental))) %>%
        filter(complete.cases(.))

    if (nrow(df_tmp) < 5) { return("Fewer than 5 subjects with data") }

    # Run linear models (regression)
    lm_males = summary(lmer(scale(RankNorm(Mental)) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + (1|assessment_center), data = df_tmp %>% filter(Sex == "Male")))
    lm_females = summary(lmer(scale(RankNorm(Mental)) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + (1|assessment_center), data = df_tmp %>% filter(Sex == "Female")))
    lm_inter = summary(lmer(scale(RankNorm(Mental)) ~ Sex + Gender_score + Gender_score*Sex + bs(Age, degree = 3, df = 4) + (1|assessment_center), data = df_tmp))

    # Save results
    results_i = data.frame(
            Mental_var = mental_var,
            Category = mapping_mental_var %>% pull(Category),
            n_females = nrow(df_tmp %>% filter(Sex == "Female")),
            n_males = nrow(df_tmp %>% filter(Sex == "Male")),
            n_all = nrow(df_tmp),
            # Regression
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

results = list()
i = 1
for (v in 4:ncol(df_mental)) {
    print(colnames(df_mental)[v])
    results[[i]] = assoc_gender(colnames(df_mental)[v])
    i = i + 1
}

results = do.call(rbind, results)
fwrite(results, "./results/results_mental_health.tsv", col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

#endregion
