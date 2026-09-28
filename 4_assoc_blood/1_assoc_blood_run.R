
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
library(future)
library(future.apply)
library(progressr)
library(scales)
library(ggbreak)

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

# Cleaned blood biomarkers
df_blood = as.data.frame(fread("../../../UKB/Analyses/clean_blood/results/clean_blood_raw.tsv"))

# Gender scores
df_gender = as.data.frame(fread("../gender_score_new/results/all_tp0/gender_score_combined.tsv"))

# Baseline data
var_df = as.data.frame(fread("../gender_score/variable_mapping.tsv"))
df_raw = as.data.frame(fread("../impute_input/results/df_imputed_tp_all_combined.tsv"))

# Body covariates (site, height, weight)
df_cov = as.data.frame(fread("../../../UKB/tabular/df_body_cov/UKBB_body_cov_wide.tsv"))

#endregion

#region Clean data

# Blood biomarkers
colnames(df_blood)[seq(3, ncol(df_blood))] = colnames(df_blood)[seq(3, ncol(df_blood))] %>%
    str_replace("_[^_]*$", "") %>%
    str_replace("_[^_]*$", "")

df_blood = df_blood %>%
    rename(Date_time_blood_sample_collected = "Time_blood_sample_collected") %>%
    filter(!is.na(Date_time_blood_sample_collected)) %>%
    mutate(
        tp = factor(tp, levels=c(0,1,2,3)),
        Date_time_blood_sample_collected = as.character(Date_time_blood_sample_collected)
    ) %>%
    separate(Date_time_blood_sample_collected, into = c("Date_blood_sample_collected", "Time_blood_sample_collected"), sep = " ") %>%
    mutate(
        Date_blood_sample_collected = as.Date(Date_blood_sample_collected),
        Time_blood_sample_collected = as_hms(Time_blood_sample_collected)
    )

df_blood = df_blood %>%
    # Remove subjects with impossible date of acquisition (n=1...)
    filter(Date_blood_sample_collected > as.Date("2006-01-01")) %>%
    mutate(
        Day_blood_sample_collected = as.integer(format(Date_blood_sample_collected, "%j"))
    ) %>%
    filter(tp == 0) %>%
    glimpse()

df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    glimpse()

df_cov = df_cov %>%
    filter(InstanceID == 0) %>%
    rename(ID = "SubjectID", height = "Standing height_50", assessment_center = "UK Biobank assessment centre_54", weight = "Weight_21002") %>%
    select(ID, assessment_center, height, weight) %>%
    mutate(assessment_center = factor(assessment_center)) %>%
    filter(complete.cases(.)) %>%
    glimpse()

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
    )

#endregion

#region Associations between gender and blood biomarkers

# Function to relate blood biomarkers to gender score

# name_field = "Testosterone"
# out="test.png"

assoc_gender = function(name_field, out, make_plot=FALSE) {
    
    df_tmp <<- df_blood %>%
        select(ID, Day_blood_sample_collected, Time_blood_sample_collected, all_of(name_field)) %>%
        rename(Blood = all_of(name_field)) %>%
        left_join(df_gender, by="ID") %>%
        left_join(df_cov, by="ID") %>%
        filter(complete.cases(.)) %>%
        mutate(Time_blood_sample_collected = as.numeric(Time_blood_sample_collected))
    
    if (nrow(df_tmp) < 5) {
        message("Skipping ", name_field, ": <5 observations")
        return(NA)
    }

    # Run linear models
    lm_males = summary(lmer(scale(Blood) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + height + weight + bs(Day_blood_sample_collected, degree = 3, df = 4) + bs(Time_blood_sample_collected, degree = 3, df = 4) + (1|assessment_center), data = df_tmp %>% filter(Sex == "Male")))
    lm_females = summary(lmer(scale(Blood) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + height + weight + bs(Day_blood_sample_collected, degree = 3, df = 4) + bs(Time_blood_sample_collected, degree = 3, df = 4) + (1|assessment_center), data = df_tmp %>% filter(Sex == "Female")))
    lm_inter = summary(lmer(scale(Blood) ~ Sex + Gender_score + Gender_score*Sex + bs(Age, degree = 3, df = 4) + height + weight + bs(Day_blood_sample_collected, degree = 3, df = 4) + bs(Time_blood_sample_collected, degree = 3, df = 4) + (1|assessment_center), data = df_tmp))

    results_i = data.frame(
            Blood_marker = name_field,
            n_females = nrow(df_tmp %>% filter(Sex == "Female")),
            n_males = nrow(df_tmp %>% filter(Sex == "Male")),
            n_all = nrow(df_tmp),
            coef_males = lm_males$coefficients["scale(Gender_score)", "Estimate"],
            pval_males = lm_males$coefficients["scale(Gender_score)", "Pr(>|t|)"],
            coef_females = lm_females$coefficients["scale(Gender_score)", "Estimate"],
            pval_females = lm_females$coefficients["scale(Gender_score)", "Pr(>|t|)"],
            coef_inter = lm_inter$coefficients["SexMale:Gender_score","Estimate"],
            pval_inter = lm_inter$coefficients["SexMale:Gender_score","Pr(>|t|)"],
            coef_sexdiff = lm_inter$coefficients["SexMale", "Estimate"],
            pval_sexdiff = lm_inter$coefficients["SexMale", "Pr(>|t|)"],
            coef_igl_female = lm_inter$coefficients["Gender_score", "Estimate"],
            coef_igl_male = lm_inter$coefficients["Gender_score", "Estimate"] + lm_inter $coefficients["SexMale:Gender_score", "Estimate"]
        )
    
    # Plot gender associations
    if (make_plot == TRUE) {
        lm_plot = lmer(Blood ~ Gender_score * Sex + bs(Age, degree = 3, df = 4) + height + weight + bs(Day_blood_sample_collected, degree = 3, df = 4) + bs(Time_blood_sample_collected, degree = 3, df = 4) + (1|assessment_center), data = df_tmp)

        fit_data = as.data.frame(Effect(c("Gender_score", "Sex"), lm_plot, data = model.frame(lm_plot), xlevels=100))

        g = rasterGrob(t(scale_gender_cont[seq(20,80)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

        lm_labels = c(
                Female=paste0(
                    "Interaction: p = ",round(results_i$pval_inter,4),
                    "\nFemale: std(B) = ", round(results_i$coef_females,4), "; p = ", round(results_i$pval_females,4)
                    ),
                Male=paste0("Male: std(B) = ", round(results_i$coef_males,4), "; p = ", round(results_i$pval_males,4))
            )
        
        plt = ggplot(fit_data, aes(x=Gender_score, y=fit, color=Sex, fill=Sex)) +
            annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
            geom_line(size=2) +
            geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2) +
            scale_x_continuous(name="IGL", breaks=c(0,0.5,1)) +
            scale_y_continuous(name="Value") +
            scale_fill_manual(values=scale_female_male_dis, guide="none") +
            scale_color_manual(values=scale_female_male_dis, labels=lm_labels) +
            ggtitle(paste0(name_field, "\nn = ", results_i$n_all, " (M",results_i$n_males,"; F",results_i$n_females,")")) +
            theme_light() + 
            theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5, size=10, face="bold"),
                legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size=10),
                strip.text = element_text(size = 15, color="black", face="bold"), strip.background = element_rect(fill="#ffffff")) +
            guides(color = guide_legend(ncol = 1, byrow = TRUE))
        
        wdt = 4
        hgt = 5

        if (name_field == "Testosterone") {
            plt = plt + 
                scale_y_break(c(1.2, 12), scales=1, space=0, ticklabels = NULL) + 
                labs(x = NULL, y = "Value") +
                scale_x_continuous(name="", breaks=c(0, 0.5, 1)) +
                theme(
                    axis.title.x = element_text(size=0),
                    axis.text.y.right  = element_blank(),
                    axis.ticks.y.right = element_blank(),
                    axis.title.y.right = element_blank()
                )
            wdt = 4.15
            hgt = 4.92

        }

        ggsave(out, width=wdt, height=hgt)
        print(out)
    }

    return(results_i)
}

# Run

results = list()

columns_blood = seq(5, 65)
i=1
for (c in columns_blood) {
    blood_name = colnames(df_blood)[c]
    print(blood_name)
    results[[i]] = assoc_gender(blood_name, paste0("./visualization/indiv/",blood_name,".png"), make_plot=FALSE)
    i = i + 1
}

results = Filter(is.data.frame, results)
results = do.call(rbind, results)

fwrite(results, "./results/lm_results.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

#endregion
