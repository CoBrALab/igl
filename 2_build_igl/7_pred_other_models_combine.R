
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
library(ggrepel)
library(psych)
library(lme4)
library(ggpattern)
library(effects)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#B34700")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#FFA754"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#FFA754")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load input data

df_raw = as.data.frame(fread("../impute_input/results/df_imputed_tp_all_combined.tsv"))

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

# Clean variable names
clean_var_names = c(
        Date_of_attending_assessment_centre_53_0 = "Date data acquired",
        sex_31 = "Sex (self-report)",
        genetic_sex_22001 = "Genetic sex",
        age_21003 = "Age",
        own_rent_accommodation_680="Own accomodation",
        num_in_household_709="Number People in Household",
        length_work_week_767="Length of Work Week",
        job_walking_standing_806="Standing/walking Job",
        job_physical_work_816="Physical Job",
        job_shift_work_826="Shift Job",
        num_days_walking_864="Days/Week Walking",
        duration_walks_874="Duration Walks",
        num_days_moderate_activity_884="Days/Week Moderate Activity",
        duration_moderate_activity_894="Duration Moderate Activity",
        num_days_vigorus_activity_904="Days/Week Vigorous Activity",
        duration_vigorus_activity_914="Duration Vigorous Activity",
        freq_friend_family_visits_1031="Freq. Friend/Family Visits",
        time_outdoors_summer_1050="Time Outdoors (Summer)",
        time_outdoors_winter_1060="Time Outdoors (Winter)",
        time_watching_televison_1070="Time Watching TV",
        time_computer_1080="Time on Computer",
        time_driving_1090="Time Driving",
        lenth_mobile_phone_use_1110="Length Mobile Phone Use",
        sleep_duration_1160="Sleep Duration",
        getting_up_in_morning_1170="Ease Getting up in Morning",
        sleep_chronotype_1180="Morning Person",
        nap_during_day_1190="Nap During Day",
        sleeplessness_1200="Sleeplessness",
        daytime_sleeping_narcolepsy_1220="Daytime Sleepiness",
        cooked_vegetable_intake_1289="Cooked Vegetables",
        raw_vegetable_intake_1299="Raw Vegetables",
        fresh_fruit_intake_1309="Fresh Fruits",
        dried_fruit_intake_1319="Dried Fruits",
        oily_fish_intake_1329="Oily Fish",
        non_oily_fish_intake_1339="Non-oily Fish",
        processed_meat_intake_1349="Processed Meat",
        poultry_intake_1359="Poultry",
        beef_intake_1369="Beef",
        lamb_mutton_intake_1379="Lamb/Mutton",
        pork_intake_1389="Pork",
        cheese_intake_1408="Cheese",
        bread_intake_1438="Bread",
        cereal_intake_1458="Cereal",
        salt_added_1478="Salt added",
        tea_intake_1488="Tea",
        coffee_intake_1498="Coffee",
        water_intake_1528="Water",
        variation_diet_1548="Variations in Diet",
        alcohol_intake_freq_1558="Alcohol Intake Freq.",
        able_to_confide_2110="Able to Confide",
        age_first_intercourse_2139="Age at First Intecourse",
        lifetime_num_sexal_partners_2149="Lifetime Sexual Partners",
        play_computer_games_2237="Play Computer Games",
        use_uv_protection_2267="Use UV Protection",
        job_satisfaction_4537="Job Satisfaction",
        health_satisfaction_4548="Health Satisfaction",
        family_relationship_satisfaction_4559="Family Relationship Satisfaction",
        friendship_satisfaction_4570="Friendship Satisfaction",
        finances_satisfaction_4581="Finances Satisfaction",
        access_private_healthcare_4674="Access Private Healthcare",
        smoking_status_20116="Smoking Status",
        alcohol_drinker_status_20117="Alcohol Drinker Status",
        pack_years_smoking_20161="Pack Years Smoking",
        edu_6138_Alevels_ASlevels_equivalent="High School (A/AS)",
        edu_6138_CSE_equivalent="High School (CSE)",
        edu_6138_college_unviersity_degree="University Degree",
        edu_6138_NVQ_HND_HNC_equivalent="College (NVQ, HND, HNC)",
        edu_6138_no_qualifications="No Qualifications",
        edu_6138_Olevels_GCSEs_equivalent="High School (O/GCSE)",
        edu_6138_other_professional_quali_nursing_teaching="Professional Degree",
        social_activity_6160_adult_education_class="Take Adult Education Classes",
        social_activity_6160_no_social_activity="No Social Activity",
        social_activity_6160_other_group_activity="Other Group Activity",
        social_activity_6160_pub_social_club="Attend Pub/Social Club",
        social_activity_6160_religious_group="Attend Religious Activity",
        social_activity_6160_sports_club_gym="In Sports Club or Gym",
        employement_6142_unpaid_voluntary_work="Volunteer",
        employement_6142_full_part_time_student="Student",
        employement_6142_paid_or_self_employed="Employed",
        employement_6142_looking_after_home_family="Family/Home Caretaker",
        employement_6142_retired="Retired",
        employement_6142_unable_to_work_sickness_disability="On Disability Leave",
        employement_6142_unemployed="Unemployed"
    )

# Variable categories
var_categ = c(
        Date_of_attending_assessment_centre_53_0 = "None",
        genetic_sex_22001 = "None",
        sex_31 = "None",
        age_21003 = "None",
        own_rent_accommodation_680="Socioeconomic",
        num_in_household_709="Social",
        length_work_week_767="Socioeconomic",
        job_walking_standing_806="Socioeconomic",
        job_physical_work_816="Socioeconomic",
        job_shift_work_826="Socioeconomic",
        num_days_walking_864="Lifestyle",
        duration_walks_874="Lifestyle",
        num_days_moderate_activity_884="Lifestyle",
        duration_moderate_activity_894="Lifestyle",
        num_days_vigorus_activity_904="Lifestyle",
        duration_vigorus_activity_914="Lifestyle",
        freq_friend_family_visits_1031="Social",
        time_outdoors_summer_1050="Lifestyle",
        time_outdoors_winter_1060="Lifestyle",
        time_watching_televison_1070="Lifestyle",
        time_computer_1080="Lifestyle",
        time_driving_1090="Lifestyle",
        lenth_mobile_phone_use_1110="Lifestyle",
        sleep_duration_1160="Lifestyle",
        getting_up_in_morning_1170="Lifestyle",
        sleep_chronotype_1180="Lifestyle",
        nap_during_day_1190="Lifestyle",
        sleeplessness_1200="Lifestyle",
        daytime_sleeping_narcolepsy_1220="Lifestyle",
        cooked_vegetable_intake_1289="Diet",
        raw_vegetable_intake_1299="Diet",
        fresh_fruit_intake_1309="Diet",
        dried_fruit_intake_1319="Diet",
        oily_fish_intake_1329="Diet",
        non_oily_fish_intake_1339="Diet",
        processed_meat_intake_1349="Diet",
        poultry_intake_1359="Diet",
        beef_intake_1369="Diet",
        lamb_mutton_intake_1379="Diet",
        pork_intake_1389="Diet",
        cheese_intake_1408="Diet",
        bread_intake_1438="Diet",
        cereal_intake_1458="Diet",
        salt_added_1478="Diet",
        tea_intake_1488="Diet",
        coffee_intake_1498="Diet",
        water_intake_1528="Diet",
        variation_diet_1548="Diet",
        alcohol_intake_freq_1558="Lifestyle",
        able_to_confide_2110="Social",
        age_first_intercourse_2139="Social",
        lifetime_num_sexal_partners_2149="Social",
        play_computer_games_2237="Lifestyle",
        use_uv_protection_2267="Lifestyle",
        job_satisfaction_4537="Socioeconomic",
        health_satisfaction_4548="Socioeconomic",
        family_relationship_satisfaction_4559="Social",
        friendship_satisfaction_4570="Social",
        finances_satisfaction_4581="Socioeconomic",
        access_private_healthcare_4674="Socioeconomic",
        smoking_status_20116="Lifestyle",
        alcohol_drinker_status_20117="Lifestyle",
        pack_years_smoking_20161="Lifestyle",
        edu_6138_Alevels_ASlevels_equivalent="Socioeconomic",
        edu_6138_CSE_equivalent="Socioeconomic",
        edu_6138_college_unviersity_degree="Socioeconomic",
        edu_6138_NVQ_HND_HNC_equivalent="Socioeconomic",
        edu_6138_no_qualifications="Socioeconomic",
        edu_6138_Olevels_GCSEs_equivalent="Socioeconomic",
        edu_6138_other_professional_quali_nursing_teaching="Socioeconomic",
        social_activity_6160_adult_education_class="Social",
        social_activity_6160_no_social_activity="Social",
        social_activity_6160_other_group_activity="Social",
        social_activity_6160_pub_social_club="Social",
        social_activity_6160_religious_group="Social",
        social_activity_6160_sports_club_gym="Social",
        employement_6142_unpaid_voluntary_work="Socioeconomic",
        employement_6142_full_part_time_student="Socioeconomic",
        employement_6142_paid_or_self_employed="Socioeconomic",
        employement_6142_looking_after_home_family="Socioeconomic",
        employement_6142_retired="Socioeconomic",
        employement_6142_unable_to_work_sickness_disability="Socioeconomic",
        employement_6142_unemployed="Socioeconomic"
    )

# Reference dataframe for variables
var_df = data.frame(
        Variable = names(var_categ),
        Variable_clean = as.character(clean_var_names),
        Category = var_categ
    )
rownames(var_df) = NULL

fwrite(var_df, "./variable_mapping.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

#endregion

#region Load results

gender_list = c(
    "all_tp0", "all_tp1", "all_tp2", "all_tp3",
    # Match
    "all_tp0_match", "all_tp1_match", "all_tp2_match", "all_tp3_match",
    "eth_white_tp0_match", "eth_black_tp0_match", "eth_asian_tp0_match", "eth_chinese_tp0_match", "eth_southasian_tp0_match", "eth_african_tp0_match", "eth_caribbean_tp0_match",
    "gen_silent_tp0_match", "gen_boom1_tp0_match", "gen_boom2_tp0_match", "gen_genx_tp0_match",
    "income_1_tp0_match", "income_2_tp0_match", "income_3_tp0_match", "income_4_tp0_match", "income_5_tp0_match"             
    )

for (g in 1:length(gender_list)) {
    print(gender_list[g])

    # Load predictions for other ethnicity
    pred_out = list()
    i = 1
    for (f in 1:5) {
        for (impdf in 1:5) {
            for (s in 1:5) {
                pred_out[[i]] = as.data.frame(fread(paste0("results/preds_",gender_list[g],"/perm/preds_fold",f-1,"_impdf",impdf,"_seed",s,".tsv")))
                pred_out[[i]] = pred_out[[i]] %>%
                    rename(Probability_male = "prediction_0_for_Female_1_for_Male") %>%
                    select(ID, Probability_male) %>%
                    mutate(Fold = f, Impdf = impdf, Seed = s)
                i = i + 1
            }
        }
    }
    pred_out = do.call(rbind, pred_out)

    pred_out = pred_out %>%
        group_by(ID) %>%
        summarize(
            Probability_male_mean = mean(Probability_male),
            Probability_male_sd = sd(Probability_male),
        ) %>%
        ungroup() %>%
        mutate(train = "left_out") %>%
        left_join(df_raw %>% filter(instanceID == 0) %>% select(ID, sex_31, genetic_sex_22001, age_21003, sex_aneuploidy), by="ID") %>%
        rename(Age = "age_21003", Sex = "sex_31", Genetic_sex = "genetic_sex_22001", Sex_aneuploidy = "sex_aneuploidy") %>%
        select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, train, Probability_male_mean, Probability_male_sd)

    # Load prediction for ethnicity of interest
    pred_in = as.data.frame(fread(paste0("../gender_score_new/results/",gender_list[g],"/gender_score_combined.tsv")))
    pred_in = pred_in %>%
        select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, train, Probability_male_mean, Probability_male_sd)

    pred_all = rbind(pred_in, pred_out)
    pred_all = pred_all %>%
        arrange(ID) %>%
        mutate(model = gender_list[g])

    fwrite(pred_all, paste0("./results/preds_",gender_list[g],"/preds_combined.tsv"), row.names = FALSE, col.names=TRUE, quote = FALSE, sep="\t")
}

#endregion
