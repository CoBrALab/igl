
library(data.table)
library(dplyr)
library(tidyverse)
library(matrixStats)

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

var_id = c(
    Date_of_attending_assessment_centre_53_0 = "53",
    genetic_sex_22001 = "22001",
    sex_31 = "31",
    age_21003 = "21003",
    own_rent_accommodation_680="680",
    num_in_household_709="709",
    length_work_week_767="767",
    job_walking_standing_806="806",
    job_physical_work_816="816",
    job_shift_work_826="826",
    num_days_walking_864="864",
    duration_walks_874="874",
    num_days_moderate_activity_884="884",
    duration_moderate_activity_894="894",
    num_days_vigorus_activity_904="904",
    duration_vigorus_activity_914="914",
    freq_friend_family_visits_1031="1031",
    time_outdoors_summer_1050="1050",
    time_outdoors_winter_1060="1060",
    time_watching_televison_1070="1070",
    time_computer_1080="1080",
    time_driving_1090="1090",
    lenth_mobile_phone_use_1110="1110",
    sleep_duration_1160="1160",
    getting_up_in_morning_1170="1170",
    sleep_chronotype_1180="1180",
    nap_during_day_1190="1190",
    sleeplessness_1200="1200",
    daytime_sleeping_narcolepsy_1220="1220",
    cooked_vegetable_intake_1289="1289",
    raw_vegetable_intake_1299="1299",
    fresh_fruit_intake_1309="1309",
    dried_fruit_intake_1319="1319",
    oily_fish_intake_1329="1329",
    non_oily_fish_intake_1339="1339",
    processed_meat_intake_1349="1349",
    poultry_intake_1359="1359",
    beef_intake_1369="1369",
    lamb_mutton_intake_1379="1379",
    pork_intake_1389="1389",
    cheese_intake_1408="1408",
    bread_intake_1438="1438",
    cereal_intake_1458="1458",
    salt_added_1478="1478",
    tea_intake_1488="1488",
    coffee_intake_1498="1498",
    water_intake_1528="1528",
    variation_diet_1548="1548",
    alcohol_intake_freq_1558="1558",
    able_to_confide_2110="2110",
    age_first_intercourse_2139="2139",
    lifetime_num_sexal_partners_2149="2149",
    play_computer_games_2237="2237",
    use_uv_protection_2267="2267",
    job_satisfaction_4537="4537",
    health_satisfaction_4548="4548",
    family_relationship_satisfaction_4559="4559",
    friendship_satisfaction_4570="4570",
    finances_satisfaction_4581="4581",
    access_private_healthcare_4674="4674",
    smoking_status_20116="20116",
    alcohol_drinker_status_20117="20117",
    pack_years_smoking_20161="20161",
    edu_6138_Alevels_ASlevels_equivalent="6138",
    edu_6138_CSE_equivalent="6138",
    edu_6138_college_unviersity_degree="6138",
    edu_6138_NVQ_HND_HNC_equivalent="6138",
    edu_6138_no_qualifications="6138",
    edu_6138_Olevels_GCSEs_equivalent="6138",
    edu_6138_other_professional_quali_nursing_teaching="6138",
    social_activity_6160_adult_education_class="6160",
    social_activity_6160_no_social_activity="6160",
    social_activity_6160_other_group_activity="6160",
    social_activity_6160_pub_social_club="6160",
    social_activity_6160_religious_group="6160",
    social_activity_6160_sports_club_gym="6160",
    employement_6142_unpaid_voluntary_work="6142",
    employement_6142_full_part_time_student="6142",
    employement_6142_paid_or_self_employed="6142",
    employement_6142_looking_after_home_family="6142",
    employement_6142_retired="6142",
    employement_6142_unable_to_work_sickness_disability="6142",
    employement_6142_unemployed="6142"
)

# Reference dataframe for variables
var_df = data.frame(
        Variable = names(var_categ),
        Variable_clean = as.character(clean_var_names),
        Category = var_categ,
        FieldID = var_id
    )
rownames(var_df) = NULL

fwrite(var_df, "./variable_mapping.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

#endregion

#region Functions

# name_ana = "all_tp3"
# tp = 3

combine_preds = function(name_ana, tp) {

    pred_models = list()

    for (i in 1:5) {
        for (s in 1:5) {
            
            # Predictions

            pred_models_tmp = as.data.frame(fread(paste0("./results/",name_ana,"/perm/impdf_",i,"_seed_",s,"/all_predictions.csv")))
            pred_models_tmp = pred_models_tmp %>%
                left_join(df_raw %>% filter(instanceID == tp) %>% select(ID, sex_31, age_21003, genetic_sex_22001, sex_aneuploidy), by="ID") %>%
                rename(!!paste0("Probability_male_i",i,"_s",s) := "pred")
            
            pred_models[[paste0("Probability_male_i",i,"_s",s)]] = pred_models_tmp %>% select(paste0("Probability_male_i",i,"_s",s))
            pred_models_tmp = pred_models_tmp %>% select(ID, age_21003, sex_31, genetic_sex_22001, sex_aneuploidy, train)
        }
    }

    # Combine predictions across runs (calculate average and sd)
    pred_models_allruns = do.call(cbind, pred_models)
    pred_models = cbind(pred_models_tmp, pred_models_allruns)

    prob_cols = grep("^Probability_male_", names(pred_models), value = TRUE)
    m = as.matrix(pred_models[, prob_cols])
    pred_models$Probability_male_mean <- rowMeans2(m, na.rm = TRUE)
    pred_models$Probability_male_sd   <- rowSds(m, na.rm = TRUE)

    pred_models = pred_models %>%
        rename(Age = "age_21003", Sex = "sex_31", Genetic_sex = "genetic_sex_22001", Sex_aneuploidy = "sex_aneuploidy")

    fwrite(pred_models, paste0("./results/", name_ana, "/gender_score_combined.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
    print(paste0("./results/", name_ana, "/gender_score_combined.tsv"))
}

# name_ana = "all_tp0_diet"
# tp = 0
# preds = "diet"

combine_shap = function(name_ana, tp) {

    shap_exp = list()
    shap_models = list()
    shap_models_heldout = list()

    t = 1
    for (i in 1:5) {
        for (s in 1:5) {
            # For each fold
            for (k in 1:5) {
                # SHAP expected value
                shap_exp_tmp = as.data.frame(fread(paste0("./results/",name_ana,"/perm/impdf_",i,"_seed_",s,"/shap_expectedval_fold",k,".tsv")))
                colnames(shap_exp_tmp) = paste0("impdf_",i,"_seed_",s,"_k_",k)
                shap_exp[[t]] = shap_exp_tmp

                # SHAP values
                shap_models_tmp = as.data.frame(fread(paste0("./results/",name_ana,"/perm/impdf_",i,"_seed_",s,"/shap_prob_fold",k,".tsv")))
                shap_models_tmp = shap_models_tmp %>%
                    mutate(
                        Fold = k,
                        impdf = i,
                        seed = s
                    ) %>%
                    select(ID, Fold, impdf, seed, everything())

                shap_models[[t]] = shap_models_tmp

                # SHAP values on heldout data
                shap_models_heldout_tmp = as.data.frame(fread(paste0("./results/",name_ana,"/perm/impdf_",i,"_seed_",s,"/shap_prob_fold",k,"_heldout.tsv")))
                shap_models_heldout_tmp = shap_models_heldout_tmp %>%
                    mutate(
                        Fold = k,
                        impdf = i,
                        seed = s
                    ) %>%
                    select(ID, Fold, impdf, seed, everything())
                
                shap_models_heldout[[t]] = shap_models_heldout_tmp

                t = t + 1
            }
        }
    }

    # Combine SHAP expected value
    shap_exp = do.call(cbind, shap_exp)

    fwrite(shap_exp, paste0("./results/", name_ana, "/shap_expectedval_combined.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
    print(paste0("./results/", name_ana, "/shap_expectedval_combined.tsv"))

    # Combine SHAP values on held-out data (calculate average across folds)
    shap_models_heldout = do.call(rbind, shap_models_heldout)
    shap_models_heldout = shap_models_heldout %>%
        mutate(
            Fold = factor(Fold),
            impdf = factor(impdf),
            seed = factor(seed)
        ) %>%
        arrange(ID, Fold, impdf, seed) %>%
        rename_with(~ paste0("shap_", .), .cols = -c(ID, Fold, impdf, seed))
    
    shap_models_heldout = as.data.table(shap_models_heldout)
    shap_models_heldout = as.data.frame(shap_models_heldout[, lapply(.SD, mean, na.rm = TRUE), by = c("ID", "impdf", "seed"), .SDcols = !c("Fold")])
    shap_models_heldout = shap_models_heldout %>%
        mutate(Fold = 1, heldout = "Yes") %>%
        select(ID, Fold, impdf, seed, heldout, everything())

    # Combine SHAP values (calculate average)
    shap_models = do.call(rbind, shap_models)
    shap_models = shap_models %>%
        mutate(
            Fold = factor(Fold),
            impdf = factor(impdf),
            seed = factor(seed)
        ) %>%
        arrange(ID, Fold, impdf, seed) %>%
        mutate(heldout = "No") %>%
        rename_with(~ paste0("shap_", .), .cols = -c(ID, Fold, impdf, seed, heldout)) %>%
        select(ID, Fold, impdf, seed, heldout, everything())

    shap_models = rbind(shap_models, shap_models_heldout)
    shap_models = shap_models %>%
        arrange(ID, Fold, impdf, seed) %>%
        mutate(heldout = factor(heldout, levels=c("No", "Yes")))

    shap_models_avg = as.data.table(shap_models)
    shap_models_avg = as.data.frame(shap_models_avg[, lapply(.SD, mean, na.rm = TRUE), by = ID, .SDcols = !c("Fold", "impdf", "seed", "heldout")])
    shap_models_sd = as.data.table(shap_models)
    shap_models_sd = as.data.frame(shap_models_sd[, lapply(.SD, sd, na.rm = TRUE), by = ID, .SDcols = !c("Fold", "impdf", "seed", "heldout")])
    
    fwrite(shap_models, paste0("./results/", name_ana, "/shap_values_combined.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
    fwrite(shap_models_avg, paste0("./results/", name_ana, "/shap_values_avg.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
    fwrite(shap_models_sd, paste0("./results/", name_ana, "/shap_values_sd.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

    print(paste0("./results/", name_ana, "/shap_values_combined.tsv"))
    print(paste0("./results/", name_ana, "/shap_values_avg.tsv"))
    print(paste0("./results/", name_ana, "/shap_values_sd.tsv"))
}

# name_ana = "all_tp3"
# tp = 3

combine_inter = function(name_ana, tp) {
    
    t = 1
    for (i in 1:5) {
        for (s in 1:5) {
            for (k in 1:5) {
                df_tmp = as.data.frame(fread(paste0("results/",name_ana,"/perm/impdf_",i,"_seed_",s,"/interactions_fold",k,".tsv")))
                colnames(df_tmp)[3] = paste0("Inter_k",k,"_s",s,"_i",i)

                if (t == 1) {
                    inter_models = df_tmp
                } else {
                    inter_models = inter_models %>%
                        left_join(df_tmp, by=c("Feature1", "Feature2"))
                }

                t = t + 1
            }
        }
    }

    # Calculate mean and SD across iterations
    inter_models = inter_models %>%
        rowwise() %>%
        # Replace all NAs with 0s
        mutate(across(starts_with("Inter"), ~replace_na(.x, 0))) %>%
        # Calculate mean and SD across runs
        mutate(
            Inter_mean = mean(c_across(starts_with("Inter")), na.rm = TRUE),
            Inter_sd   = sd(c_across(starts_with("Inter")), na.rm = TRUE)
        ) %>%
        ungroup() %>%
        # Variable mappings
        left_join(
            var_df %>%
                rename(Feature1 = "Variable", Variable_clean_1 = "Variable_clean", Category_1 = "Category"),
            by="Feature1"
        ) %>%
        left_join(
            var_df %>%
                rename(Feature2 = "Variable", Variable_clean_2 = "Variable_clean", Category_2 = "Category"),
            by="Feature2"
        ) %>%
        select(Feature1, Variable_clean_1, Category_1, Feature2, Variable_clean_2, Category_2, Inter_mean, Inter_sd, everything()) %>%
        arrange(desc(Inter_mean))
    
    fwrite(inter_models, paste0("./results/", name_ana, "/interactions_combined.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
    print(paste0("./results/", name_ana, "/interactions_combined.tsv"))
}

#endregion

#region Combine data for each gender score

name_analyses = c(
    "all_tp0", "all_tp1", "all_tp2", "all_tp3",
    # Matched
    "all_tp0_match", "all_tp1_match", "all_tp2_match", "all_tp3_match",
    "eth_white_tp0_match", "eth_asian_tp0_match", "eth_black_tp0_match",
    "eth_chinese_tp0_match", "eth_southasian_tp0_match",
    "eth_caribbean_tp0_match", "eth_african_tp0_match",
    "gen_silent_tp0_match", "gen_boom1_tp0_match", "gen_boom2_tp0_match", "gen_genx_tp0_match",
    "income_1_tp0_match", "income_2_tp0_match", "income_3_tp0_match", "income_4_tp0_match", "income_5_tp0_match"
)

timepoints = c(
    0,1,2,3,
    # Matched
    0,1,2,3,
    0,0,0,
    0,0,
    0,0,
    0,0,0,0,
    0,0,0,0,0,
    )

for (a in 1:length(name_analyses)) {

    print(name_analyses[a])
    combine_preds(name_analyses[a], timepoints[a])
    combine_shap(name_analyses[a], timepoints[a])
    combine_inter(name_analyses[a], timepoints[a])

}

#endregion

