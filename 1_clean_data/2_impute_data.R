
library(data.table)
library(tidyverse)
library(dplyr)
library(cli)
library(htmlwidgets)
library(usethis)
library(devtools)
# install.packages("mixgb", lib="/home/parent41/R/x86_64-pc-linux-gnu-library/4.4")
library(mixgb)
library(readr)

print("Load data")

#region Load data

# Read the data from the specified input file
df_gene = as.data.frame(fread("../../../UKB/tabular/df_genetic_cov/UKBB_genetic_cov_wider.tsv"))

df_gene = df_gene %>%
    rename(ID = "SubjectID", sex_aneuploidy = "Sex_chromosome_aneuploidy_22019_0") %>%
    select(ID, sex_aneuploidy) %>%
    mutate(sex_aneuploidy = ifelse(sex_aneuploidy == "Yes", 1, 0)) %>%
    glimpse()

df = as.data.frame(fread("../../../UKB/Analyses/clean_lifestyle_gender/results/lifestyle_clean_coded.tsv"))

df = df %>% 
    left_join(df_gene, by="ID") %>%
    mutate(
        instanceID = factor(instanceID, levels=c(0,1,2,3)),
        Date_of_attending_assessment_centre_53_0 = as.Date(Date_of_attending_assessment_centre_53_0),
        sex_31 = factor(sex_31, levels=c(0,1), labels=c("Female", "Male")),
        genetic_sex_22001 = factor(genetic_sex_22001, levels=c(0,1), labels=c("Female", "Male")),
        own_rent_accommodation_680 = factor(own_rent_accommodation_680),
        # income_before_tax_738 = factor(income_before_tax_738),
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
    filter(!is.na(genetic_sex_22001))

#endregion

#region Functions

remove_impossible_values = function(df_func) {
    df_func = df_func %>% mutate(
        num_in_household_709 = ifelse(num_in_household_709<0, 0, num_in_household_709),
        length_work_week_767 = ifelse(length_work_week_767<0, 0, length_work_week_767),
        num_days_walking_864 = ifelse(num_days_walking_864<0, 0, num_days_walking_864),
        duration_walks_874 = ifelse(duration_walks_874<0, 0, duration_walks_874),
        num_days_moderate_activity_884 = ifelse(num_days_moderate_activity_884<0, 0, num_days_moderate_activity_884),
        num_days_moderate_activity_884 = ifelse(num_days_moderate_activity_884>7, 7, num_days_moderate_activity_884),
        duration_moderate_activity_894 = ifelse(duration_moderate_activity_894<0, 0, duration_moderate_activity_894),
        num_days_vigorus_activity_904 = ifelse(num_days_vigorus_activity_904<0, 0, num_days_vigorus_activity_904),
        num_days_vigorus_activity_904 = ifelse(num_days_vigorus_activity_904>7, 7, num_days_vigorus_activity_904),
        duration_vigorus_activity_914 = ifelse(duration_vigorus_activity_914<0, 0, duration_vigorus_activity_914),
        time_outdoors_summer_1050 = ifelse(time_outdoors_summer_1050<0, 0, ifelse(time_outdoors_summer_1050>24,24,time_outdoors_summer_1050)),
        time_outdoors_winter_1060 = ifelse(time_outdoors_winter_1060<0, 0, ifelse(time_outdoors_winter_1060>24,24,time_outdoors_winter_1060)),
        time_watching_televison_1070 = ifelse(time_watching_televison_1070<0, 0, ifelse(time_watching_televison_1070>24,24,time_watching_televison_1070)),
        time_computer_1080 = ifelse(time_computer_1080<0, 0, ifelse(time_computer_1080>24,24,time_computer_1080)),
        time_driving_1090 = ifelse(time_driving_1090<0, 0, ifelse(time_driving_1090>24,24,time_driving_1090)),
        sleep_duration_1160 = ifelse(sleep_duration_1160<0, 0, ifelse(sleep_duration_1160>24,24,sleep_duration_1160)),
        cooked_vegetable_intake_1289 = ifelse(cooked_vegetable_intake_1289<0, 0, cooked_vegetable_intake_1289),
        raw_vegetable_intake_1299 = ifelse(raw_vegetable_intake_1299<0, 0, raw_vegetable_intake_1299),
        fresh_fruit_intake_1309 = ifelse(fresh_fruit_intake_1309<0, 0, fresh_fruit_intake_1309),
        dried_fruit_intake_1319 = ifelse(dried_fruit_intake_1319<0, 0, dried_fruit_intake_1319),
        bread_intake_1438 = ifelse(bread_intake_1438<0, 0, bread_intake_1438),
        cereal_intake_1458 = ifelse(cereal_intake_1458<0, 0, cereal_intake_1458),
        tea_intake_1488 = ifelse(tea_intake_1488<0, 0, tea_intake_1488),
        coffee_intake_1498 = ifelse(coffee_intake_1498<0, 0, coffee_intake_1498),
        water_intake_1528 = ifelse(water_intake_1528<0, 0, water_intake_1528),
        age_first_intercourse_2139 = ifelse(age_first_intercourse_2139<0, 0, age_first_intercourse_2139),
        lifetime_num_sexal_partners_2149 = ifelse(lifetime_num_sexal_partners_2149<0, 0, lifetime_num_sexal_partners_2149),
        pack_years_smoking_20161 = ifelse(pack_years_smoking_20161<0, 0, pack_years_smoking_20161)
    )

    return(df_func)
}

# Combine imputed dataframes (mean for numeric variables, mode for factor variables)
combine_imputations <- function(imputed_list) {
    # Number of imputations
    m <- length(imputed_list)
    
    # Get variable names
    vars <- names(imputed_list[[1]])
    
    # Initialize combined dataset
    combined <- imputed_list[[1]]  # start from first one
    
    for (v in vars) {
        vals <- lapply(imputed_list, function(df) df[[v]])
        
        if (is.numeric(vals[[1]])) {
        # Numeric: take mean across imputations
        combined[[v]] <- rowMeans(do.call(cbind, vals))
        
        } else if (is.factor(vals[[1]]) || is.character(vals[[1]])) {
        # Factor or character: take mode across imputations
        combined[[v]] <- apply(
            do.call(cbind, lapply(vals, as.character)),
            1,
            function(x) {
            ux <- unique(x)
            ux[which.max(tabulate(match(x, ux)))]
            }
        )
        # Preserve factor levels if the original was a factor
        if (is.factor(vals[[1]])) {
            combined[[v]] <- factor(combined[[v]], levels = levels(vals[[1]]))
        }
        }
    }
    
    return(combined)
}


#endregion

#region Run imputation

# Impute for each timepoint seperately
timepoints = seq(0,3)
n_imp = 5 # Number of multiple imputations

df_imputed = list()
for (i in 4:length(timepoints)) {
    print(i)

    # Train impute model removing age and sex, and participants with incongruent sex or sex aneuploidy
    df_train = df %>%
        filter(instanceID == timepoints[i]) %>%
        # filter(sex_aneuploidy == 0) %>%
        # filter(sex_31 == genetic_sex_22001) %>%
        select(-c(ID, instanceID, Date_of_attending_assessment_centre_53_0, sex_31, genetic_sex_22001, age_21003, sex_aneuploidy))

    df_train_other_cols = df %>%
        filter(instanceID == timepoints[i]) %>%
        # filter(sex_aneuploidy == 0) %>%
        # filter(sex_31 == genetic_sex_22001) %>%
        select(c(ID, instanceID, Date_of_attending_assessment_centre_53_0, sex_31, genetic_sex_22001, age_21003, sex_aneuploidy))
    
    # Train model
    df_train = data_clean(df_train)
    impute_model = mixgb(data = df_train, m = n_imp, verbose=TRUE, save.models = TRUE)

    # Remove impossible values from imputed datasets
    imputed_clean <- lapply(impute_model$imputed.data, remove_impossible_values)

    # Save individual imputed datasets
    for (m in 1:n_imp) {
        df_train_imp = as.data.frame(impute_model$imputed.data[[m]])
        df_train_imp = cbind(df_train_other_cols, df_train_imp)
        fwrite(df_train_imp, paste0("./results/df_imputed_tp_",timepoints[i],"_m",m,".tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
    }

    # Combine imputed datasets
    df_combined = combine_imputations(imputed_clean)
    fwrite(df_train_imp, paste0("./results/df_imputed_tp_",timepoints[i],"_combined.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
}

#endregion

#region Combine across timepoints

df_imputed = list()

for (i in 1:4) {
    df_imputed[[i]] = as.data.frame(fread(paste0("./results/df_imputed_tp_",i-1,"_combined.tsv")))
}

df_imputed = do.call(rbind, df_imputed)
df_imputed = df_imputed %>%
    arrange(ID, instanceID)

fwrite(df_imputed, paste0("./results/df_imputed_tp_all_combined.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

#endregion
