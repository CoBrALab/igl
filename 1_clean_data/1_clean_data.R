
library(data.table)
library(dplyr)
library(tidyverse)
library(stringr)

print("Load data")

df = as.data.frame(fread("../../tabular/df_lifestyle_gender/UKBB_lifestyle_gender_wider.tsv"))
df_genetic_sex = as.data.frame(fread("../../tabular/df_genetic_sex/UKBB_genetic_sex_wide.tsv"))

# Replace empty strings with NAs
df[df == ""] <- NA

# Rename columns
df = df %>%
    rename(
        ID = SubjectID,
        instanceID = InstanceID,
        sex_31 = Sex_31_0,
        own_rent_accommodation_680 = Own_or_rent_accommodation_lived_in_680_0,
        num_in_household_709 = Number_in_household_709_0,
        length_work_week_767 = Length_of_working_week_for_main_job_767_0,
        job_walking_standing_806 = Job_involves_mainly_walking_or_standing_806_0,
        job_physical_work_816 = Job_involves_heavy_manual_or_physical_work_816_0,
        job_shift_work_826 = Job_involves_shift_work_826_0,
        num_days_walking_864 = 'Number_of_days/week_walked_10+_minutes_864_0',
        duration_walks_874 = Duration_of_walks_874_0,
        num_days_moderate_activity_884 = 'Number_of_days/week_of_moderate_physical_activity_10+_minutes_884_0',
        duration_moderate_activity_894 = Duration_of_moderate_activity_894_0,
        num_days_vigorus_activity_904 = 'Number_of_days/week_of_vigorous_physical_activity_10+_minutes_904_0',
        duration_vigorus_activity_914 = Duration_of_vigorous_activity_914_0,
        freq_friend_family_visits_1031 = 'Frequency_of_friend/family_visits_1031_0',
        time_outdoors_summer_1050 = Time_spend_outdoors_in_summer_1050_0,
        time_outdoors_winter_1060 = Time_spent_outdoors_in_winter_1060_0,
        time_watching_televison_1070 = 'Time_spent_watching_television_(TV)_1070_0',
        time_computer_1080 = Time_spent_using_computer_1080_0,
        time_driving_1090 = Time_spent_driving_1090_0,
        lenth_mobile_phone_use_1110 = Length_of_mobile_phone_use_1110_0,
        sleep_duration_1160 = Sleep_duration_1160_0,
        getting_up_in_morning_1170 = Getting_up_in_morning_1170_0,
        sleep_chronotype_1180 = 'Morning/evening_person_(chronotype)_1180_0',
        nap_during_day_1190 = Nap_during_day_1190_0,
        sleeplessness_1200 = 'Sleeplessness_/_insomnia_1200_0',
        daytime_sleeping_narcolepsy_1220 = "Daytime_dozing_/_sleeping_(narcolepsy)_1220_0",
        cooked_vegetable_intake_1289 = "Cooked_vegetable_intake_1289_0",
        raw_vegetable_intake_1299 = "Salad_/_raw_vegetable_intake_1299_0",
        fresh_fruit_intake_1309 = "Fresh_fruit_intake_1309_0",
        dried_fruit_intake_1319 = "Dried_fruit_intake_1319_0",
        oily_fish_intake_1329 = "Oily_fish_intake_1329_0",
        non_oily_fish_intake_1339 = "Non-oily_fish_intake_1339_0",
        processed_meat_intake_1349 = "Processed_meat_intake_1349_0",
        poultry_intake_1359 = "Poultry_intake_1359_0",
        beef_intake_1369 = "Beef_intake_1369_0",
        lamb_mutton_intake_1379 = "Lamb/mutton_intake_1379_0",
        pork_intake_1389 = "Pork_intake_1389_0",
        cheese_intake_1408 = "Cheese_intake_1408_0",
        bread_intake_1438 = "Bread_intake_1438_0",
        cereal_intake_1458 = "Cereal_intake_1458_0",
        salt_added_1478 = "Salt_added_to_food_1478_0",
        tea_intake_1488 = "Tea_intake_1488_0",
        coffee_intake_1498 = "Coffee_intake_1498_0",
        water_intake_1528 = "Water_intake_1528_0",
        variation_diet_1548 = "Variation_in_diet_1548_0",
        alcohol_intake_freq_1558 = 'Alcohol_intake_frequency._1558_0',
        able_to_confide_2110 = Able_to_confide_2110_0,
        age_first_intercourse_2139 = "Age_first_had_sexual_intercourse_2139_0",
        lifetime_num_sexal_partners_2149 = Lifetime_number_of_sexual_partners_2149_0,
        play_computer_games_2237 = Plays_computer_games_2237_0,
        use_uv_protection_2267 = "Use_of_sun/uv_protection_2267_0",
        job_satisfaction_4537 = 'Work/job_satisfaction_4537_0',
        health_satisfaction_4548 = Health_satisfaction_4548_0,
        family_relationship_satisfaction_4559 = Family_relationship_satisfaction_4559_0,
        friendship_satisfaction_4570 = Friendships_satisfaction_4570_0,
        finances_satisfaction_4581 = Financial_situation_satisfaction_4581_0,
        access_private_healthcare_4674 = Private_healthcare_4674_0,
        quals_6138_0 = Qualifications_6138_0,
        quals_6138_1 = Qualifications_6138_1,
        quals_6138_2 = Qualifications_6138_2,
        quals_6138_3 = Qualifications_6138_3,
        quals_6138_4 = Qualifications_6138_4,
        quals_6138_5 = Qualifications_6138_5,
        employment_status_6142_0 = Current_employment_status_6142_0,
        employment_status_6142_1 = Current_employment_status_6142_1,
        employment_status_6142_2 = Current_employment_status_6142_2,
        employment_status_6142_3 = Current_employment_status_6142_3,
        employment_status_6142_4 = Current_employment_status_6142_4,
        employment_status_6142_5 = Current_employment_status_6142_5,
        employment_status_6142_6 = Current_employment_status_6142_6,
        social_activites_6160_0 = 'Leisure/social_activities_6160_0',
        social_activites_6160_1 = 'Leisure/social_activities_6160_1',
        social_activites_6160_2 = 'Leisure/social_activities_6160_2',
        social_activites_6160_3 = 'Leisure/social_activities_6160_3',
        social_activites_6160_4 = 'Leisure/social_activities_6160_4',
        smoking_status_20116 = Smoking_status_20116_0,
        alcohol_drinker_status_20117 = Alcohol_drinker_status_20117_0,
        age_21003 = Age_when_attended_assessment_centre_21003_0,
        pack_years_smoking_20161 = Pack_years_of_smoking_20161_0
    ) %>% glimpse()

df_genetic_sex = df_genetic_sex %>%
    rename(ID = SubjectID, genetic_sex_22001 = 'Genetic sex_22001', instanceID = InstanceID) %>%
    select(-ArrayID)

# Insert genetic sex and reorder columns
print("Insert genetic sex and reorder columns")

df = df %>%
    left_join(df_genetic_sex, by=c("ID", "instanceID")) %>% 
    select(ID, instanceID, Date_of_attending_assessment_centre_53_0, sex_31, genetic_sex_22001, age_21003, everything()) %>% glimpse()

# Change some values
print("Change some values")

df = df %>%
    mutate(
        # Qualifications
        quals_6138_0 = ifelse(quals_6138_0 == "None of the above", "No qualifications", quals_6138_0),
        quals_6138_1 = ifelse(quals_6138_1 == "None of the above", "No qualifications", quals_6138_1),
        quals_6138_2 = ifelse(quals_6138_2 == "None of the above", "No qualifications", quals_6138_2),
        quals_6138_3 = ifelse(quals_6138_3 == "None of the above", "No qualifications", quals_6138_3),
        quals_6138_4 = ifelse(quals_6138_4 == "None of the above", "No qualifications", quals_6138_4),
        quals_6138_5 = ifelse(quals_6138_5 == "None of the above", "No qualifications", quals_6138_5),
        # Employement
        employment_status_6142_0 = ifelse(employment_status_6142_0 == "None of the above", NA, employment_status_6142_0),
        employment_status_6142_1 = ifelse(employment_status_6142_1 == "None of the above", NA, employment_status_6142_1),
        employment_status_6142_2 = ifelse(employment_status_6142_2 == "None of the above", NA, employment_status_6142_2),
        employment_status_6142_3 = ifelse(employment_status_6142_3 == "None of the above", NA, employment_status_6142_3),
        employment_status_6142_4 = ifelse(employment_status_6142_4 == "None of the above", NA, employment_status_6142_4),
        employment_status_6142_5 = ifelse(employment_status_6142_5 == "None of the above", NA, employment_status_6142_5),
        employment_status_6142_6 = ifelse(employment_status_6142_6 == "None of the above", NA, employment_status_6142_6),
        # Social activities
        social_activites_6160_0 = ifelse(social_activites_6160_0 == "None of the above", "No social activity", social_activites_6160_0),
        social_activites_6160_1 = ifelse(social_activites_6160_1 == "None of the above", "No social activity", social_activites_6160_1),
        social_activites_6160_2 = ifelse(social_activites_6160_2 == "None of the above", "No social activity", social_activites_6160_2),
        social_activites_6160_3 = ifelse(social_activites_6160_3 == "None of the above", "No social activity", social_activites_6160_3),
        social_activites_6160_4 = ifelse(social_activites_6160_4 == "None of the above", "No social activity", social_activites_6160_4),
    )

# Unite array fields
print("Unite array fields")

df = df %>%
    unite(quals_6138, quals_6138_0:quals_6138_5, sep = "; ", na.rm = TRUE, remove = TRUE) %>%
    unite(employment_status_6142, employment_status_6142_0:employment_status_6142_6, sep = "; ", na.rm = TRUE, remove = TRUE) %>%
    unite(social_activites_6160, social_activites_6160_0:social_activites_6160_4, sep = "; ", na.rm = TRUE, remove = TRUE) %>%
    glimpse()

df[df == ""] <- NA

# Logical imputation of some data fields 
print("Logical imputation of some data fields")

df = df %>%
    mutate(
        length_work_week_767 = ifelse(is.na(length_work_week_767)==TRUE & grepl("Retired", employment_status_6142, fixed=TRUE)==TRUE, 0, length_work_week_767),
        job_walking_standing_806 = ifelse(is.na(job_walking_standing_806)==TRUE & grepl("In paid employment or self-employed", employment_status_6142, fixed=TRUE)==FALSE, 'Never/rarely', job_walking_standing_806),
        job_physical_work_816 = ifelse(is.na(job_physical_work_816)==TRUE & grepl("In paid employment or self-employed", employment_status_6142, fixed=TRUE)==FALSE, 'Never/rarely', job_physical_work_816),
        job_shift_work_826 = ifelse(is.na(job_shift_work_826)==TRUE & grepl("In paid employment or self-employed", employment_status_6142, fixed=TRUE)==FALSE, 'Never/rarely', job_shift_work_826),
        job_satisfaction_4537 = ifelse(is.na(job_satisfaction_4537)==TRUE & grepl("In paid employment or self-employed", employment_status_6142, fixed=TRUE)==FALSE, "Neutral", job_satisfaction_4537),
        pack_years_smoking_20161 = ifelse(is.na(pack_years_smoking_20161)==TRUE & smoking_status_20116 == "Never", 0, pack_years_smoking_20161),
    ) %>% glimpse()

# Remove if age is missing
df = df %>% filter(is.na(age_21003) == FALSE)
df[df == ""] <- NA

# More logical imputation: take value of previous timepoint if NA
print("More logical imputation: take value of previous timepoint if NA")

setDT(df)

df <- df[order(ID, instanceID)][, lapply(.SD, function(x) {
        coalesce(
            x,
            shift(x, type = "lag", n = 1, fill = NA),
            shift(x, type = "lag", n = 2, fill = NA),
            shift(x, type = "lag", n = 3, fill = NA)
        )
    }), by = ID]

df <- df[order(ID, instanceID)][, lapply(.SD, function(x) {
        coalesce(
            x,
            shift(x, type = "lead", n = 1, fill = NA),
            shift(x, type = "lead", n = 2, fill = NA),
            shift(x, type = "lead", n = 3, fill = NA)
        )
    }), by = ID]

df = as.data.frame(df)

# Write and read intermediate results
print("Write intermediate results")

fwrite(df, "./results/lifestyle_clean_inter.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

df = as.data.frame(fread("./results/lifestyle_clean_inter.tsv"))

# Remove participants with >10% of missing data

temp = df
temp = temp %>% 
    select(-c(sex_31, genetic_sex_22001, age_21003, Date_of_attending_assessment_centre_53_0)) %>%
    glimpse()

temp[temp == ""] <- NA

threshold = 0.1

participants_to_remove = temp %>%
    mutate(missing_percent = rowSums(is.na(.)) / (ncol(.)-2)) %>%
    filter(missing_percent > threshold) %>%
    select(ID, instanceID)

temp_0 = temp %>% filter(instanceID == 0, !ID %in% (participants_to_remove %>% filter(instanceID == 0) %>% pull(ID)))
temp_1 = temp %>% filter(instanceID == 1, !ID %in% (participants_to_remove %>% filter(instanceID == 1) %>% pull(ID)))
temp_2 = temp %>% filter(instanceID == 2, !ID %in% (participants_to_remove %>% filter(instanceID == 2) %>% pull(ID)))
temp_3 = temp %>% filter(instanceID == 3, !ID %in% (participants_to_remove %>% filter(instanceID == 3) %>% pull(ID)))

print(paste0("Participants left for tp0 = ", nrow(temp_0)))
print(paste0("Participants left for tp1 = ", nrow(temp_1)))
print(paste0("Participants left for tp2 = ", nrow(temp_2)))
print(paste0("Participants left for tp3 = ", nrow(temp_3)))

print(data.frame(names = colnames(temp_0), NA_perc = round(colSums(is.na(temp_0))/nrow(temp_0)*100, 2)))
print(data.frame(names = colnames(temp_1), NA_perc = round(colSums(is.na(temp_1))/nrow(temp_1)*100, 2)))
print(data.frame(names = colnames(temp_2), NA_perc = round(colSums(is.na(temp_2))/nrow(temp_2)*100, 2)))
print(data.frame(names = colnames(temp_3), NA_perc = round(colSums(is.na(temp_3))/nrow(temp_3)*100, 2)))

df = df %>%
    anti_join(participants_to_remove, by=c("ID", "instanceID")) %>% glimpse()

# One-hot encoding

# Qualitications
df = df %>% 
    mutate(
        edu_6138_Alevels_ASlevels_equivalent = ifelse(grepl("A levels/AS levels or equivalent", quals_6138, fixed=TRUE), 1, 0),
        edu_6138_CSE_equivalent = ifelse(grepl("CSEs or equivalent", quals_6138, fixed=TRUE), 1, 0),
        edu_6138_college_unviersity_degree = ifelse(grepl("College or University degree", quals_6138, fixed=TRUE), 1, 0),
        edu_6138_NVQ_HND_HNC_equivalent = ifelse(grepl("NVQ or HND or HNC or equivalent", quals_6138, fixed=TRUE), 1, 0),
        edu_6138_no_qualifications = ifelse(grepl("No qualifications", quals_6138, fixed=TRUE), 1, 0),
        edu_6138_Olevels_GCSEs_equivalent = ifelse(grepl("O levels/GCSEs or equivalent", quals_6138, fixed=TRUE), 1, 0),
        edu_6138_other_professional_quali_nursing_teaching = ifelse(grepl("Other professional qualifications", quals_6138, fixed=TRUE), 1, 0)
    ) %>% glimpse()

# Social activities
df = df %>% 
    mutate(
        social_activity_6160_adult_education_class = ifelse(grepl("Adult education class", social_activites_6160, fixed=TRUE), 1, 0),
        social_activity_6160_no_social_activity = ifelse(grepl("No social activity", social_activites_6160, fixed=TRUE), 1, 0),
        social_activity_6160_other_group_activity = ifelse(grepl("Other group activity", social_activites_6160, fixed=TRUE), 1, 0),
        social_activity_6160_pub_social_club = ifelse(grepl("Pub or social club", social_activites_6160, fixed=TRUE), 1, 0),
        social_activity_6160_religious_group = ifelse(grepl("Religious group", social_activites_6160, fixed=TRUE), 1, 0),
        social_activity_6160_sports_club_gym = ifelse(grepl("Sports club or gym", social_activites_6160, fixed=TRUE), 1, 0)
    ) %>% glimpse()

# Employement
df = df %>% 
    mutate(
        employement_6142_unpaid_voluntary_work = ifelse(grepl("Doing unpaid or voluntary work", employment_status_6142, fixed=TRUE), 1, 0),
        employement_6142_full_part_time_student = ifelse(grepl("Full or part-time student", employment_status_6142, fixed=TRUE), 1, 0),
        employement_6142_paid_or_self_employed = ifelse(grepl("In paid employment or self-employed", employment_status_6142, fixed=TRUE), 1, 0),
        employement_6142_looking_after_home_family = ifelse(grepl("Looking after home and/or family", employment_status_6142, fixed=TRUE), 1, 0),
        employement_6142_retired = ifelse(grepl("Retired", employment_status_6142, fixed=TRUE), 1, 0),
        employement_6142_unable_to_work_sickness_disability = ifelse(grepl("Unable to work because of sickness or disability", employment_status_6142, fixed=TRUE), 1, 0),
        employement_6142_unemployed = ifelse(grepl("Unemployed", employment_status_6142, fixed=TRUE), 1, 0)
    ) %>% glimpse()

# Remove variables recoded categ variables
df = df %>% select(-c(quals_6138, social_activites_6160, employment_status_6142))

# Save again
df[df == ""] <- NA

fwrite(df, "./results/lifestyle_clean_inter_2.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

df = as.data.frame(fread("./results/lifestyle_clean_inter_2.tsv"))

# Dicotomize own/rent accomodation into own or not
df = df %>%
    mutate(
        own_rent_accommodation_680 = ifelse(grepl("Own", own_rent_accommodation_680, fixed=TRUE), 1, 0)
    ) %>%
    glimpse()

# Ordinal recoding for categorical variables

df = df %>%
    mutate(
        sex_31 = recode(sex_31, "Male" = 1, "Female" = 0),
        genetic_sex_22001 = recode(genetic_sex_22001, "Male" = 1, "Female" = 0),
        job_walking_standing_806 = recode(job_walking_standing_806, "Never/rarely" = 0, "Sometimes" = 1, "Usually" = 2, "Always" = 3),
        job_physical_work_816 = recode(job_physical_work_816, "Never/rarely" = 0, "Sometimes" = 1, "Usually" = 2, "Always" = 3),
        job_shift_work_826 = recode(job_shift_work_826, "Never/rarely" = 0, "Sometimes" = 1, "Usually" = 2, "Always" = 3),
        num_days_walking_864 = as.numeric(ifelse(num_days_walking_864 == "Unable to walk", 0, num_days_walking_864)),
        freq_friend_family_visits_1031 = recode(freq_friend_family_visits_1031, "Never or almost never" = 0, "No friends/family outside household" = 0, "Once every few months" = 1, "About once a month" = 2, "About once a week" = 3, "2-4 times a week" = 4, "Almost daily" = 5),
        time_outdoors_summer_1050 = as.numeric(ifelse(time_outdoors_summer_1050 == "Less than an hour a day", 0, time_outdoors_summer_1050)),
        time_outdoors_winter_1060 = as.numeric(ifelse(time_outdoors_winter_1060 == "Less than an hour a day", 0, time_outdoors_winter_1060)),
        time_watching_televison_1070 = as.numeric(ifelse(time_watching_televison_1070 == "Less than an hour a day", 0, time_watching_televison_1070)),
        time_computer_1080 = as.numeric(ifelse(time_computer_1080 == "Less than an hour a day", 0, time_computer_1080)),
        time_driving_1090 = as.numeric(ifelse(time_driving_1090 == "Less than an hour a day", 0, time_driving_1090)),
        lenth_mobile_phone_use_1110 = recode(lenth_mobile_phone_use_1110, "Never used mobile phone at least once per week" = 0, "One year or less" = 1, "Two to four years" = 2, "Five to eight years" = 3, "More than eight years" = 4),
        getting_up_in_morning_1170 = recode(getting_up_in_morning_1170, "Not at all easy" = 0, "Not very easy" = 1, "Fairly easy" = 2, "Very easy" = 3),
        sleep_chronotype_1180 = recode(sleep_chronotype_1180, "Definitely an 'evening' person" = 0, "More an 'evening' than a 'morning' person" = 1, "More a 'morning' than 'evening' person" = 2, "Definitely a 'morning' person" = 3),
        nap_during_day_1190 = recode(nap_during_day_1190, "Never/rarely" = 0, "Sometimes" = 1, "Usually" = 2),
        sleeplessness_1200 = recode(sleeplessness_1200, "Never/rarely" = 0, "Sometimes" = 1, "Usually" = 2),
        daytime_sleeping_narcolepsy_1220 = recode(daytime_sleeping_narcolepsy_1220, "Never/rarely" = 0, "Sometimes" = 1, "Often" = 2, "All of the time" = 3),
        cooked_vegetable_intake_1289 = as.numeric(ifelse(cooked_vegetable_intake_1289=="Less than one", 0, cooked_vegetable_intake_1289)),
        raw_vegetable_intake_1299 = as.numeric(ifelse(raw_vegetable_intake_1299=="Less than one", 0, raw_vegetable_intake_1299)),
        fresh_fruit_intake_1309 = as.numeric(ifelse(fresh_fruit_intake_1309=="Less than one", 0, fresh_fruit_intake_1309)),
        dried_fruit_intake_1319 = as.numeric(ifelse(dried_fruit_intake_1319=="Less than one", 0, dried_fruit_intake_1319)),
        oily_fish_intake_1329 = recode(oily_fish_intake_1329, "Never" = 0, "Less than once a week" = 1, "Once a week" = 2, "2-4 times a week" = 3, "5-6 times a week" = 4, "Once or more daily" = 5),
        non_oily_fish_intake_1339 = recode(non_oily_fish_intake_1339, "Never" = 0, "Less than once a week" = 1, "Once a week" = 2, "2-4 times a week" = 3, "5-6 times a week" = 4, "Once or more daily" = 5),
        processed_meat_intake_1349 = recode(processed_meat_intake_1349, "Never" = 0, "Less than once a week" = 1, "Once a week" = 2, "2-4 times a week" = 3, "5-6 times a week" = 4, "Once or more daily" = 5),
        poultry_intake_1359 = recode(poultry_intake_1359, "Never" = 0, "Less than once a week" = 1, "Once a week" = 2, "2-4 times a week" = 3, "5-6 times a week" = 4, "Once or more daily" = 5),
        beef_intake_1369 = recode(beef_intake_1369, "Never" = 0, "Less than once a week" = 1, "Once a week" = 2, "2-4 times a week" = 3, "5-6 times a week" = 4, "Once or more daily" = 5),
        lamb_mutton_intake_1379 = recode(lamb_mutton_intake_1379, "Never" = 0, "Less than once a week" = 1, "Once a week" = 2, "2-4 times a week" = 3, "5-6 times a week" = 4, "Once or more daily" = 5),
        pork_intake_1389 = recode(pork_intake_1389, "Never" = 0, "Less than once a week" = 1, "Once a week" = 2, "2-4 times a week" = 3, "5-6 times a week" = 4, "Once or more daily" = 5),
        cheese_intake_1408 = recode(cheese_intake_1408, "Never" = 0, "Less than once a week" = 1, "Once a week" = 2, "2-4 times a week" = 3, "5-6 times a week" = 4, "Once or more daily" = 5),
        bread_intake_1438 = as.numeric(ifelse(bread_intake_1438=="Less than one", 0, bread_intake_1438)),
        cereal_intake_1458 = as.numeric(ifelse(cereal_intake_1458=="Less than one", 0, cereal_intake_1458)),
        salt_added_1478 = recode(salt_added_1478, "Never/rarely" = 0, "Sometimes" = 1, "Usually" = 2, "Always" = 3),
        tea_intake_1488 = as.numeric(ifelse(tea_intake_1488=="Less than one", 0, tea_intake_1488)),
        coffee_intake_1498 = as.numeric(ifelse(coffee_intake_1498=="Less than one", 0, coffee_intake_1498)),
        water_intake_1528 = as.numeric(ifelse(water_intake_1528=="Less than one", 0, water_intake_1528)),
        variation_diet_1548 = recode(variation_diet_1548, "Never/rarely" = 0, "Often" = 1, "Sometimes" = 2),
        alcohol_intake_freq_1558 = recode(alcohol_intake_freq_1558, "Never" = 0, "Special occasions only" = 1, "One to three times a month" = 2, "Once or twice a week" = 3, "Three or four times a week" = 4, "Daily or almost daily" = 5),
        able_to_confide_2110 = recode(able_to_confide_2110, "Never or almost never" = 0, "Once every few months" = 1, "About once a month" = 2, "About once a week" = 3, "2-4 times a week" = 4, "Almost daily" = 5),
        age_first_intercourse_2139 = as.numeric(ifelse(age_first_intercourse_2139 == "Never had sex", 99, age_first_intercourse_2139)),
        play_computer_games_2237 = recode(play_computer_games_2237, "Never/rarely" = 0, "Sometimes" = 1, "Often" = 2),
        use_uv_protection_2267 = recode(use_uv_protection_2267, "Do not go out in sunshine" = 0, "Never/rarely" = 1, "Sometimes" = 2, "Most of the time" = 3, "Always" = 4),
        job_satisfaction_4537 = recode(job_satisfaction_4537, "Extremely unhappy" = 0, "Very unhappy" = 1, "Moderately unhappy" = 2, "Unemployed" = 3, "Moderately happy" = 4, "Very happy" = 5, "Extremely happy" = 6),
        health_satisfaction_4548 = recode(health_satisfaction_4548, "Extremely unhappy" = 0, "Very unhappy" = 1, "Moderately unhappy" = 2, "Moderately happy" = 3, "Very happy" = 4, "Extremely happy" = 5),
        family_relationship_satisfaction_4559 = recode(family_relationship_satisfaction_4559, "Extremely unhappy" = 0, "Very unhappy" = 1, "Moderately unhappy" = 2, "Moderately happy" = 3, "Very happy" = 4, "Extremely happy" = 5),
        friendship_satisfaction_4570 = recode(friendship_satisfaction_4570, "Extremely unhappy" = 0, "Very unhappy" = 1, "Moderately unhappy" = 2, "Moderately happy" = 3, "Very happy" = 4, "Extremely happy" = 5),
        finances_satisfaction_4581 = recode(finances_satisfaction_4581, "Extremely unhappy" = 0, "Very unhappy" = 1, "Moderately unhappy" = 2, "Moderately happy" = 3, "Very happy" = 4, "Extremely happy" = 5),
        access_private_healthcare_4674 = recode(access_private_healthcare_4674, "No, never" = 0, "Yes, sometimes" = 1, "Yes, most of the time" = 2, "Yes, all of the time" = 3),
        smoking_status_20116 = recode(smoking_status_20116, "Never" = 0, "Previous" = 1, "Current" = 2),
        alcohol_drinker_status_20117 = recode(alcohol_drinker_status_20117, "Never" = 0, "Previous" = 1, "Current" = 2)
        ) %>% glimpse

fwrite(df, "./results/lifestyle_clean_coded.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

# Dataframe of missing values per timepoint

df$instanceID <- as.factor(df$instanceID)
tp_levels <- levels(df$instanceID)

var_mapping = as.data.frame(fread("../../../gender_datadriven/Analyses/gender_score/variable_mapping.tsv"))

# Function to summarize missingness per variable and per instanceID
missing_summary = df %>%
    select(-c(Date_of_attending_assessment_centre_53_0, sex_31, genetic_sex_22001, age_21003)) %>%
    pivot_longer(cols = -c(ID, instanceID), names_to = "variable", values_to = "value") %>%
    group_by(variable, instanceID) %>%
    summarise(
        missing_count = sum(is.na(value)),
        total = n(),
        missing_pct = round(100 * missing_count / total, 1),
        .groups = "drop"
    ) %>%
    mutate(
        formatted = paste0(missing_count, " (", missing_pct, "%)")
    ) %>%
    select(variable, instanceID, formatted) %>%
    pivot_wider(names_from = instanceID, values_from = formatted) %>%
    ungroup() %>%
    rename(Variable = "variable") %>%
    left_join(var_mapping, by="Variable") %>%
    select(c("Category", "Variable", "Variable_clean", "FieldID", "0", "1", "2", "3")) %>%
    glimpse()

fwrite(missing_summary, "df_missing.csv")

# Combine the TP columns into a single column
missing_summary <- missing_summary %>%
  unite("missing_by_timepoint", starts_with("0"), starts_with("1"), starts_with("2"), starts_with("3"), sep = ", ", na.rm = TRUE)

fwrite(missing_summary, "df_missing_formatted.csv")

