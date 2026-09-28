
library(data.table)
library(tidyverse)
library(dplyr)
library(stringr)
library(hms)
library(viridis)
library(patchwork)
library(mgcv)
library(grid)
library(effects)
library(lme4)
library(lmerTest)
library(splines)

dir.create("./results/indiv", showWarnings = FALSE)
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

# Gender scores
df_gender_0 = as.data.frame(fread("../gender_score_new/results/all_tp0/gender_score_combined.tsv"))
df_gender_1 = as.data.frame(fread("../gender_score_new/results/all_tp1/gender_score_combined.tsv"))
df_gender_2 = as.data.frame(fread("../gender_score_new/results/all_tp2/gender_score_combined.tsv"))
df_gender_3 = as.data.frame(fread("../gender_score_new/results/all_tp3/gender_score_combined.tsv"))

# Covariates
df_cov = as.data.frame(fread("../../../UKB/tabular/df_body_cov/UKBB_body_cov_wide.tsv"))

# Baseline data
var_df = as.data.frame(fread("../gender_score_new/variable_mapping.tsv"))
df_raw = as.data.frame(fread("../impute_input/results/df_imputed_tp_all_combined.tsv"))

# Physical data
types_physical = c(
        "arterial_stiffness", "blood_pressure", "body_composition", "body_size",
        "bone_density_heel", "carotid", "ecg_exercise", "ecg_rest", "grip_strength", "hearing", "spirometry"
    )

df_physical = list()

for (i in 1:length(types_physical)) {
    phys_tmp = types_physical[i]
    df_physical[[phys_tmp]] = as.data.frame(fread(paste0("../../../UKB/Analyses/clean_physical/results/df_",phys_tmp,".tsv")))
}

#endregion

#region Clean data

# Baseline data

df_raw = df_raw %>%
    rename(InstanceID = "instanceID") %>%
    mutate(
        InstanceID = factor(InstanceID),
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
    glimpse()

# Covariates
df_cov = df_cov %>%
    rename(ID = "SubjectID", height = "Standing height_50", assessment_center = "UK Biobank assessment centre_54", weight = "Weight_21002") %>%
    select(ID, InstanceID, assessment_center, height, weight) %>%
    mutate(assessment_center = factor(assessment_center), InstanceID = factor(InstanceID)) %>%
    filter(complete.cases(.)) %>%
    glimpse()

# Gender scores
df_gender_0 = df_gender_0 %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    mutate(InstanceID = 0) %>%
    glimpse()

df_gender_1 = df_gender_1 %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    mutate(InstanceID = 1) %>%
    glimpse()

df_gender_2 = df_gender_2 %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    mutate(InstanceID = 2) %>%
    glimpse()

df_gender_3 = df_gender_3 %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    mutate(InstanceID = 3) %>%
    glimpse()

df_gender = rbind(df_gender_0, df_gender_1, df_gender_2, df_gender_3)
df_gender = df_gender %>% mutate(InstanceID = factor(InstanceID))

# Arterial stiffness

df_physical[['arterial_stiffness']] = df_physical[['arterial_stiffness']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        arterial_pulsewave_stiffness_device_id_4136 = factor(arterial_pulsewave_stiffness_device_id_4136),
        stiffness_method_4186 = factor(stiffness_method_4186),
        absence_of_notch_position_in_the_pulse_waveform_4204 = factor(absence_of_notch_position_in_the_pulse_waveform_4204),
        arterial_stiffness_device_id_4206 = factor(arterial_stiffness_device_id_4206)
    ) %>%
    # Remove participants: non-direct entry
    filter(stiffness_method_4186 == "Direct entry") %>%
    glimpse()

# Blood pressure

df_physical[['blood_pressure']] = df_physical[['blood_pressure']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        blood_pressure_device_id_36_0 = factor(blood_pressure_device_id_36_0),
        blood_pressure_device_id_36_1 = factor(blood_pressure_device_id_36_1),
        method_of_measuring_blood_pressure_4081_0 = factor(method_of_measuring_blood_pressure_4081_0),
        method_of_measuring_blood_pressure_4081_1 = factor(method_of_measuring_blood_pressure_4081_1),
        blood_pressure_manual_sphygmomanometer_device_id_37_0 = factor(blood_pressure_manual_sphygmomanometer_device_id_37_0),
        blood_pressure_manual_sphygmomanometer_device_id_37_1 = factor(blood_pressure_manual_sphygmomanometer_device_id_37_1)
    ) %>%
    mutate(
        pulse_pressure = systolic_blood_pressure_automated_reading_4080 - diastolic_blood_pressure_automated_reading_4079,
        mean_arterial_pressure = ((2*diastolic_blood_pressure_automated_reading_4079) + systolic_blood_pressure_automated_reading_4080) / 3
    ) %>%
    # Remove participants: non-direct entry
    filter(method_of_measuring_blood_pressure_4081_0 == "Direct entry" & method_of_measuring_blood_pressure_4081_1 == "Direct entry") %>%
    glimpse()

# ECG during exercise

df_physical[['ecg_exercise']] = df_physical[['ecg_exercise']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        doctor_restricts_physical_activity_due_to_heart_condition_6014 = factor(doctor_restricts_physical_activity_due_to_heart_condition_6014),
        chest_pain_felt_during_physical_activity_6015 = factor(chest_pain_felt_during_physical_activity_6015),
        chest_pain_felt_outside_physical_activity_6016 = factor(chest_pain_felt_outside_physical_activity_6016),
        able_to_walk_or_cycle_unaided_for_10_minutes_6017 = factor(able_to_walk_or_cycle_unaided_for_10_minutes_6017),
        ecg_bike_method_for_fitness_test_6019 = factor(ecg_bike_method_for_fitness_test_6019),
        completion_status_of_test_6020 = factor(completion_status_of_test_6020),
        description_of_exercise_protocol_recommended_6023 = factor(description_of_exercise_protocol_recommended_6023),
        program_category_6024 = factor(program_category_6024),
        target_heart_rate_achieved_6034 = factor(target_heart_rate_achieved_6034),
        reason_ecg_not_completed_20059 = factor(reason_ecg_not_completed_20059),
        reason_atrest_ecg_performed_without_bicycle_20060 = factor(reason_atrest_ecg_performed_without_bicycle_20060),
        reason_for_skipping_ecg_20058 = factor(reason_for_skipping_ecg_20058)
    ) %>%
    # Remove participants: ecg not on bike
    filter(ecg_bike_method_for_fitness_test_6019 == "Bicycle") %>%
    glimpse()

# Bone-densitometry of heel

df_physical[['bone_density_heel']] = df_physical[['bone_density_heel']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        heel_ultrasound_method_19 = factor(heel_ultrasound_method_19),
        heel_ultrasound_device_id_45 = factor(heel_ultrasound_device_id_45),
        foot_measured_for_bone_density_3081 = factor(foot_measured_for_bone_density_3081),
        fractured_heel_3082 = factor(fractured_heel_3082),
        heel_ultrasound_method_left_4092 = factor(heel_ultrasound_method_left_4092),
        heel_ultrasound_method_right_4095 = factor(heel_ultrasound_method_right_4095),
        fractured_heel_left_4093 = factor(fractured_heel_left_4093),
        fractured_heel_right_4096 = factor(fractured_heel_right_4096)
    ) %>%
    # Remove participants: not direct entry
    filter(heel_ultrasound_method_19 == "Direct entry") %>%
    glimpse()

# Hand grip strength

df_physical[['grip_strength']] = df_physical[['grip_strength']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        hand_grip_dynamometer_device_id_38 = factor(hand_grip_dynamometer_device_id_38),
        reason_for_skipping_grip_strength_right_20043 = factor(reason_for_skipping_grip_strength_right_20043),
        reason_for_skipping_grip_strength_left_20044 = factor(reason_for_skipping_grip_strength_left_20044)
    ) %>%
    # Calculate left/right maximum
    mutate(
        max_hand_grip_strength = ifelse(hand_grip_strength_right_47>=hand_grip_strength_left_46, hand_grip_strength_right_47, hand_grip_strength_left_46)
    ) %>%
    glimpse()

# Spirometry

df_physical[['spirometry']] = df_physical[['spirometry']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        spirometry_method_23 = factor(spirometry_method_23),
        spirometer_device_id_42 = factor(spirometer_device_id_42),
        contraindications_for_spirometry_3088 = factor(contraindications_for_spirometry_3088),
        caffeine_drink_within_last_hour_3089 = factor(caffeine_drink_within_last_hour_3089),
        used_an_inhaler_for_chest_within_last_hour_3090 = factor(used_an_inhaler_for_chest_within_last_hour_3090),
        spirometry_device_serial_number_3132 = factor(spirometry_device_serial_number_3132),
        smoked_cigarette_or_pipe_within_last_hour_3159 = factor(smoked_cigarette_or_pipe_within_last_hour_3159),
        reproduciblity_of_spirometry_measurement_using_ers_ats_criteria_20152 = factor(reproduciblity_of_spirometry_measurement_using_ers_ats_criteria_20152),
        reason_for_skipping_spirometry_20042 = factor(reason_for_skipping_spirometry_20042),
        spirometry_method_pilot_10711 = factor(spirometry_method_pilot_10711),
        spirometry_device_serial_number_pilot_10714 = factor(spirometry_device_serial_number_pilot_10714)
    ) %>%
    mutate(
        fev1_fvc_ratio = forced_expiratory_volume_in_1second_fev1_best_measure_20150 / forced_vital_capacity_fvc_best_measure_20151
    ) %>%
    # Remove participants: not direct entry, not reproducible according to ERS/ATS Criteria
    filter(spirometry_method_23 == "Direct entry" & reproduciblity_of_spirometry_measurement_using_ers_ats_criteria_20152 == "Yes") %>%
    glimpse()

# Hearing test

df_physical[['hearing']] = df_physical[['hearing']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        completion_status_left_4268 = factor(completion_status_left_4268),
        volume_level_set_by_participant_left_4270 = as.numeric(factor(volume_level_set_by_participant_left_4270, levels=c("10%", "20%", "40%", "70%", "100% (max)"), labels=c(1,2,3,4,5))),
        completion_status_right_4275 = factor(completion_status_right_4275),
        volume_level_set_by_participant_right_4277 = as.numeric(factor(volume_level_set_by_participant_right_4277, levels=c("10%", "20%", "40%", "70%", "100% (max)"), labels=c(1,2,3,4,5))),
        hearing_test_done_4849 = factor(hearing_test_done_4849)
    ) %>%
    mutate(
        mean_srt = (speechreceptionthreshold_srt_estimate_left_20019 + speechreceptionthreshold_srt_estimate_right_20021) / 2
    ) %>%
    # Remove participants: not completed, 
    filter(completion_status_left_4268 == "completed" & completion_status_right_4275 == "completed" & hearing_test_done_4849 == "Yes") %>%
    glimpse()

# Carotid ultrasound

df_physical[['carotid']] = df_physical[['carotid']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        carotid_ultrasound_measuring_method_12291 = factor(carotid_ultrasound_measuring_method_12291),
        carotid_ultrasound_measurement_completed_12292 = factor(carotid_ultrasound_measurement_completed_12292),
        quality_control_indicator_for_imt_at_120_degrees_22682 = factor(quality_control_indicator_for_imt_at_120_degrees_22682),
        quality_control_indicator_for_imt_at_150_degrees_22683 = factor(quality_control_indicator_for_imt_at_150_degrees_22683),
        quality_control_indicator_for_imt_at_210_degrees_22684 = factor(quality_control_indicator_for_imt_at_210_degrees_22684),
        quality_control_indicator_for_imt_at_240_degrees_22685 = factor(quality_control_indicator_for_imt_at_240_degrees_22685)
    ) %>%
    mutate(
        mean_cIMT = rowMeans(select(., 
                mean_carotid_imt_intimamedial_thickness_at_120_degrees__22671,
                mean_carotid_imt_intimamedial_thickness_at_150_degrees__22674,
                mean_carotid_imt_intimamedial_thickness_at_210_degrees__22677,
                mean_carotid_imt_intimamedial_thickness_at_240_degrees__22680), 
                na.rm = TRUE),
        max_cIMT = pmax(mean_carotid_imt_intimamedial_thickness_at_120_degrees__22671,
                mean_carotid_imt_intimamedial_thickness_at_150_degrees__22674,
                mean_carotid_imt_intimamedial_thickness_at_210_degrees__22677,
                mean_carotid_imt_intimamedial_thickness_at_240_degrees__22680,
                na.rm = TRUE),
        sd_cIMT = apply(select(., 
                mean_carotid_imt_intimamedial_thickness_at_120_degrees__22671,
                mean_carotid_imt_intimamedial_thickness_at_150_degrees__22674,
                mean_carotid_imt_intimamedial_thickness_at_210_degrees__22677,
                mean_carotid_imt_intimamedial_thickness_at_240_degrees__22680), 
                1, sd, na.rm = TRUE)
    ) %>%
    # Remove participants: not direct entry, not completed
    filter(carotid_ultrasound_measuring_method_12291 == "Direct entry" & carotid_ultrasound_measurement_completed_12292 == "Yes") %>%
    glimpse()

# ECG at rest

df_physical[['ecg_rest']] = df_physical[['ecg_rest']] %>%
    mutate(
        InstanceID = factor(InstanceID),
        ecg_measuring_method_12lead_12323 = factor(ecg_measuring_method_12lead_12323),
        ecg_automated_diagnoses_12653 = factor(ecg_automated_diagnoses_12653),
        ecg_automated_diagnoses_12653_1 = factor(ecg_automated_diagnoses_12653_1),
        ecg_automated_diagnoses_12653_2 = factor(ecg_automated_diagnoses_12653_2),
        ecg_automated_diagnoses_12653_3 = factor(ecg_automated_diagnoses_12653_3),
        ecg_automated_diagnoses_12653_4 = factor(ecg_automated_diagnoses_12653_4),
        ecg_automated_diagnoses_12653_5 = factor(ecg_automated_diagnoses_12653_5),
        ecg_automated_diagnoses_12653_6 = factor(ecg_automated_diagnoses_12653_6),
        ecg_automated_diagnoses_12653_7 = factor(ecg_automated_diagnoses_12653_7),
        ecg_automated_diagnoses_12653_8 = factor(ecg_automated_diagnoses_12653_8),
        ecg_automated_diagnoses_12653_9 = factor(ecg_automated_diagnoses_12653_9),
        ecg_automated_diagnoses_12653_10 = factor(ecg_automated_diagnoses_12653_10),
        ecg_automated_diagnoses_12653_11 = factor(ecg_automated_diagnoses_12653_11),
        ecg_automated_diagnoses_12653_12 = factor(ecg_automated_diagnoses_12653_12),
        ecg_automated_diagnoses_12653_13 = factor(ecg_automated_diagnoses_12653_13),
        ecg_automated_diagnoses_12653_14 = factor(ecg_automated_diagnoses_12653_14),
        identifier_for_12lead_ecg_device_12658 = factor(identifier_for_12lead_ecg_device_12658),
        suspicious_flag_for_12lead_ecg_12657 = factor(suspicious_flag_for_12lead_ecg_12657)
    ) %>%
    # Remove participants: not direct entry
    filter(ecg_measuring_method_12lead_12323 == "Direct entry") %>%
    glimpse()

#endregion

#region Associations with gender

# df = df_physical[['arterial_stiffness']] %>% filter(InstanceID == 0)
# name_df = "arterial_stiffness"
# vars_of_interest = c("pulse_rate_4194", "pulse_wave_reflection_index_4195", "pulse_wave_arterial_stiffness_index_21021")
# covariate_list = c("height", "arterial_stiffness_device_id_4206")
# cov_effect_type = c("fixed", "random")

assoc_phys_gender = function(df, name_df, vars_of_interest, covariate_list, cov_effect_type) {

    df_tmp <<- df_gender %>%
        inner_join(df %>% select(SubjectID, InstanceID, all_of(vars_of_interest), any_of(covariate_list)) %>% rename(ID = "SubjectID"), by=c("ID", "InstanceID")) %>%
        left_join(df_cov, by=c("ID", "InstanceID"))

    covariate_formula = ""
    if (length(covariate_list) > 0) { for (c in 1:length(covariate_list)) {
        if (cov_effect_type[c] == "fixed") { covariate_formula = paste0(covariate_formula, " + ", covariate_list[c]) }
        if (cov_effect_type[c] == "random") { covariate_formula = paste0(covariate_formula, " + (1|", covariate_list[c],")") }
    } }

    results_tmp = list()
    i = 1
    # For each variable of interest
    for (v in 1:length(vars_of_interest)) {
    
        print(paste0(vars_of_interest[v]))
        
        # Linear models
        lm_males = summary(lmer(paste0("scale(",vars_of_interest[v],") ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + (1|assessment_center)", covariate_formula), data = df_tmp %>% filter(Sex == "Male")))
        lm_females = summary(lmer(paste0("scale(",vars_of_interest[v],") ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + (1|assessment_center)", covariate_formula), data = df_tmp %>% filter(Sex == "Female")))
        lm_inter = summary(lmer(paste0("scale(",vars_of_interest[v],") ~ Sex + Gender_score + Gender_score*Sex + bs(Age, degree = 3, df = 4) + (1|assessment_center)", covariate_formula), data = df_tmp))

        # Save results
        results_tmp[[i]] = data.frame(
            phys_categ = name_df,
            instance = unique(df_tmp$InstanceID),
            var = vars_of_interest[v],
            n_females = length(lm_females$residuals),
            n_males = length(lm_males$residuals),
            n_all = length(lm_inter$residuals),
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

        i = i + 1
    }

    results_tmp = do.call(rbind, results_tmp)

    return(results_tmp)
}

# Run

results = list()

# Arterial stiffness
interest_vars = c(
        "pulse_rate_4194",
        "pulse_wave_reflection_index_4195",
        "pulse_wave_arterial_stiffness_index_21021"
    )

results[['arterial_stiffness']] = assoc_phys_gender(
    df_physical[['arterial_stiffness']]  %>% filter(InstanceID == 0),
    "arterial_stiffness",
    interest_vars, 
    c("height", "arterial_stiffness_device_id_4206"),
    c("fixed", "random")
)

# Blood pressure
interest_vars = c(
        "diastolic_blood_pressure_automated_reading_4079",
        "systolic_blood_pressure_automated_reading_4080",
        "pulse_pressure",
        "mean_arterial_pressure"
    )

results[['blood_pressure']] = assoc_phys_gender(
    df_physical[['blood_pressure']] %>% filter(InstanceID == 0),
    "blood_pressure",
    interest_vars,
    c("height", "pulse_rate_automated_reading_102", "blood_pressure_device_id_36_0"), # White does pulse rate have all NAs?
    c("fixed", "fixed", "random")
)

# ECG during exercise
interest_vars = c(
        "maximum_workload_during_fitness_test_6032",
        "maximum_heart_rate_during_fitness_test_6033",
        "duration_of_fitness_test_6039"
    )

results[['ecg_exercise']] = assoc_phys_gender(
    df_physical[['ecg_exercise']] %>% filter(InstanceID == 0),
    "ecg_exercise",
    interest_vars,
    c("height"),
    c("fixed")
)

# Bone density of heel
interest_vars = c(
        "heel_bone_mineral_density_bmd_tscore_automated_78",
        "heel_quantitative_ultrasound_index_qui_direct_entry_3147",
        "heel_bone_mineral_density_bmd_3148",
        "heel_broadband_ultrasound_attenuation_direct_entry_3144",
        "speed_of_sound_through_heel_3146"
    )

results[['bone_density_heel']] = assoc_phys_gender(
    df_physical[['bone_density_heel']] %>% filter(InstanceID == 0),
    "bone_density_heel",
    interest_vars,
    c("height", "weight", "heel_ultrasound_device_id_45"),
    c("fixed", "fixed", "random")
)

# Grip strength
interest_vars = c(
        "max_hand_grip_strength"
    )

results[['grip_strength']] = assoc_phys_gender(
    df_physical[['grip_strength']] %>% filter(InstanceID == 0),
    "grip_strength",
    interest_vars,
    c("height", "hand_grip_dynamometer_device_id_38"),
    c("fixed", "random")
)

# Spirometry
interest_vars = c(
        "forced_expiratory_volume_in_1second_fev1_best_measure_20150",
        "forced_vital_capacity_fvc_best_measure_20151",
        "fev1_fvc_ratio"
    )

results[['spirometry']] = assoc_phys_gender(
    df_physical[['spirometry']] %>% filter(InstanceID == 0),
    "spirometry",
    interest_vars,
    c("height", "smoked_cigarette_or_pipe_within_last_hour_3159", "caffeine_drink_within_last_hour_3089", "used_an_inhaler_for_chest_within_last_hour_3090", "spirometer_device_id_42"),
    c("fixed", "fixed", "fixed", "fixed", "random")
)

# Hearing test
interest_vars = c(
        "speechreceptionthreshold_srt_estimate_left_20019",
        "speechreceptionthreshold_srt_estimate_right_20021",
        "mean_srt"
    )

results[['hearing']] = assoc_phys_gender(
    df_physical[['hearing']] %>% filter(InstanceID == 0),
    "hearing",
    interest_vars,
    c("duration_of_hearing_test_left_4272", "duration_of_hearing_test_right_4279", "volume_level_set_by_participant_left_4270", "volume_level_set_by_participant_right_4277"),
    c("fixed", "fixed", "fixed", "fixed")
)

# Carotid ultrasound
interest_vars = c(
        "mean_carotid_imt_intimamedial_thickness_at_120_degrees__22671",
        "mean_carotid_imt_intimamedial_thickness_at_150_degrees__22674",
        "mean_carotid_imt_intimamedial_thickness_at_210_degrees__22677",
        "mean_carotid_imt_intimamedial_thickness_at_240_degrees__22680",
        "mean_cIMT",
        "max_cIMT",
        "sd_cIMT"
    )

results[['carotid']] = assoc_phys_gender(
    df_physical[['carotid']] %>% filter(InstanceID == 2),
    "carotid",
    interest_vars,
    c("height", "weight"),
    c("fixed", "fixed")
)

# ECG at rest (12-lead)
interest_vars = c(
        "ventricular_rate_12336",
        "p_duration_12338",
        "qrs_duration_12340",
        "pq_interval_22330",
        "qt_interval_22331",
        "qtc_interval_22332",
        "rr_interval_22333",
        "pp_interval_22334",
        "p_axis_22335",
        "r_axis_22336",
        "t_axis_22337",
        "qrs_num_22338"
    )

results[['ecg_rest']] = assoc_phys_gender(
    df_physical[['ecg_rest']] %>% filter(InstanceID == 2),
    "ecg_rest",
    interest_vars,
    c("height", "weight", "identifier_for_12lead_ecg_device_12658"),
    c("fixed", "fixed", "random")
)

# Bind and write results

results = do.call(rbind, results)
fwrite(results, "./results/results.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

#endregion


