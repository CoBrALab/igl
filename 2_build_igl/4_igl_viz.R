
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
library(matrixStats)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

args = commandArgs(trailingOnly = TRUE)
run = args[1]

print(run)

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

# Same-sex intercourse
df_orientation = as.data.frame(fread("../../../UKB/tabular/df_samesex_intercourse/UKBB_samesex_intercourse_wide.tsv"))
df_orientation = df_orientation %>%
    filter(InstanceID == 0) %>%
    rename(ID = "SubjectID", same_sex_intercourse = "Ever had same-sex intercourse_2159", number_same_sex_partners = "Lifetime number of same-sex sexual partners_3669") %>%
    select(ID, same_sex_intercourse, number_same_sex_partners) %>%
    inner_join(df_raw %>% filter(instanceID == 0) %>% select(ID, instanceID, sex_31, genetic_sex_22001, sex_aneuploidy), by="ID") %>%
    glimpse()

# Calculate sex
df_tmp = df_raw %>%
    filter(instanceID == 0) %>%
    glimpse()

table(df_tmp$sex_aneuploidy)

df_tmp %>%
    filter(sex_31 != genetic_sex_22001) %>%
    glimpse()

#endregion

#region Prep functions for plotting

# Function to plot confusion matrix

# name_ana = "all_tp0"
# out = "test.png"

plot_conf_matrix = function(name_ana, out) {

    df = as.data.frame(fread(paste0("./results/",name_ana,"/gender_score_combined.tsv")))

    df = df %>%
        mutate(
            Observed = factor(Genetic_sex, levels=c("Female", "Male")),
            Predictions = factor(ifelse(Probability_male_mean>0.5, "Male", "Female"), levels=c("Female", "Male"))
        ) %>%
        filter(train == "train")

    # Get confusion matrix and performance metrics
    conf_matrix_obj = confusionMatrix(df$Observed, df$Predictions, positive="Male")

    perf_metrics = data.frame(
            name = name_ana,
            AUC = as.numeric(roc(df$Observed, df$Probability_male_mean)$auc),
            TN = conf_matrix_obj$table[1,1],
            TP = conf_matrix_obj$table[2,2],
            FP = conf_matrix_obj$table[2,1],
            FN = conf_matrix_obj$table[1,2],
            as.data.frame(t(conf_matrix_obj$overall)),
            as.data.frame(t(conf_matrix_obj$byClass))
        )

    conf_matrix = data.frame(
            Reference = c("Male", "Female", "Male", "Female"),
            Prediction = c("Male", "Female", "Female", "Male"),
            Value = c(as.numeric(perf_metrics$TP), as.numeric(perf_metrics$TN), as.numeric(perf_metrics$FN), as.numeric(perf_metrics$FP))
        )
    
    conf_matrix = conf_matrix %>% mutate(
            Reference = factor(Reference, levels=c("Female", "Male")),
            Prediction = factor(Prediction, levels=c("Male", "Female")),
            Percentage = paste0(round((Value / sum(Value) * 100), 2), "%"),
        )

    # Plot confusion matrix
    plt = ggplot(data = conf_matrix, aes(x = Reference, y = Prediction, fill = Value)) +
        geom_tile() +
        geom_text(aes(label = Percentage), color = "white", size = 8) +
        scale_fill_gradientn(colors = gender_neutral_cont(100)) +
        labs(x = "Observed", y = "Predicted") +
        ggtitle("Confusion matrix") +
        theme_light() + 
        theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
            strip.background = element_rect(fill="#d4d4d2"), strip.text = element_text(color="black"),
            legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13))

    ggsave(out, width=5, height=5)
    print(out)

    ret = list()
    ret[['plt']] = plt
    ret[['df']] = perf_metrics

    return(ret)
}

# Function to plot ROC curves

# name_ana="all_tp0"
# out="test.png"

plot_roc = function(name_ana, out) {
    
    df = as.data.frame(fread(paste0("./results/",name_ana,"/gender_score_combined.tsv")))

    df = df %>%
        mutate(
            Observed = factor(Genetic_sex, levels=c("Female", "Male")),
            Predictions = factor(ifelse(Probability_male_mean>0.5, "Male", "Female"), levels=c("Female", "Male"))
        ) %>%
        filter(train == "train")
    
    rocobj = roc(df, Observed, Probability_male_mean, direction="<")
    auc_tmp = round(as.numeric(rocobj$auc),3)

    plt = ggroc(rocobj, size=2, color=gender_neutral_single) +
        geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color = "black") + 
        annotate("text",  x = 0.3, y = 0.1, # Adjust these values for precise placement
            label = paste0("AUC = ", auc_tmp), 
            size = 8, 
            color = "black") +
        ggtitle("ROC curve") +
        scale_x_reverse(name="1 - Specificity", labels = c(1,0.5,0), breaks=c(0,0.5,1)) +
        scale_y_continuous(name="Sensitivity", labels = c(0,0.5,1), breaks=c(0,0.5,1)) +
        theme_light() + 
        theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
            legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size=13))

    ggsave(out, width=5, height=5)
    print(out)

    return(plt)

}

# Function to plot precision-recall curves

# name_ana="all_tp3"
# out="test.png"

plot_precision_recall = function(name_ana, out) {

    df = as.data.frame(fread(paste0("./results/",name_ana,"/gender_score_combined.tsv")))

    df = df %>%
        mutate(
            Observed = factor(Genetic_sex, levels=c("Female", "Male")),
            Predictions = factor(ifelse(Probability_male_mean>0.5, "Male", "Female"), levels=c("Female", "Male"))
        )
    
    probj = pr.curve(scores.class0 = df$Probability_male_mean, weights.class0 = ifelse(df$Observed == "Female", FALSE, TRUE), curve = TRUE)
    pr_df = data.frame(Recall = probj$curve[, 1], Precision = probj$curve[, 2])

    plt = ggplot(pr_df, aes(x = Recall, y = Precision)) +
        geom_segment(aes(x = 0, xend = 1, y = 1, yend = 0), color = "black") + 
        geom_line(size = 2, color=gender_neutral_single) +
        ggtitle("Precision-recall curve") +
        scale_x_continuous(name="Recall") +
        scale_y_continuous(name="Precision") +
        theme_light() + 
        theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
            legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size=13))

    ggsave(out, width=7, height=7)
    print(out)

    return(plt)
}

# Plot distributions of gender score (residuals) across males and females

# name_ana="all_tp3"
# out="test"

plot_dist_gender = function(name_ana, out) {

    df = as.data.frame(fread(paste0("./results/",name_ana,"/gender_score_combined.tsv")))

    df = df %>%
        mutate(
            Observed = factor(Genetic_sex, levels=c("Female", "Male")),
            Predictions = factor(ifelse(Probability_male_mean>0.5, "Male", "Female"), levels=c("Female", "Male"))
        )

    g = rasterGrob(t(scale_gender_cont[seq(20,80)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

    plt = ggplot(df, aes(x=Probability_male_mean, fill=Observed, color=Observed)) + 
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
        geom_density(alpha=0.7) + 
        ggtitle("IGL Distributions") +
        scale_x_continuous(name="IGL", breaks=c(0, 0.5, 1)) +
        scale_y_continuous(name="Density") +
        scale_fill_manual(values=scale_female_male_dis) +
        scale_color_manual(values=scale_female_male_dis) +
        theme_light() + 
        theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
            legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13))

    ggsave(paste0(out, ".png"), width=8, height=5)
    print(paste0(out, ".png"))

    ggplot(df %>% filter(train == "left_out"), aes(x=Probability_male_mean, fill=Observed, color=Observed)) + 
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
        geom_density(alpha=0.7) + 
        ggtitle("IGL Distributions (held out set)") +
        scale_x_continuous(name="IGL", breaks=c(0, 0.5, 1)) +
        scale_y_continuous(name="Density") +
        scale_fill_manual(values=scale_female_male_dis) +
        scale_color_manual(values=scale_female_male_dis) +
        theme_light() + 
        theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
            legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13))

    ggsave(paste0(out, "_heldout.png"), width=8, height=5)
    print(paste0(out, "_heldout.png"))

    return(plt)
}

# Plot SHAP values (scatterplot)

# name_ana = "all_tp0"
# tp = 0
# out=paste0("./visualization/test/shap_")
# plot_shap = TRUE

plot_shap_var = function(name_ana, tp, out, plot_shap = TRUE) {

    # Load data
    df_shap = as.data.frame(fread(paste0("./results/",name_ana,"/shap_values_combined.tsv")))
    df_shap_avg = as.data.frame(fread(paste0("./results/",name_ana,"/shap_values_avg.tsv")))
    df_imp_avg = as.data.frame(fread(paste0("../impute_input/results/df_imputed_tp_",tp,"_combined.tsv")))

    df_imp = list()
    for (i in 1:5) {
        df_imp[[i]] = as.data.frame(fread(paste0("../impute_input/results/df_imputed_tp_",tp,"_m",i,".tsv")))
        df_imp[[i]]$impdf = i
    }
    df_imp = do.call(rbind, df_imp)

    # Plot
    g = rasterGrob(rev(scale_gender_cont[seq(20,80)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

    directions = list()
    for (v in 6:ncol(df_shap)) {

        # var_tmp = var_df$Variable[v]
        var_tmp = sub("^shap_", "", colnames(df_shap)[v])
        var_tmp_clean = var_df[which(var_df$Variable == var_tmp), 2]

        df_tmp = df_shap %>%
            select(ID, Fold, impdf, seed, all_of(paste0("shap_",var_tmp))) %>%
            left_join(df_imp %>% select(ID, impdf, genetic_sex_22001, all_of(var_tmp)), by=c("ID", "impdf")) %>%
            left_join(df_shap_avg %>% select(ID, all_of(paste0("shap_",var_tmp))) %>% rename(shap_avg = all_of(paste0("shap_",var_tmp))), by="ID") %>%
            left_join(df_imp_avg %>% select(ID, all_of(var_tmp)) %>% rename(raw_var_avg = all_of(var_tmp)), by="ID") %>%
            rename(raw_var = all_of(var_tmp), shap_var = paste0("shap_",var_tmp)) %>%
            mutate(raw_var=as.numeric(raw_var)) %>%
            mutate(
                run = factor(paste0("i",impdf,"_s",seed,"_k",Fold)),
                impdf = factor(impdf, levels=seq(1,5))
            )

        df_tmp_avg = df_shap_avg %>%
            select(ID, all_of(paste0("shap_",var_tmp))) %>%
            left_join(df_imp_avg %>% select(ID, all_of(var_tmp)) %>% rename(raw_var_avg = all_of(var_tmp)), by="ID") %>%
            rename(shap_var = paste0("shap_",var_tmp)) %>%
            mutate(raw_var_avg=as.numeric(raw_var_avg)) %>%
            filter(raw_var_avg >= 0)

        if (var_tmp == "age_first_intercourse_2139") {
            df_tmp_avg = df_tmp_avg %>% filter(raw_var_avg<99)
        }

        # Get directions
        directions[[var_tmp]] = list()
        for (r in 1:length(unique(df_tmp$run))) {
            run_i = unique(df_tmp$run)[r]

            pearson_model = as.numeric(cor.test(x=df_tmp %>% filter(run == run_i) %>% pull(raw_var), y=df_tmp %>% filter(run == run_i) %>% pull(shap_var), method="pearson")$estimate)
            spearman_model = as.numeric(cor.test(x=df_tmp %>% filter(run == run_i) %>% pull(raw_var), y=df_tmp %>% filter(run == run_i) %>% pull(shap_var), method="spearman")$estimate)
            directions[[var_tmp]][[run_i]] = data.frame(
                Variable = var_tmp,
                direction_pearson = ifelse(is.na(pearson_model) == TRUE, "none", ifelse(pearson_model > 0, "Male", "Female")),
                direction_spearman = ifelse(is.na(spearman_model) == TRUE, "none", ifelse(spearman_model > 0, "Male", "Female")),
                run=run_i
            )
        }
        directions[[var_tmp]] = do.call(rbind, directions[[var_tmp]])

        if (plot_shap == TRUE) {
            # Non-linear fit by fold

            iqr <- IQR(df_tmp_avg$raw_var_avg, na.rm = TRUE)
            med <- median(df_tmp_avg$raw_var_avg, na.rm = TRUE)
            q <- quantile(df_tmp_avg$raw_var_avg, probs = c(0.999), na.rm = TRUE)

            df_tmp_avg = df_tmp_avg %>%
                # Remove outliers (|MAD|>3)
                filter(
                    n_distinct(raw_var_avg) < 15 |
                    raw_var_avg <= q
                )

            lim_plot = max(max(abs(df_tmp_avg$shap_var)), max(abs(df_tmp_avg$shap_var)))
            gam_k = ifelse(length(unique(df_tmp_avg$raw_var_avg)) < 15, length(unique(df_tmp_avg$raw_var_avg)), 15)

            plt_nl = ggplot(data = df_tmp_avg) +
                annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
                geom_hline(yintercept=0, size=0.5, color="black") +
                geom_point(aes(x=raw_var_avg, y=shap_var), size=1, alpha=0.1, color=gender_neutral_single) + 
                xlab(var_tmp_clean) +
                scale_y_continuous(name=paste0("SHAP (probability)"), limits=c(-lim_plot,lim_plot), labels = function(x) sprintf("%.2f", x)) +
                scale_x_continuous(limits=c(min(df_tmp_avg$raw_var_avg), max(df_tmp_avg$raw_var_avg))) +
                scale_color_viridis_d(option="D") +
                ggtitle(var_tmp_clean) +
                theme_light() + 
                theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size=15, face="bold"),
                    strip.background = element_rect(fill="#d4d4d2"), strip.text = element_text(color="black"),
                    legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13))
            
            if (gam_k <= 2) {
                plt_nl = plt_nl + 
                    geom_line(aes(x=raw_var_avg, y=shap_var), stat="smooth", method="lm", alpha=1, color="black", size=1)                    
            } else {
                plt_nl = plt_nl + 
                    geom_line(aes(x=raw_var_avg, y=shap_var), stat="smooth", method="gam", formula=y ~ s(x, bs = "cs", k = gam_k), alpha=1, color="black", size=1)
            }

            ggsave(paste0(out, var_tmp, "_nl.png"), width=5, height=3)
            print(paste0(out, var_tmp, "_nl.png"))
        }
    }
    directions = do.call(rbind, directions)
    rownames(directions) = NULL

    fwrite(directions, paste0("./results/",name_ana,"/shap_directions_lm.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

    return(directions)
}

# Plot variable importance (horizontal bar graph)

# name_ana = "all_tp0"
# preds = "all"
# out="test"

plot_varimp_bar = function(name_ana, preds, out) {

    # Load SHAP values and directions
    df_shap = as.data.frame(fread(paste0("./results/",name_ana,"/shap_values_combined.tsv")))
    df_directions = as.data.frame(fread(paste0("./results/",name_ana,"/shap_directions_lm.tsv")))

    # Clean dataset
    df_tmp = df_shap %>%
        mutate(run = factor(paste0("i",impdf,"_s",seed,"_k",Fold))) %>%
        group_by(run) %>%
        summarise(across(starts_with("shap_"),
                        ~ mean(abs(.x), na.rm = TRUE),
                        .names = "{.col}")) %>%
        ungroup() %>%
        rename_with(~ str_remove(.x, "^shap_")) %>%
        pivot_longer(cols=-run, names_to = "Variable", values_to = "mean_abs_shap") %>%
        left_join(df_directions, by=c("Variable", "run")) %>%
        left_join(var_df, by="Variable") %>%
        arrange(Variable, run) %>%
        mutate(
            direction_pearson = factor(direction_pearson),
            direction_spearman = factor(direction_spearman),
            Variable_clean = factor(Variable_clean),
            Category = factor(Category),
        ) %>% 
        # Assign 0 if negative importance
        mutate(mean_abs_shap = ifelse(mean_abs_shap < 0, 0, mean_abs_shap)) %>%
        # *-1 if female direction
        mutate(
            mean_abs_shap_pearson = ifelse(direction_pearson == "Female", mean_abs_shap*-1, mean_abs_shap),
            mean_abs_shap_spearman = ifelse(direction_spearman == "Female", mean_abs_shap*-1, mean_abs_shap),
        ) %>%
        separate(run, into = c("i", "s", "k"), sep = "_", remove = FALSE) %>%
        rename(impdf = "i", seed = "s", fold = "k") %>%
        mutate(impdf = factor(impdf), seed = factor(seed), fold = factor(fold))
    
    fwrite(df_tmp, paste0("./results/",name_ana,"/shap_mean_abs.tsv"), row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

    # Calculate average across runs
    df_tmp_avg = df_tmp %>%
        group_by(Variable) %>%
        summarise(
            avg_mean_abs_shap_pearson = mean(mean_abs_shap_pearson, na.rm = TRUE),
            sd_mean_abs_shap_pearson = sd(mean_abs_shap_pearson, na.rm = TRUE),
            avg_mean_abs_shap_spearman = mean(mean_abs_shap_spearman, na.rm = TRUE),
            sd_mean_abs_shap_spearman = sd(mean_abs_shap_spearman, na.rm = TRUE),
            avg_mean_abs_shap_nodir = mean(mean_abs_shap, na.rm = TRUE),
            sd_mean_abs_shap_nodir = sd(mean_abs_shap, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        left_join(var_df, by="Variable") %>%
        mutate(
            Variable_clean = factor(Variable_clean),
            Category = factor(Category),
        ) %>%
        mutate(
            Variable_clean_pearson = fct_reorder(Variable_clean, avg_mean_abs_shap_pearson),
            Variable_clean_spearman = fct_reorder(Variable_clean, avg_mean_abs_shap_spearman),
            Variable_clean_nodir = fct_reorder(Variable_clean, avg_mean_abs_shap_nodir),
        ) %>%
        mutate(
            direction_pearson = factor(ifelse(avg_mean_abs_shap_pearson < 0, "Female", "Male"), levels = c("Female", "Male")),
            direction_spearman = factor(ifelse(avg_mean_abs_shap_spearman < 0, "Female", "Male"), levels = c("Female", "Male"))
        )

    fwrite(df_tmp_avg, paste0("./results/",name_ana,"/shap_mean_abs_avg.tsv"), row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

    # Plots
    plt_width = ifelse(preds=="all", 25, 7)

    # By category
    plt = ggplot(df_tmp_avg) + 
        geom_col(aes(x=Variable_clean_nodir, y=avg_mean_abs_shap_nodir, fill=direction_pearson)) + 
        geom_errorbar(aes(x=Variable_clean_nodir, y=avg_mean_abs_shap_nodir, ymin = avg_mean_abs_shap_nodir - sd_mean_abs_shap_nodir, ymax = avg_mean_abs_shap_nodir + sd_mean_abs_shap_nodir), width = 0.6, linewidth=1, color = "black") +
        geom_jitter(data = df_tmp, aes(x=Variable_clean, y=mean_abs_shap, group=run), size = 0.5, width = 0.2, height = 0, alpha=0.2, color="black", shape=16) +
        geom_hline(yintercept=0) +
        scale_x_discrete(name="") +
        scale_y_continuous(name="Mean |SHAP|", expand=c(0,0), breaks=seq(0, 1, by=0.05)) +
        scale_fill_manual(values=scale_gender_dis) +
        coord_flip() +
        facet_wrap(~Category, nrow=1, scales="free_y") +
        theme_light() + 
        theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5, size=20, face="bold"),
            legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
            strip.text = element_text(size = 25, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"))
    ggsave(paste0(out, "_mean_abs_shap_categ_nodir.png"), width=plt_width, height=8)
    print(paste0(out, "_mean_abs_shap_categ_nodir.png"))

}

# name_ana = "all_tp0_diet"
# preds = "diet"
# out=paste0("./test_shap_indiv/")

plot_shap_indiv = function(name_ana, preds, out) {

    top_n_features = 20
    set.seed(123)

    # Load data
    df_shap = as.data.frame(fread(paste0("./results/",name_ana,"/shap_values_combined.tsv")))
    df_pred = as.data.frame(fread(paste0("./results/",name_ana,"/gender_score_combined.tsv")))

    df_pred = df_pred %>%
        mutate(pred_error = abs(Probability_male_mean - ifelse(Genetic_sex == "Male", 1, 0)))

    # At random select 5 correctly/incorrectly predicted males/females, and 5 androgynous males/females
    ids_to_plot <- bind_rows(
        df_pred %>% filter(Genetic_sex == "Male" & pred_error < 0.1) %>% slice_sample(n = 5),
        df_pred %>% filter(Genetic_sex == "Male" & pred_error > 0.9) %>% slice_sample(n = 5),
        df_pred %>% filter(Genetic_sex == "Male" & Probability_male_mean < 0.6 & Probability_male_mean > 0.4) %>% slice_sample(n = 5),
        df_pred %>% filter(Genetic_sex == "Female" & pred_error < 0.1) %>% slice_sample(n = 5),
        df_pred %>% filter(Genetic_sex == "Female" & pred_error > 0.9) %>% slice_sample(n = 5),
        df_pred %>% filter(Genetic_sex == "Female" & Probability_male_mean < 0.6 & Probability_male_mean > 0.4) %>% slice_sample(n = 5)
        ) %>% arrange(ID) %>% pull(ID)

    plt_width = ifelse(preds == "all", 20, 7)

    for (id_to_plot in ids_to_plot) {

        # Plot IGL variability
        df_pred_tmp = df_pred %>%
            filter(ID == id_to_plot) %>%
            pivot_longer(cols=-c(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, train, Probability_male_mean, Probability_male_sd, pred_error), names_to = "Probability_male_run", values_to = "Probability_male_value")

        g = rasterGrob(t(scale_gender_cont[seq(20,80)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)
        plt_igl = ggplot(df_pred_tmp, aes(x = factor(Sex, labels=c("")), y=Probability_male_value)) + 
            annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
            geom_jitter(width=0.2, height=0, alpha=0.5) + 
            geom_errorbar(aes(ymin = Probability_male_mean - Probability_male_sd, ymax = Probability_male_mean + Probability_male_sd), width = 0.6, linewidth=1, color = "black") +
            geom_hline(yintercept=unique(df_pred_tmp$Probability_male_mean), linewidth=2) +
            ggtitle(paste0(
                "\nSex = ", unique(df_pred_tmp$Genetic_sex),
                "\nIGL mean (SD) = ", round(unique(df_pred_tmp$Probability_male_mean),3), " (", round(unique(df_pred_tmp$Probability_male_sd),3),")"
            )) +
            coord_flip() +
            scale_x_discrete(name="") +
            scale_y_continuous(name = "", limits = c(0,1), expand = c(0, 0), breaks=seq(0,1, by=0.2)) +
            coord_flip() +
            theme_light() + 
            theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5, size=20, face="bold"),
                legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13))

        # Plot SHAP value contributions
        df_shap_tmp = df_shap %>%
            filter(ID == id_to_plot) %>%
            rename_with(~ str_remove(.x, "^shap_")) %>%
            pivot_longer(cols=-c(ID, Fold, impdf, seed, heldout), names_to = "Variable", values_to = "SHAP_value") %>%
            mutate(run = factor(paste0("k",Fold,"_i",impdf,"_s",seed))) %>%
            left_join(var_df, by="Variable")

        df_shap_tmp_avg = df_shap_tmp %>%
            group_by(Variable) %>%
            summarize(
                avg_shap = mean(SHAP_value, na.rm = TRUE),
                sd_shap = sd(SHAP_value, na.rm = TRUE),
            ) %>%
            left_join(var_df, by="Variable") %>%
            mutate(Variable_clean = factor(Variable_clean)) %>%
            mutate(
                Variable_clean = fct_reorder(Variable_clean, avg_shap),
                direction = factor(ifelse(avg_shap < 0, "Female", "Male"), levels=c("Female", "Male"))
            )

        plt_shap = ggplot(df_shap_tmp_avg, aes(x=Variable_clean, y=avg_shap)) +
            geom_col(aes(fill=direction)) + 
            geom_errorbar(aes(ymin = avg_shap - sd_shap, ymax = avg_shap + sd_shap), width = 0.6, linewidth=1, color = "black") +
            geom_jitter(data = df_shap_tmp, aes(x=factor(Variable_clean), y=SHAP_value, group=run), size = 0.5, width = 0.2, height = 0, alpha=0.5, color="black") +
            geom_hline(yintercept=0) +
            # ggtitle("") +
            scale_x_discrete(name="") +
            scale_y_continuous(name="Importance * Direction") +
            scale_fill_manual(values=scale_gender_dis) +
            coord_flip() +
            facet_wrap(~Category, nrow=1, scales="free_y") +
            theme_light() + 
            theme(text = element_text(size=15), plot.title = element_text(hjust = 0.5, size=20, face="bold"),
                legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
                strip.text = element_text(size = 20, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"))

        plt_igl / plt_shap + plot_layout(heights = c(1,10))
        ggsave(paste0(out, "shap_", id_to_plot, ".png"), width=plt_width, height=8)
        print(paste0(out, "shap_", id_to_plot, ".png"))
    }

}

#endregion

#region Make plots for each gender score

# 28 analyses
name_analyses = c(
    "all_tp0", "all_tp1", "all_tp2", "all_tp3",
    # Matched
    "all_tp0_match", "all_tp1_match", "all_tp2_match", "all_tp3_match",
    "eth_white_tp0_match", "eth_asian_tp0_match", "eth_black_tp0_match", "eth_chinese_tp0_match", "eth_southasian_tp0_match", "eth_caribbean_tp0_match", "eth_african_tp0_match",
    "gen_silent_tp0_match", "gen_boom1_tp0_match", "gen_boom2_tp0_match", "gen_genx_tp0_match",
    "income_1_tp0_match", "income_2_tp0_match", "income_3_tp0_match", "income_4_tp0_match", "income_5_tp0_match"
)

timepoints = c(
    0,1,2,3,
    # Matched
    0,1,2,3,
    0,0,0,0,0,0,0,
    0,0,0,0,
    0,0,0,0,0
    )

predictors = c(
    "all","all","all","all",
    # Matched
    "all","all","all","all",
    "all","all","all","all","all","all","all",
    "all","all","all","all",
    "all","all","all","all","all"
    )

for (ana in run:run) {
    print(name_analyses[ana])

    model_name = name_analyses[ana]
    outdir = paste0("./visualization/",model_name)
    dir.create(outdir, showWarnings = FALSE)

    # Plot confusion  matrix, performance metrics, roc curve, PR curve
    model_perf = plot_conf_matrix(model_name, paste0(outdir,"/conf_matrix_",model_name,".png"))
    model_plt_metrics = plot_metrics(model_perf[['df']], model_name, paste0(outdir,"/perf_metrics_",model_name,".png"))
    model_roc_curve = plot_roc(model_name, paste0(outdir,"/roc_curve_",model_name,".png"))
    model_pr_curve = plot_precision_recall(model_name, paste0(outdir,"/pr_curve_",model_name,".png"))

    # Plot distritutions of gender scores across males and females
    model_dist = plot_dist_gender(model_name, paste0(outdir,"/gender_dist_",model_name))
    
    # Plot age distribitions by sex
    plot_age_by_sex(model_name, paste0(outdir,"/dist_age_sex_",model_name,".png"))

    # Plot relationships with age
    model_assoc_age = plot_assoc_age(model_name, paste0(outdir,"/assoc_age_",model_name,".png"))
    fwrite(model_assoc_age$lm, paste0("./visualization/",model_name,"/lm_age.tsv"), row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

    # Plot SHAP values by variable
    dir.create(paste0(outdir, "/shap_var"), showWarnings=FALSE)
    model_directions = plot_shap_var(model_name, timepoints[ana], paste0(outdir, "/shap_var/shap_"), plot_shap = TRUE)

    # Plot variable importance (horizontal bar graph)
    model_varimp_bar = plot_varimp_bar(model_name, predictors[ana], paste0(outdir,"/varimp_bar_",model_name))

    # Plot SHAP values (individual people)
    dir.create(paste0(outdir, "/shap_indiv"), showWarnings=FALSE)
    plot_shap_indiv(model_name, predictors[ana], paste0(outdir, "/shap_indiv/"))
}

#endregion
