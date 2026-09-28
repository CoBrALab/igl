
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
library(cowplot)
library(ggpubr)
library(ggsignif)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
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

#region Functions

# names_ana = c("all_tp0", "all_tp1", "all_tp2", "all_tp3")
# names_clean = c("TP0", "TP1", "TP2", "TP3")
# out = "test_compare"

compare_lm = function(names_ana, names_clean, out) {

    # Load data
    df_tmp = list()
    for (i in 1:length(names_ana)) {
        df_tmp[[i]] = as.data.frame(fread(paste0("./results/",names_ana[i],"/shap_mean_abs.tsv")))
        df_tmp[[i]]$name_ana = names_ana[i]
    }
    df_tmp = do.call(rbind, df_tmp)

    df_tmp = df_tmp %>%
        mutate(
            run = factor(run),
            direction_pearson = factor(direction_pearson, levels=c("Female", "Male")),
            Variable_clean = factor(Variable_clean),
            Category = factor(Category),
            name_ana = factor(name_ana, levels=names_ana, labels=names_clean)
        ) %>%
        select(run, mean_abs_shap, direction_pearson, Variable_clean, Category, name_ana)

    # Run lm
    results = list()

    list_vars = unique(df_tmp$Variable_clean)
    for (i in 1:length(list_vars)) {

        df_lm = df_tmp %>%
            filter(Variable_clean == list_vars[i])

        lm_tmp = summary(lm(scale(mean_abs_shap) ~ name_ana, data=df_lm))

        results[[i]] = list()
        for (n in 2:length(names_ana)) {
            results[[i]][[n-1]] = data.frame(
                Variable_clean = list_vars[i],
                Category = unique(df_lm$Category),
                ref_categ = names_clean[1],
                name_ana = names_clean[n],
                coef_lm = lm_tmp$coefficients[paste0("name_ana",names_clean[n]),'Estimate'],
                pval_lm = lm_tmp$coefficients[paste0("name_ana",names_clean[n]),'Pr(>|t|)']
            )
        }
        results[[i]] = do.call(rbind, results[[i]])

        aov_tmp = summary(aov(mean_abs_shap ~ name_ana, data=df_lm))
        results[[i]]$pval_aov = aov_tmp[[1]]["name_ana","Pr(>F)"]
    }
    results = do.call(rbind, results)

    # Multiple comparison corrections
    results$pval_lm_fdr = p.adjust(results$pval_lm, method="fdr")
    results$pval_lm_bonf = p.adjust(results$pval_lm, method="bonferroni")
    results$pval_aov_bonf = pmin(1, (results$pval_aov * length(list_vars)))

    fwrite(results, paste0("./results/", out, "/comp_results.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
}

# Pairwise comparison of AUCs
compare_auc = function(names_ana, names_clean, out, eval_set = "train") {

    # Load data and build one ROC per analysis
    rocobj = list()
    aucs = list()
    for (i in 1:length(names_ana)) {
        df_tmp = as.data.frame(fread(paste0("./results/",names_ana[i],"/gender_score_combined.tsv"),
                                     select = c("ID", "Genetic_sex", "train", "Probability_male_mean")))
        df_tmp = df_tmp %>% filter(train == eval_set)

        rocobj[[i]] = roc(df_tmp$Genetic_sex, df_tmp$Probability_male_mean,
                          levels = c("Female", "Male"), direction = "<", quiet = TRUE)
        ci_tmp = ci.auc(rocobj[[i]], method = "delong")

        aucs[[i]] = data.frame(
            name_ana = names_ana[i],
            ana_name = names_clean[i],
            n        = nrow(df_tmp),
            AUC      = as.numeric(auc(rocobj[[i]])),
            ci_lo    = as.numeric(ci_tmp[1]),
            ci_hi    = as.numeric(ci_tmp[3]),
            stringsAsFactors = FALSE
        )
    }
    aucs = do.call(rbind, aucs)

    fwrite(aucs, paste0("./results/", out, "/auc_results_", eval_set, ".tsv"),
           col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

    # All pairwise comparisons (unpaired DeLong test)
    pairs = combn(length(names_ana), 2)

    results = list()
    for (p in 1:ncol(pairs)) {
        i = pairs[1,p]
        j = pairs[2,p]

        tt = roc.test(rocobj[[i]], rocobj[[j]], method = "delong", paired = FALSE)

        results[[p]] = data.frame(
            group_1   = names_clean[i],
            group_2   = names_clean[j],
            n_1       = aucs$n[i],
            n_2       = aucs$n[j],
            auc_1     = aucs$AUC[i],
            auc_2     = aucs$AUC[j],
            delta_auc = aucs$AUC[i] - aucs$AUC[j],
            pval      = as.numeric(tt$p.value),
            stringsAsFactors = FALSE
        )
    }
    results = do.call(rbind, results)

    # Multiple comparison corrections (within this comparison set)
    results$pval_bonf = pmin(1, results$pval * nrow(results))
    results$pval_fdr  = p.adjust(results$pval, method="fdr")

    fwrite(results, paste0("./results/", out, "/auc_comp_results_", eval_set, ".tsv"),
           col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
    print(paste0("./results/", out, "/auc_comp_results_", eval_set, ".tsv"))

    # Plot AUC per group (DeLong 95% CI)
    aucs_plot = aucs %>% mutate(ana_name = factor(ana_name, levels=names_clean))

    step = 0.005   # tight vertical spacing between stacked brackets
    df_sig_pairs = results %>%
        rstatix::add_significance("pval", cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns")) %>%
        mutate(
            xmin = pmin(match(group_1, names_clean), match(group_2, names_clean)),
            xmax = pmax(match(group_1, names_clean), match(group_2, names_clean))
        ) %>%
        arrange(xmax - xmin) %>%
        mutate(y_position = max(aucs_plot$ci_hi, na.rm=TRUE) + step * row_number())

    plt_auc = ggplot(aucs_plot, aes(x=ana_name, y=AUC, fill=ana_name)) +
        geom_col(width=0.6) +
        geom_errorbar(aes(ymin=ci_lo, ymax=ci_hi), width=0.2, linewidth=0.8) +
        geom_signif(data=df_sig_pairs,
                    aes(xmin=xmin, xmax=xmax, y_position=y_position, annotations=pval.signif,
                        group=seq_len(nrow(df_sig_pairs))),
                    manual=TRUE, inherit.aes=FALSE, tip_length=0.01, textsize=4.5) +
        scale_fill_viridis_d(option="viridis", begin=0, end=0.9, guide="none") +
        scale_y_continuous(name="AUC") +
        scale_x_discrete(name="") +
        coord_cartesian(ylim = c(min(aucs$ci_lo), max(df_sig_pairs$y_position) + step)) +
        labs(title = "AUC") +
        theme_light() +
        theme(text = element_text(size=18),
              plot.title = element_text(hjust=0.5, size=16, face="bold"),
              plot.subtitle = element_text(hjust=0.5, size=10),
              axis.text.x = element_text(angle=45, hjust=1))

    ggsave(paste0("./visualization/",out,"/",out,"_auc_barplot.png"), width = 5, height = 5)
    print(paste0("./visualization/",out,"/",out,"_auc_barplot.png"))
}

# names_ana = c("all_tp0", "all_tp1", "all_tp2", "all_tp3")
# names_clean = c("TP0", "TP1", "TP2", "TP3")
# out = "test_compare"

plot_bar = function(names_ana, names_clean, out) {

    # Load data
    df_tmp = list()
    for (i in 1:length(names_ana)) {
        df_tmp[[i]] = as.data.frame(fread(paste0("./results/",names_ana[i],"/shap_mean_abs.tsv")))
        df_tmp[[i]]$name_ana = names_ana[i]
        df_tmp_avg = as.data.frame(fread(paste0("./results/",names_ana[i],"/shap_mean_abs_avg.tsv")))
        df_tmp_avg$name_ana = names_ana[i]

        df_tmp[[i]] = df_tmp[[i]] %>%
            left_join(
                df_tmp_avg %>% select(Variable, avg_mean_abs_shap_nodir, sd_mean_abs_shap_nodir, name_ana, direction_pearson) %>% rename(direction_pearson_avg = "direction_pearson"),
                by=c("Variable", "name_ana")
            )
    }
    df_tmp = do.call(rbind, df_tmp)

    df_tmp = df_tmp %>%
        mutate(
            run = factor(run),
            direction_pearson = factor(direction_pearson, levels=c("Female", "Male")),
            Variable_clean = factor(Variable_clean),
            Category = factor(Category),
            name_ana = factor(name_ana, levels=names_ana, labels=names_clean),
            direction_pearson_avg = factor(direction_pearson_avg, levels=c("Female", "Male"))
        ) %>%
        select(run, mean_abs_shap, direction_pearson, Variable_clean, Category, name_ana, direction_pearson_avg, avg_mean_abs_shap_nodir, sd_mean_abs_shap_nodir)

    df_tmp_avg = df_tmp %>%
        group_by(Variable_clean, name_ana) %>%
        slice(1) %>%
        select(-c(run, mean_abs_shap, direction_pearson)) %>%
        ungroup() %>%
        mutate(
            Variable_clean = fct_reorder(Variable_clean, avg_mean_abs_shap_nodir, .fun = mean, .na_rm = TRUE),
            name_ana = factor(name_ana, levels=rev(names_clean))
        )

    df_results = as.data.frame(fread(paste0("./results/",out,"/comp_results.tsv")))
    df_results = df_results %>%
        mutate(
            Variable_clean = factor(Variable_clean),
            Category       = factor(Category),
            name_ana       = factor(name_ana)
        )
    df_sig = df_results %>%
        filter(pval_aov_bonf < 0.05) %>%
        distinct(Variable_clean, Category)

    df_ypos = df_tmp_avg %>%
        group_by(Variable_clean, Category) %>%
        summarise(ymax = max(avg_mean_abs_shap_nodir + sd_mean_abs_shap_nodir, na.rm = TRUE), .groups = "drop")

    df_sig = df_sig %>%
        left_join(df_ypos, by = c("Variable_clean", "Category")) %>%
        mutate(ypos = ymax + 0.005)

    # Plot
    plt = ggplot(df_tmp_avg) +
        geom_col(aes(x=Variable_clean, y=avg_mean_abs_shap_nodir, fill=direction_pearson_avg, alpha=name_ana, group=name_ana), position = position_dodge(width = 0.9), width=0.9) +
        geom_linerange(aes(x=Variable_clean, y=avg_mean_abs_shap_nodir, ymin = avg_mean_abs_shap_nodir - sd_mean_abs_shap_nodir, ymax = avg_mean_abs_shap_nodir + sd_mean_abs_shap_nodir, group=name_ana), position = position_dodge(width = 0.9), linewidth=1, color = "black") +
        geom_text(data = df_sig, aes(x = Variable_clean, y = ypos, label = "*"), color = "black", size = 8) +
        geom_hline(yintercept=0) +
        geom_vline(data = df_tmp_avg %>% group_by(Category) %>% mutate(ypos = as.numeric(factor(Variable_clean)) - 0.5) %>% distinct(Category, ypos), aes(xintercept = ypos), color = "black", linewidth = 0.1) +
        scale_x_discrete(name="") +
        scale_y_continuous(name="Mean |SHAP|", breaks=seq(0,1, by=0.05)) +
        scale_alpha_discrete(range = c(1, 0.3), guide = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values=scale_gender_dis) +
        coord_flip() +
        facet_wrap(~Category, nrow=1, scales="free_y") +
        theme_light() +
        theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5, size=20, face="bold"),
            legend.title = element_blank(), legend.text=element_text(size=13),
            strip.text = element_text(size = 30, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
            panel.grid.major.y = element_blank(), panel.grid.minor.y = element_line(color = "black", linewidth = 0.3))
    ggsave(paste0("./visualization/",out,"/",out,"_bar_",sig_suffix,".png"), width=30, height=14)
    print(paste0("./visualization/",out,"/",out,"_bar_",sig_suffix,".png"))

}

#endregion

#region Run for each comparison

names_ana_list = list()
names_clean_list = list()

out_list = c(
    "all_compare",
    "all_compare_match", "eth_compare_match", "eth_compareAsian_match", "eth_compareBlack_match", "gen_compare_match", "income_compare_match"
)

names_ana_list[[1]] = c("all_tp0", "all_tp1", "all_tp2", "all_tp3")
# Matched
names_ana_list[[2]] = c("all_tp0_match", "all_tp1_match", "all_tp2_match", "all_tp3_match")
names_ana_list[[3]] = c("eth_white_tp0_match", "eth_asian_tp0_match", "eth_black_tp0_match")
names_ana_list[[4]] = c("eth_southasian_tp0_match", "eth_chinese_tp0_match")
names_ana_list[[5]] = c("eth_caribbean_tp0_match", "eth_african_tp0_match")
names_ana_list[[6]] = c("gen_silent_tp0_match", "gen_boom1_tp0_match", "gen_boom2_tp0_match", "gen_genx_tp0_match")
names_ana_list[[7]] = c("income_1_tp0_match", "income_2_tp0_match", "income_3_tp0_match", "income_4_tp0_match", "income_5_tp0_match")

names_clean_list[[1]] = c("TP0", "TP1", "TP2", "TP3")
# Matched
names_clean_list[[2]] = c("TP0", "TP1", "TP2", "TP3")
names_clean_list[[3]] = c("White", "Asian", "Black")
names_clean_list[[4]] = c("South asian", "Chinese")
names_clean_list[[5]] = c("Caribbean", "African")
names_clean_list[[6]] = c("Silent", "Boom 1", "Boom 2", "Gen X")
names_clean_list[[7]] = c("<18k", "18k to 31k", "31k to 52k", "52k to 100k", ">100k")

for (i in 1:length(out_list)) {

    print(out_list[i])

    dir.create(paste0("./results/",out_list[i]), showWarnings = FALSE)
    dir.create(paste0("./visualization/",out_list[i]), showWarnings = FALSE)

    # Statistical comparisons
    compare_lm(names_ana_list[[i]], names_clean_list[[i]], out_list[i])

    # Bar plots
    plot_bar(names_ana_list[[i]], names_clean_list[[i]], out_list[i], use_corrected = TRUE)

    # AUC comparisons
    compare_auc(names_ana_list[[i]], names_clean_list[[i]], out_list[i])
}

#endregion

