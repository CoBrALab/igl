
library(glue)
library(ggplot2)
# library(scales)
library(ggnewscale)
library(data.table)
library(tidyverse)
library(patchwork)
library(RMINC)
library(viridis)
library(RColorBrewer)
library(foreach)
library(doParallel)
library(cetcolor)
library(pals)
library(cowplot)
library(splines)
library(forcats)
library(viridis)

dir.create("visualization/dbm", showWarnings=FALSE)
dir.create("results/dbm", showWarnings=FALSE)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

mask=mincGetVolume("../../../UKB/temporary_template/Mask_2mm.mnc")

#region Load data

# Results

name_ana = c("rel_males", "rel_females", "rel_inter", "abs_males", "abs_females", "abs_inter")

results = list()
thresholds = list()
for (n in 1:length(name_ana)) {
    results[[name_ana[n]]] = readRDS(paste0("./results/lm_dbm_",name_ana[n],".rds"))

    FDR = mincFDR(results[[name_ana[n]]], mask= mask)
    thresholds[[n]] = reshape2::melt(print(mincFDR(results[[name_ana[n]]], mask= mask)))

    results[[name_ana[n]]] = cbind(results[[name_ana[n]]], FDR)
    results[[name_ana[n]]] = as.data.frame(results[[name_ana[n]]][which(mask[]>0.5),])
}

# Inclusions
inclusions = as.data.frame(fread("../../QC/inclusions_dbm.txt"))
colnames(inclusions) = "ID"

# Brain inputs

dbm_path_rel = as.data.frame(list.files(path="../../../UKB/DBM_2mm", pattern="*rel_2mm.mnc", full.names=TRUE, recursive=TRUE))
dbm_path_abs = as.data.frame(list.files(path="../../../UKB/DBM_2mm", pattern="*rel_2mm.mnc", full.names=TRUE, recursive=TRUE))
df_brain = cbind(dbm_path_rel, dbm_path_abs)

colnames(df_brain) = c("dbm_path_rel", "dbm_path_abs")

# Gender scores

df_gender = as.data.frame(fread("../gender_score_new/results/all_tp2/gender_score_combined.tsv"))

# Baseline data

var_df = as.data.frame(fread("../gender_score_new/variable_mapping.tsv"))

df_raw = as.data.frame(fread("../impute_input/results/df_imputed_tp_all_combined.tsv"))

#endregion

#region Clean data

# Results

var_to_plot = c("Gender_score", "Gender_score", "Gender_score_SexMale", "Gender_score", "Gender_score", "Gender_score_SexMale")

for (n in 1:length(name_ana)) {
    colnames(results[[name_ana[n]]]) = gsub("[-:]", "_", colnames(results[[name_ana[n]]]))
    colnames(results[[name_ana[n]]]) = gsub("[()]", "", colnames(results[[name_ana[n]]]))

    beta_var = paste0("beta_", var_to_plot[n])
    qval_var = paste0("qvalue_", var_to_plot[n])
    tval_var = paste0("tvalue_", var_to_plot[n])

    name_ana_split = str_split(name_ana[n], "_", simplify = TRUE)

    results[[name_ana[n]]] = results[[name_ana[n]]] %>%
        rename(
            beta_toplot = all_of(beta_var),
            tval_toplot = all_of(tval_var),
            qval_toplot = all_of(qval_var)
        ) %>%
        select(
            # F_statistic,
            # R_squared,
            beta_toplot,
            tval_toplot,
            qval_toplot
        ) %>%
        mutate(
            sig_01 = factor(ifelse(qval_toplot < 0.01, 1, 0)),
            sig_05 = factor(ifelse(qval_toplot < 0.05, 1, 0)),
            sig_10 = factor(ifelse(qval_toplot < 0.10, 1, 0)),
            sig_20 = factor(ifelse(qval_toplot < 0.20, 1, 0))
        ) %>% 
        mutate(
            tval_sig01 = ifelse(sig_01 == 1, tval_toplot, 0),
            tval_sig05 = ifelse(sig_05 == 1, tval_toplot, 0),
            tval_sig10 = ifelse(sig_10 == 1, tval_toplot, 0),
            tval_sig20 = ifelse(sig_20 == 1, tval_toplot, 0),
            beta_sig01 = ifelse(sig_01 == 1, beta_toplot, 0),
            beta_sig05 = ifelse(sig_05 == 1, beta_toplot, 0),
            beta_sig10 = ifelse(sig_10 == 1, beta_toplot, 0),
            beta_sig20 = ifelse(sig_20 == 1, beta_toplot, 0)
        ) %>% 
        mutate(
            abs_rel_type = name_ana[n],
            abs_rel = name_ana_split[1],
            type = name_ana_split[2]
        ) %>% glimpse()

    # For FDR thresholds
    thresholds[[n]] = thresholds[[n]] %>% 
        rename(
            Thresh = "Var1",
            Metric = "Var2",
            Value = "value"
        ) %>%
        mutate(
            Thresh = factor(Thresh),
            Metric = factor(Metric),
            abs_rel_type = name_ana[n],
            abs_rel = name_ana_split[1],
            type = name_ana_split[2]
        )
}

results = do.call(rbind, results)

results = results %>% mutate(
        abs_rel_type = factor(abs_rel_type),
        abs_rel = factor(abs_rel),
        type = factor(type)
    ) %>% glimpse()

thresholds = do.call(rbind, thresholds)

thresholds = thresholds %>% mutate(
        abs_rel_type = factor(abs_rel_type),
        abs_rel = factor(abs_rel),
        type = factor(type)
    ) %>% glimpse()

# Brain data

df_brain = df_brain %>% 
    mutate(ID=str_extract(dbm_path_rel, "(?<=sub-)[^_]+")) %>%
    select(c(ID, dbm_path_rel, dbm_path_abs)) %>%
    filter(!grepl("ses-3", dbm_path_rel)) %>%
    glimpse()


# Gender scores

df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
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
    ) %>%
    filter(instanceID == 2)


#endregion

#region Plot on brain

# Source yohan's ggplot for MRI extension
source("../../yohan_ggplot/plotting_functions.R")
source("../../yohan_ggplot/plotting_functions_labels.R")

anatVol = mincArray(mincGetVolume("../../../UKB/temporary_template/avg.020_2mm.mnc"))
mask=mincGetVolume("../../../UKB/temporary_template/Mask_2mm.mnc")

# Function to plot on brain

# df = results %>% filter(abs_rel == "rel")
# ana_name = "rel"

plot_on_brain = function(df, ana_name) {
 
    dir.create(paste0("visualization/dbm/",ana_name), showWarnings=FALSE)
    dir.create(paste0("results/dbm/",ana_name), showWarnings=FALSE)

    # Load data for slices across different axes
    slices = list()
    slices[[1]] = c(-15, 0, 20) # Slices for z-axis
    slices[[2]] = c(0, 25, 45) # Slices for x-axis
    slices[[3]] = c(-40, 5, 20) # Slices for y-axis
    
    slices_axes = c("z", "x", "y")

    cols_to_viz = colnames(df)[seq(1,17)]

    contour_final = tibble(matrix(nrow = 0, ncol = 11))
    anat_final = tibble(matrix(nrow = 0, ncol = 11))
    n_slices = 0
    for (a in 1:length(slices)) {
        # Load anatomical contours
        cont_df = prepare_label_contours(paste0("../../../UKB/temporary_template/bison_ukbb/RF0_ukbb_bison_Label_2mm.mnc"), "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slice_axis_coordinates = slices[[a]], labels = seq(3,9))
        ifelse(slices_axes[a] == "x", cont_df$contours_df$x_toplot <- cont_df$contours_df$y, ifelse(slices_axes[a] == "y", cont_df$contours_df$x_toplot <- cont_df$contours_df$x, ifelse(slices_axes[a] == "z", cont_df$contours_df$x_toplot <- cont_df$contours_df$x, NA)))
        ifelse(slices_axes[a] == "x", cont_df$contours_df$y_toplot <- cont_df$contours_df$z, ifelse(slices_axes[a] == "y", cont_df$contours_df$y_toplot <- cont_df$contours_df$z, ifelse(slices_axes[a] == "z", cont_df$contours_df$y_toplot <- cont_df$contours_df$y, NA)))
        contour_final = rbind(contour_final, cont_df$contours_df)

        # Load template T1
        anat_df = prepare_masked_anatomy("../../../UKB/temporary_template/avg.020_2mm.mnc", "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slices[[a]])
        ifelse(slices_axes[a] == "x", anat_df$anatomy_df$x_toplot <- anat_df$anatomy_df$y, ifelse(slices_axes[a] == "y", anat_df$anatomy_df$x_toplot <- anat_df$anatomy_df$x, ifelse(slices_axes[a] == "z", anat_df$anatomy_df$x_toplot <- anat_df$anatomy_df$x, NA)))
        ifelse(slices_axes[a] == "x", anat_df$anatomy_df$y_toplot <- anat_df$anatomy_df$z, ifelse(slices_axes[a] == "y", anat_df$anatomy_df$y_toplot <- anat_df$anatomy_df$z, ifelse(slices_axes[a] == "z", anat_df$anatomy_df$y_toplot <- anat_df$anatomy_df$y, NA)))
        anat_df = as.data.frame(anat_df$anatomy_df)
        anat_df$intensity = ifelse(anat_df$intensity > 150, 150, ifelse(anat_df$intensity < 10, 10, anat_df$intensity))
        anat_final = rbind(anat_final, anat_df)

        n_slices = n_slices + length(slices[[a]])
    }

    # Generate figure for each stat
    for (c in 1:length(cols_to_viz)) {

        df_toplot = list()
        i = 1
        for (t in 1:length(unique(df$type))) {
            # Write to mnc
            vol = mask
            vol[mask>0.5] = df %>% filter(type == unique(df$type)[t]) %>% pull(cols_to_viz[c])
            mincWriteVolume(vol, paste0("./results/dbm/",ana_name,"/dbm_",ana_name,"_",unique(df$type)[t],"_",cols_to_viz[c],".mnc"), clobber=TRUE)
            
            n_slices = 0
            for (a in 1:length(slices)) {
                slices_num = seq(n_slices + 1, n_slices + length(slices[[a]]))

                # Load results
                df_tmp = prepare_masked_anatomy(paste0("./results/dbm/",ana_name,"/dbm_",ana_name,"_",unique(df$type)[t],"_",cols_to_viz[c],".mnc"), "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slices[[a]])
                ifelse(slices_axes[a] == "x", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$y, ifelse(slices_axes[a] == "y", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$x, ifelse(slices_axes[a] == "z", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$x, NA)))
                ifelse(slices_axes[a] == "x", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$z, ifelse(slices_axes[a] == "y", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$z, ifelse(slices_axes[a] == "z", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$y, NA)))
                df_tmp = as.data.frame(df_tmp$anatomy_df)
                df_tmp$type = unique(df$type)[t]
                df_toplot[[i]] = as.data.frame(df_tmp)

                i = i + 1
                n_slices = n_slices + length(slices[[a]])
            }
        }
        df_toplot = do.call(rbind, df_toplot)

        df_toplot$type = factor(df_toplot$type, levels=c("males", "females", "inter"), labels=c("Males", "Females", "Interaction"))
        df_toplot$intensity = ifelse(df_toplot$intensity < 0.0001 & df_toplot$intensity > -0.0001, NA, df_toplot$intensity)

        # Plot
        plt = ggplot(mapping = aes(x=x_toplot, y_toplot)) +
            geom_raster(data = anat_final %>% filter(mask_value > 0.5), aes(fill = intensity), alpha = 1) +
            scale_fill_gradient(low='black', high= 'white', limits=c(10,150), guide = "none")

        if (is.logical(df_toplot$intensity)) {
            df_toplot$intensity = 0
        }

        # Custom color scales for overlay
        if (grepl("beta|tval", cols_to_viz[c])) {
            limit = max(abs(df_toplot$intensity), na.rm=TRUE)
            plt = plt + new_scale_fill() +
                scale_fill_gradientn(colours=scale_gender_cont, limits=c(-limit,limit), breaks=c(-round(limit,2),0,round(limit,2)), name="", na.value = "transparent")
        } else {
            limit = max(abs(df_toplot$intensity), na.rm=TRUE)
            plt = plt + new_scale_fill() +
                scale_fill_viridis(option="A", limits=c(0,round(limit,2)), breaks=c(0,round(limit,2)), name="", na.value="transparent")
        }

        plt = plt + 
            geom_raster(data = df_toplot %>% filter(mask_value > 0.5), aes(fill = intensity)) +
            geom_path(data = contour_final, aes(group=interaction(label, obj)), size=0.1, color="black") +
            coord_fixed(ratio = 1) +
            facet_grid(type~slice_index, switch = "y") +
            labs(title = cols_to_viz[c]) +
            theme_void() +
            theme(plot.background = element_rect(fill = "white", color = NA),
                plot.title = element_text(hjust = 0.5, size=20),
                strip.text.y = element_text(size=20, hjust = 0.64),
                strip.text.x=element_text(size=0.01),
                strip.placement = "outside",
                panel.spacing = unit(0, "lines"),
                plot.margin = unit(c(0, 0, 0, 0), "cm"))
        
        ggsave(paste0("./visualization/dbm/",ana_name,"/dbm_",ana_name,"_",cols_to_viz[c],".png"), width=10, height=6)
        print(paste0("./visualization/dbm/",ana_name,"/dbm_",ana_name,"_",cols_to_viz[c],".png"))
    
    }

    # Plot unthresholded t-values with contour for significance
    df_toplot = list()
    sig_toplot = list()

    i = 1

    for (t in 1:length(unique(df$type))) {
        # Write to mnc
        sig_toplot[[i]] = tibble(matrix(nrow = 0, ncol = 11))
        n_slices = 0
        for (a in 1:length(slices)) {
            slices_num = seq(n_slices + 1, n_slices + length(slices[[a]]))

            # Load results
            df_tmp = prepare_masked_anatomy(paste0("./results/dbm/",ana_name,"/dbm_",ana_name,"_",unique(df$type)[t],"_tval_toplot.mnc"), "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slices[[a]])
            ifelse(slices_axes[a] == "x", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$y, ifelse(slices_axes[a] == "y", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$x, ifelse(slices_axes[a] == "z", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$x, NA)))
            ifelse(slices_axes[a] == "x", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$z, ifelse(slices_axes[a] == "y", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$z, ifelse(slices_axes[a] == "z", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$y, NA)))
            df_tmp = as.data.frame(df_tmp$anatomy_df)
            df_tmp$type = unique(df$type)[t]
            df_toplot[[i]] = as.data.frame(df_tmp)

            # Load significance contours
            sig_df = prepare_label_contours(paste0("./results/dbm/",ana_name,"/dbm_",ana_name,"_",unique(df$type)[t],"_sig_05.mnc"), "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slice_axis_coordinates = slices[[a]], labels = c(1,2))
            sig_df$contours_df = sig_df$contours_df %>% filter(label == 2)
            ifelse(slices_axes[a] == "x", sig_df$contours_df$x_toplot <- sig_df$contours_df$y, ifelse(slices_axes[a] == "y", sig_df$contours_df$x_toplot <- sig_df$contours_df$x, ifelse(slices_axes[a] == "z", sig_df$contours_df$x_toplot <- sig_df$contours_df$x, NA)))
            ifelse(slices_axes[a] == "x", sig_df$contours_df$y_toplot <- sig_df$contours_df$z, ifelse(slices_axes[a] == "y", sig_df$contours_df$y_toplot <- sig_df$contours_df$z, ifelse(slices_axes[a] == "z", sig_df$contours_df$y_toplot <- sig_df$contours_df$y, NA)))
            sig_df$contours_df$type = unique(df$type)[t]
            sig_toplot[[i]] = sig_df$contours_df

            i = i + 1
            n_slices = n_slices + length(slices[[a]])
        }
    }
    df_toplot = do.call(rbind, df_toplot)
    sig_toplot = do.call(rbind, sig_toplot)

    df_toplot$type = factor(df_toplot$type, levels=c("males", "females", "inter"), labels=c("Males", "Females", "Interaction"))
    df_toplot$intensity = ifelse(df_toplot$intensity < 0.0001 & df_toplot$intensity > -0.0001, NA, df_toplot$intensity)

    sig_toplot$type = factor(sig_toplot$type, levels=c("males", "females", "inter"), labels=c("Males", "Females", "Interaction"))

    # Plot
    limit = max(abs(df_toplot$intensity), na.rm=TRUE)

    for (t in c("Males", "Females", "Interaction")) {

        df_tmp = df_toplot %>% filter(type == t)
        sig_tmp = sig_toplot %>% filter(type == t)

        if (is.logical(df_tmp$intensity)) {
            df_tmp$intensity = 0
        }

        plt = ggplot(mapping = aes(x=x_toplot, y_toplot)) +
            geom_raster(data = anat_final %>% filter(mask_value > 0.5), aes(fill = intensity), alpha = 1) +
            scale_fill_gradient(low='black', high= 'white', limits=c(10,150), guide = "none") + 
            new_scale_fill() +
            scale_fill_gradientn(colours=scale_gender_cont, limits=c(-limit,limit), breaks=c(-round(limit,2),0,round(limit,2)), name="", na.value = "transparent") +
            geom_raster(data = df_tmp %>% filter(mask_value > 0.5), aes(fill = intensity)) +
            geom_path(data = contour_final, aes(group=interaction(label, obj)), size=0.1, color="#4d4d4d") +
            geom_path(data = sig_tmp, aes(group=interaction(label, obj)), size=0.3, color="black") +
            coord_fixed(ratio = 1) +
            facet_grid(slice_axis~slice_index, switch = "y") +
            labs(title = "t-values, FDR 5%") +
            theme_void() +
            theme(plot.background = element_rect(fill = "white", color = NA),
                plot.title = element_text(hjust = 0.5, size=20),
                strip.text.y = element_text(size=20, hjust = 0.64),
                strip.text.x=element_text(size=0.01),
                strip.placement = "outside",
                panel.spacing = unit(0, "lines"),
                plot.margin = unit(c(0, 0, 0, 0), "cm"))
        
        ggsave(paste0("./visualization/dbm/",ana_name,"/dbm_",ana_name,"_tval_all_sig05_",t,".png"), width=10, height=10)
        print(paste0("./visualization/dbm/",ana_name,"/dbm_",ana_name,"_tval_all_sig05_",t,".png"))
    }
}

# df = results %>% filter(abs_rel == "rel")
# ana_name = "rel"

plot_on_brain_explore = function(df, ana_name) {

    dir.create(paste0("visualization/dbm/",ana_name), showWarnings=FALSE)
    dir.create(paste0("results/dbm/",ana_name), showWarnings=FALSE)

    # Load data for slices across different axes
    slices = list()
    slices[[1]] = seq(-50,70, by=5) # Slices for z-axis
    slices[[2]] = seq(-50,70, by=5) # Slices for x-axis
    slices[[3]] = seq(-50,70, by=5) # Slices for y-axis

    slices_axes = c("z", "x", "y")

    cols_to_viz = colnames(df)[seq(1,17)]

    contour_final = tibble(matrix(nrow = 0, ncol = 11))
    anat_final = tibble(matrix(nrow = 0, ncol = 11))
    n_slices = 0
    for (a in 1:length(slices)) {
        # Load anatomical contours
        cont_df = prepare_label_contours(paste0("../../../UKB/temporary_template/bison_ukbb/RF0_ukbb_bison_Label_2mm.mnc"), "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slice_axis_coordinates = slices[[a]], labels = seq(3,9))
        ifelse(slices_axes[a] == "x", cont_df$contours_df$x_toplot <- cont_df$contours_df$y, ifelse(slices_axes[a] == "y", cont_df$contours_df$x_toplot <- cont_df$contours_df$x, ifelse(slices_axes[a] == "z", cont_df$contours_df$x_toplot <- cont_df$contours_df$x, NA)))
        ifelse(slices_axes[a] == "x", cont_df$contours_df$y_toplot <- cont_df$contours_df$z, ifelse(slices_axes[a] == "y", cont_df$contours_df$y_toplot <- cont_df$contours_df$z, ifelse(slices_axes[a] == "z", cont_df$contours_df$y_toplot <- cont_df$contours_df$y, NA)))
        cont_df$contours_df$slice_index = cont_df$contours_df$slice_index + n_slices
        contour_final = rbind(contour_final, cont_df$contours_df)

        # Load template T1
        anat_df = prepare_masked_anatomy("../../../UKB/temporary_template/avg.020_2mm.mnc", "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slices[[a]])
        ifelse(slices_axes[a] == "x", anat_df$anatomy_df$x_toplot <- anat_df$anatomy_df$y, ifelse(slices_axes[a] == "y", anat_df$anatomy_df$x_toplot <- anat_df$anatomy_df$x, ifelse(slices_axes[a] == "z", anat_df$anatomy_df$x_toplot <- anat_df$anatomy_df$x, NA)))
        ifelse(slices_axes[a] == "x", anat_df$anatomy_df$y_toplot <- anat_df$anatomy_df$z, ifelse(slices_axes[a] == "y", anat_df$anatomy_df$y_toplot <- anat_df$anatomy_df$z, ifelse(slices_axes[a] == "z", anat_df$anatomy_df$y_toplot <- anat_df$anatomy_df$y, NA)))
        anat_df = as.data.frame(anat_df$anatomy_df)
        anat_df$slice_index = anat_df$slice_index + n_slices
        anat_df$intensity = ifelse(anat_df$intensity > 150, 150, ifelse(anat_df$intensity < 10, 10, anat_df$intensity))
        anat_final = rbind(anat_final, anat_df)

        n_slices = n_slices + length(slices[[a]])
    }

    # Plot unthresholded t-values with contour for significance
    df_toplot = list()
    sig_toplot = list()

    i = 1

    for (t in 1:length(unique(df$type))) {
        sig_toplot[[i]] = tibble(matrix(nrow = 0, ncol = 11))
        n_slices = 0
        for (a in 1:length(slices)) {
            slices_num = seq(n_slices + 1, n_slices + length(slices[[a]]))

            # Load results
            df_tmp = prepare_masked_anatomy(paste0("./results/dbm/",ana_name,"/dbm_",ana_name,"_",unique(df$type)[t],"_tval_toplot.mnc"), "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slices[[a]])
            ifelse(slices_axes[a] == "x", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$y, ifelse(slices_axes[a] == "y", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$x, ifelse(slices_axes[a] == "z", df_tmp$anatomy_df$x_toplot <- df_tmp$anatomy_df$x, NA)))
            ifelse(slices_axes[a] == "x", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$z, ifelse(slices_axes[a] == "y", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$z, ifelse(slices_axes[a] == "z", df_tmp$anatomy_df$y_toplot <- df_tmp$anatomy_df$y, NA)))
            df_tmp = as.data.frame(df_tmp$anatomy_df)
            df_tmp$slice_index = df_tmp$slice_index + n_slices
            df_tmp$type = unique(df$type)[t]
            df_toplot[[i]] = as.data.frame(df_tmp)

            # Load significance contours
            sig_df = prepare_label_contours(paste0("./results/dbm/",ana_name,"/dbm_",ana_name,"_",unique(df$type)[t],"_sig_05.mnc"), "../../../UKB/temporary_template/Mask_2mm.mnc", slice_axis = slices_axes[a], slice_axis_coordinates = slices[[a]], labels = c(1,2))
            sig_df$contours_df = sig_df$contours_df %>% filter(label == 2)
            ifelse(slices_axes[a] == "x", sig_df$contours_df$x_toplot <- sig_df$contours_df$y, ifelse(slices_axes[a] == "y", sig_df$contours_df$x_toplot <- sig_df$contours_df$x, ifelse(slices_axes[a] == "z", sig_df$contours_df$x_toplot <- sig_df$contours_df$x, NA)))
            ifelse(slices_axes[a] == "x", sig_df$contours_df$y_toplot <- sig_df$contours_df$z, ifelse(slices_axes[a] == "y", sig_df$contours_df$y_toplot <- sig_df$contours_df$z, ifelse(slices_axes[a] == "z", sig_df$contours_df$y_toplot <- sig_df$contours_df$y, NA)))
            sig_df$contours_df$slice_index = sig_df$contours_df$slice_index + n_slices
            sig_df$contours_df$type = unique(df$type)[t]
            sig_toplot[[i]] = sig_df$contours_df

            i = i + 1
            n_slices = n_slices + length(slices[[a]])
        }
    }
    df_toplot = do.call(rbind, df_toplot)
    sig_toplot = do.call(rbind, sig_toplot)

    df_toplot$type = factor(df_toplot$type, levels=c("males", "females", "inter"), labels=c("Males", "Females", "Interaction"))
    df_toplot$intensity = ifelse(df_toplot$intensity < 0.0001 & df_toplot$intensity > -0.0001, NA, df_toplot$intensity)

    sig_toplot$type = factor(sig_toplot$type, levels=c("males", "females", "inter"), labels=c("Males", "Females", "Interaction"))

    # Plot
    for (a in slices_axes) {

        df_tmp = df_toplot %>% filter(slice_axis == a)
        sig_tmp = sig_toplot %>% filter(slice_axis == a)

        if (is.logical(df_tmp$intensity)) {
            df_tmp$intensity = 0
        }

        limit = max(abs(df_tmp$intensity), na.rm=TRUE)

        slice_labels = df_tmp %>%
            distinct(type, slice_index, slice_axis, slice_world) %>%
            mutate(
                label = paste0(slice_axis, slice_world),
                x = Inf,
                y = Inf
            )

        plt = ggplot(mapping = aes(x=x_toplot, y_toplot)) +
            geom_raster(data = anat_final %>% filter(mask_value > 0.5, slice_axis == a), aes(fill = intensity), alpha = 1) +
            scale_fill_gradient(low='black', high= 'white', limits=c(10,150), guide = "none") + 
            new_scale_fill() +
            scale_fill_gradientn(colours=scale_gender_cont, limits=c(-limit,limit), breaks=c(-round(limit,2),0,round(limit,2)), name="", na.value = "transparent") +
            geom_raster(data = df_tmp %>% filter(mask_value > 0.5), aes(fill = intensity)) +
            geom_path(data = contour_final %>% filter(slice_axis == a), aes(group=interaction(label, obj)), size=0.1, color="#4d4d4d") +
            geom_path(data = sig_tmp, aes(group=interaction(label, obj)), size=0.2, color="black") +
            geom_text(data = slice_labels, aes(x = x, y = y, label = label), inherit.aes = FALSE, hjust = 1.05, vjust = 1.2, size = 5, color = "black") +
            coord_fixed(ratio = 1) +
            facet_grid(type~slice_index, switch = "y") +
            labs(title = "t-values, FDR 5%") +
            theme_void() +
            theme(plot.background = element_rect(fill = "white", color = NA),
                plot.title = element_text(hjust = 0.5, size=20),
                strip.text.y = element_text(size=20, hjust = 0.64),
                strip.text.x=element_text(size=0.01),
                strip.placement = "outside",
                panel.spacing = unit(0, "lines"),
                plot.margin = unit(c(0, 0, 0, 0), "cm"))
        
        ggsave(paste0("./visualization/dbm/",ana_name,"/dbm_",ana_name,"_tval_explore_sig05_",a,".png"), width=30, height=6)
        print(paste0("./visualization/dbm/",ana_name,"/dbm_",ana_name,"_tval_explore_sig05_",a,".png"))
    }
}

# Run for each analysis

# Plot results on brain
plot_on_brain(results %>% filter(abs_rel == "abs"), "abs")
plot_on_brain(results %>% filter(abs_rel == "rel"), "rel")

plot_on_brain_explore(results %>% filter(abs_rel == "abs"), "abs")
plot_on_brain_explore(results %>% filter(abs_rel == "rel"), "rel")

#endregion

#region Counts in Allen labels

allen_mapping = as.data.frame(fread("../../allen/voxel_count.csv"))
labels = as.data.frame(fread("../../allen/label_values.txt"))$V1
label_counts = as.data.frame(fread("../../allen/label_counts_ukb_2mm.txt"))$V1

results_females_fem = as.data.frame(fread("results/dbm/rel/dbm_rel_females_sig05_fem_allen.txt"))$V1
results_females_masc = as.data.frame(fread("results/dbm/rel/dbm_rel_females_sig05_masc_allen.txt"))$V1
results_males_fem = as.data.frame(fread("results/dbm/rel/dbm_rel_males_sig05_fem_allen.txt"))$V1
results_males_masc = as.data.frame(fread("results/dbm/rel/dbm_rel_males_sig05_masc_allen.txt"))$V1

results_counts = data.frame(
    id = labels,
    label_counts = label_counts,
    counts_females_fem = results_females_fem,
    counts_females_masc = results_females_masc,
    counts_males_fem = results_males_fem,
    counts_males_masc = results_males_masc
)

allen_mapping = allen_mapping %>%
    filter(voxel_count != "NA") %>%
    select(id, acronym, name)

results_counts = results_counts %>%
    left_join(allen_mapping, by="id") %>%
    select(id, acronym, name, label_counts, everything()) %>%
    mutate(
        perc_females_fem = counts_females_fem / label_counts,
        perc_females_masc = counts_females_masc / label_counts,
        perc_males_fem = counts_males_fem / label_counts,
        perc_males_masc = counts_males_masc / label_counts
    ) %>%
    glimpse()

fwrite(results_counts, "results/dbm/rel/allen_sig_counts.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

# Plot
df_counts = results_counts %>%
    filter(label_counts > 10) %>%
    filter(if_any(starts_with("counts_"), ~ .x > 0)) %>%
    pivot_longer(cols = matches("^(counts|perc)_"), names_to = c("type", "sex_gen"), names_pattern = "(counts|perc)_(.*)",values_to = "counts_perc") %>%
    mutate(
        sex_gen = factor(sex_gen, levels = c("females_fem", "females_masc", "males_fem", "males_masc"), labels=c("Females - feminine IGL", "Females - masculine IGL", "Males - feminine IGL", "Males - masculine IGL")),
        type = factor(type, levels = c("counts", "perc"))
    ) %>%
    glimpse()

# Counts
df_counts %>% 
    filter(type == "counts") %>%
    group_by(name) %>%
    mutate(total_counts = sum(counts_perc, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
        name = fct_reorder(name, total_counts, .desc = TRUE),
        counts_perc = ifelse(counts_perc == 0, NA, counts_perc)
    ) %>%
ggplot(aes(x=name, y=sex_gen, fill=counts_perc)) + 
    geom_tile() + 
    scale_fill_viridis_c(option="D", direction = 1, guide="none", na.value = "transparent") +
    scale_y_discrete(name="") + 
    scale_x_discrete(name="") +
    theme_light() + 
    theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5, size=10, face="bold"),
        axis.text.x = element_text(angle=30, hjust=1, vjust=1),
        legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size=10),
        strip.text = element_text(size = 15, color="black", face="bold"), strip.background = element_rect(fill="#ffffff")) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE))

ggsave("./visualization/dbm/rel/allen_sig_counts.png", height=7, width=25)

# Percentages
df_counts %>% 
    filter(type == "perc") %>%
    group_by(name) %>%
    mutate(total_counts = sum(counts_perc, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
        name = fct_reorder(name, total_counts, .desc = TRUE),
        counts_perc = ifelse(counts_perc == 0, NA, counts_perc)
    ) %>%
ggplot(aes(x=name, y=sex_gen, fill=counts_perc)) + 
    geom_tile() + 
    scale_fill_viridis_c(option="D", direction = 1, guide="none", na.value = "transparent") +
    scale_y_discrete(name="") + 
    scale_x_discrete(name="") +
    theme_light() + 
    theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5, size=10, face="bold"),
        axis.text.x = element_text(angle=30, hjust=1, vjust=1),
        legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size=10),
        strip.text = element_text(size = 15, color="black", face="bold"), strip.background = element_rect(fill="#ffffff")) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE))

ggsave("./visualization/dbm/rel/allen_sig_perc.png", height=7, width=25)

#endregion
