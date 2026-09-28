
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
library(stringr)
library(ggseg)
library(ggseg.formats)
library(ggsegDesterieux)
library(ggsegJHU)
library(ggsegICBM)
library(effects)
library(lme4)
library(lmerTest)
library(splines)
library(scales)

desterieux = convert_legacy_brain_atlas(atlas_2d = desterieux, atlas_name = "desterieux", type = "cortical")

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

df_results = as.data.frame(fread("./results/alltab/results_alltab.tsv"))

df_results = df_results %>% 
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    mutate(
        Field_title = factor(Field_title),
        fmrib_name = factor(fmrib_name),
        Category = factor(Category),
        Modality = factor(ifelse(Modality %in% c("tfMRI", "rsfMRI"), "fMRI", Modality))
    )

brain_mapping = as.data.frame(fread("../../../UKB/Analyses/clean_brain_tab/results/column_mapping.tsv"))

#endregion

#region Individual plots for paper

dir.create("./visualization/tab_paper/", showWarnings = FALSE)
dir.create("./results/tab_paper/", showWarnings = FALSE)

# Load data
df_brain = as.data.frame(fread("../../../UKB/Analyses/clean_brain_tab/results/brain_tab_clean_wide.tsv"))
brain_mapping = as.data.frame(fread("../../../UKB/Analyses/clean_brain_tab/results/column_mapping.tsv"))

# Gender scores
df_gender = as.data.frame(fread("../gender_score_new/results/all_tp2/gender_score_combined.tsv"))
df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    glimpse()

# Covariates
df_cov = as.data.frame(fread("../../../UKB/tabular/df_body_cov/UKBB_body_cov_wide.tsv"))
df_cov = df_cov %>%
    filter(InstanceID == 2) %>%
    rename(ID = "SubjectID", height = "Standing height_50", assessment_center = "UK Biobank assessment centre_54", weight = "Weight_21002") %>%
    select(ID, assessment_center, height, weight) %>%
    mutate(assessment_center = factor(assessment_center)) %>%
    filter(complete.cases(.)) %>%
    glimpse()

# QC data
inclusions = list()
inclusions[['T1']] = as.data.frame(fread("../../QC/inclusions_t1.txt"))
inclusions[['T2-FLAIR']] = as.data.frame(fread("../../QC/inclusions_flair.txt"))
inclusions[['DWI']] = as.data.frame(fread("../../QC/inclusions_dwi.txt"))

# Brain data
list_confound_vars = c(
        "24419", #Measure of head motion in T1 structural image
        "25756", #Scanner lateral (X) brain position
        "25757", #Scanner transverse (Y) brain position
        "25758", #Scanner longitudinal (Z) brain position
        "25759", #Scanner table position
        "25000" #Volumetric scaling from T1 head image to standard space
    )

df_confounds = df_brain %>% select(ID, all_of(list_confound_vars))
df_brain = df_brain %>% select(-all_of(list_confound_vars))

# Fields to visualize
field_ids = c(
    "25009",
    "25007",
    "25005",
    "25003",
    "25781"
)

fwrite(as.data.frame(field_ids), "results/markers_whole_brain.txt", col.names=FALSE)

results = list()
i = 1
# Run linear models
for (num_field in field_ids) {
    var_mapping = brain_mapping %>% filter(FieldID == num_field)
    
    if (var_mapping$modality %in% c("T1", "T2-FLAIR", "DWI")) { 
        inc_tmp = inclusions[[var_mapping$modality]] 
    } else { inc_tmp = inclusions[['T1']] }

    df_lm = df_brain %>% 
        select(ID, all_of(num_field)) %>% # Select brain var
        filter(ID %in% inc_tmp$V1) %>% # Remove QC exclusions
        left_join(df_gender, by="ID") %>%
        left_join(df_confounds, by="ID") %>%
        left_join(df_cov, by="ID") %>%
        rename(
            Brain = all_of(num_field),
            motion_t1 = "24419",
            scanner_x = "25756",
            scanner_y = "25757",
            scanner_z = "25758",
            scanner_pos = "25759",
            ICV = "25000"
            ) %>%
        filter(complete.cases(.))

    if (nrow(df_lm) < 5) { return("Fewer than 5 subjects with data") }
    if ( is.numeric(df_lm$Brain) == FALSE ) { return("Non-numeric brain variable") }

    # Run linear models
    lm_inter = summary(lmer(scale(Brain) ~ scale(Gender_score) * Sex + bs(Age, degree = 3, df = 4) + ICV + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), data = df_lm))
    lm_males = summary(lmer(scale(Brain) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + ICV + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), data = df_lm %>% filter(Sex == "Male")))
    lm_females = summary(lmer(scale(Brain) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + ICV + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), data = df_lm %>% filter(Sex == "Female")))

    # Store results
    results_i = data.frame(
            FieldID = var_mapping %>% pull(FieldID),
            Field_title = var_mapping %>% pull(ukb_field_title),
            fmrib_name = var_mapping %>% pull(fmrib_name),
            Category = var_mapping %>% pull(Category),
            Modality = var_mapping %>% pull(modality),
            n_females = nrow(df_lm %>% filter(Sex == "Female")),
            n_males = nrow(df_lm %>% filter(Sex == "Male")),
            n_all = nrow(df_lm),
            coef_males = lm_males$coefficients["scale(Gender_score)", "Estimate"],
            pval_males = lm_males$coefficients["scale(Gender_score)", "Pr(>|t|)"],
            coef_females = lm_females$coefficients["scale(Gender_score)", "Estimate"],
            pval_females = lm_females$coefficients["scale(Gender_score)", "Pr(>|t|)"],
            coef_inter = lm_inter$coefficients["scale(Gender_score):SexMale", "Estimate"],
            pval_inter = lm_inter$coefficients["scale(Gender_score):SexMale", "Pr(>|t|)"]
        )
    
    results[[i]] = results_i

    # Plot
    lm_plot = lmer(Brain ~ Gender_score * Sex + bs(Age, degree = 3, df = 4) + ICV + motion_t1 + scanner_x + scanner_y + scanner_z + scanner_pos + (1|assessment_center), data = df_lm)
    fit_data = as.data.frame(Effect(c("Gender_score", "Sex"), lm_plot, xlevels=100))

    g = rasterGrob(t(scale_gender_cont[seq(20,80)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

    lm_labels = c(
            Female=paste0(
                "Interaction: p = ",round(results_i$pval_inter,4),
                "\nFemale: std(B) = ", round(results_i$coef_females,4), "; p = ", round(results_i$pval_females,4)
                ),
            Male=paste0("Male: std(B) = ", round(results_i$coef_males,4), "; p = ", round(results_i$pval_males,4))
        )

    # Change mm3 to cm3
    fit_data = fit_data %>%
        mutate(
            fit = fit / 1000,
            se = se / 1000,
            lower = lower / 1000,
            upper = upper / 1000
        )

    ggplot(fit_data, aes(x=Gender_score, y=fit, color=Sex, fill=Sex)) +
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
        geom_line(linewidth=2) +
        geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2) +
        scale_x_continuous(name="IGL", breaks=c(0,0.5,1)) +
        scale_y_continuous(name="Value") +
        scale_fill_manual(values=scale_female_male_dis, guide="none") +
        scale_color_manual(values=scale_female_male_dis, labels=lm_labels) +
        ggtitle(paste0(var_mapping$FieldID, "\n", var_mapping$ukb_field_title)) +
        theme_light() + 
            theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5, size=10, face="bold"),
                legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size=10),
                strip.text = element_text(size = 15, color="black", face="bold"), strip.background = element_rect(fill="#ffffff")) +
        guides(color = guide_legend(ncol = 1, byrow = TRUE))
    ggsave(paste0("./visualization/tab_paper/",var_mapping$fmrib_name,".png"), width=4.2, height=5)
    print(paste0("./visualization/tab_paper/",var_mapping$fmrib_name,".png"))

    i = i + 1
}

results = do.call(rbind, results)

# FDR correction
results = results %>%
    mutate(
        pvalfdr_males = p.adjust(pval_males, method = "fdr"),
        pvalfdr_females = p.adjust(pval_females, method = "fdr"),
        pvalfdr_inter = p.adjust(pval_inter, method = "fdr")
    ) %>%
    mutate(
        sig_males = ifelse(pvalfdr_males < 0.05, 1, 0),
        sig_females = ifelse(pvalfdr_females < 0.05, 1, 0),
        sig_inter = ifelse(pvalfdr_inter < 0.05, 1, 0),
    ) %>%
    glimpse()

fwrite(results, "./results/tab_paper/results_clean.tsv", col.names = TRUE, row.names=FALSE, quote=FALSE, sep="\t")

#endregion

#region FreeSurfer CT/SA/GMV, Destrieux (a2009s) parcellation, white surface

# df = df_results
# type="Mean thickness"

map_destrieux = function(df, type) {

    df_tmp = df %>%
        filter(Category == 197) %>%
        left_join(brain_mapping %>% select(-c(fmrib_name, Category)), by="FieldID") %>%
        filter(str_starts(ukb_field_title, type)) %>%
        mutate(
            hemisphere = str_extract(fmrib_name, "^aparc-a2009s_([lr]h)_", group = 1),
            ukb_label = fmrib_name %>%
                str_remove("^aparc-a2009s_[lr]h_(volume|area|thickness)_") %>%
                str_replace_all("G\\+S-", "G_and_S_") %>%
                str_replace_all("(?<!\\+)G-", "G_") %>%
                str_replace_all("(?<!\\+)S-", "S_") %>%
                str_replace_all("-", "_") %>%
                str_replace_all("(?<!and)([a-z]{2,})_([A-Z])", "\\1-\\2"), 
            ukb_label = paste0(hemisphere, "_", ukb_label)
        )
    
    mapping_df = data.frame(
        ukb_label = df_tmp$ukb_label,
        desterieux_label = c(
            # Left hemisphere
            "lh_G_and_S_frontomargin", "lh_G_and_S_occipital_inf",
            "lh_G_and_S_paracentral", "lh_G_and_S_subcentral",
            "lh_G_and_S_transv_frontopol", "lh_G_and_S_cingul-Ant",
            "lh_G_and_S_cingul-Mid-Ant", "lh_G_and_S_cingul-Mid-Post",
            "lh_G_cingul-Post-dorsal", "lh_G_cingul-Post-ventral",
            "lh_G_cuneus", "lh_G_front_inf-Opercular",
            "lh_G_front_inf-Orbital", "lh_G_front_inf-Triangul",
            "lh_G_front_middle", "lh_G_front_sup",
            "lh_G_Ins_lg_and_S_cent_ins", "lh_G_insular_short",
            "lh_G_occipital_middle", "lh_G_occipital_sup",
            "lh_G_oc-temp_lat-fusifor", "lh_G_oc-temp_med-Lingual",
            "lh_G_oc-temp_med-Parahip", "lh_G_orbital",
            "lh_G_pariet_inf-Angular", "lh_G_pariet_inf-Supramar",
            "lh_G_parietal_sup", "lh_G_postcentral",
            "lh_G_precentral", "lh_G_precuneus",
            "lh_G_rectus", "lh_G_subcallosal",
            "lh_G_temp_sup-G_T_transv", "lh_G_temp_sup-Lateral",
            "lh_G_temp_sup-Plan_polar", "lh_G_temp_sup-Plan_tempo",
            "lh_G_temporal_inf", "lh_G_temporal_middle",
            "lh_Lat_Fis-ant-Horizont", "lh_Lat_Fis-ant-Vertical",
            "lh_Lat_Fis-post", "lh_Pole_occipital",
            "lh_Pole_temporal", "lh_S_calcarine",
            "lh_S_central", "lh_S_cingul-Marginalis",
            "lh_S_circular_insula_ant", "lh_S_circular_insula_inf",
            "lh_S_circular_insula_sup", "lh_S_collat_transv_ant",
            "lh_S_collat_transv_post", "lh_S_front_inf",
            "lh_S_front_middle", "lh_S_front_sup",
            "lh_S_interm_prim-Jensen", "lh_S_intrapariet_and_P_trans",
            "lh_S_oc_middle_and_Lunatus", "lh_S_oc_sup_and_transversal",
            "lh_S_occipital_ant", "lh_S_oc-temp_lat",
            "lh_S_oc-temp_med_and_Lingual", "lh_S_orbital_lateral",
            "lh_S_orbital_med-olfact", "lh_S_orbital-H_Shaped",
            "lh_S_parieto_occipital", "lh_S_pericallosal",
            "lh_S_postcentral", "lh_S_precentral-inf-part",
            "lh_S_precentral-sup-part", "lh_S_suborbital",
            "lh_S_subparietal", "lh_S_temporal_inf",
            "lh_S_temporal_sup", "lh_S_temporal_transverse",
            # Right hemisphere - mirror of left
            "rh_G_and_S_frontomargin", "rh_G_and_S_occipital_inf",
            "rh_G_and_S_paracentral", "rh_G_and_S_subcentral",
            "rh_G_and_S_transv_frontopol", "rh_G_and_S_cingul-Ant",
            "rh_G_and_S_cingul-Mid-Ant", "rh_G_and_S_cingul-Mid-Post",
            "rh_G_cingul-Post-dorsal", "rh_G_cingul-Post-ventral",
            "rh_G_cuneus", "rh_G_front_inf-Opercular",
            "rh_G_front_inf-Orbital", "rh_G_front_inf-Triangul",
            "rh_G_front_middle", "rh_G_front_sup",
            "rh_G_Ins_lg_and_S_cent_ins", "rh_G_insular_short",
            "rh_G_occipital_middle", "rh_G_occipital_sup",
            "rh_G_oc-temp_lat-fusifor", "rh_G_oc-temp_med-Lingual",
            "rh_G_oc-temp_med-Parahip", "rh_G_orbital",
            "rh_G_pariet_inf-Angular", "rh_G_pariet_inf-Supramar",
            "rh_G_parietal_sup", "rh_G_postcentral",
            "rh_G_precentral", "rh_G_precuneus",
            "rh_G_rectus", "rh_G_subcallosal",
            "rh_G_temp_sup-G_T_transv", "rh_G_temp_sup-Lateral",
            "rh_G_temp_sup-Plan_polar", "rh_G_temp_sup-Plan_tempo",
            "rh_G_temporal_inf", "rh_G_temporal_middle",
            "rh_Lat_Fis-ant-Horizont", "rh_Lat_Fis-ant-Vertical",
            "rh_Lat_Fis-post", "rh_Pole_occipital",
            "rh_Pole_temporal", "rh_S_calcarine",
            "rh_S_central", "rh_S_cingul-Marginalis",
            "rh_S_circular_insula_ant", "rh_S_circular_insula_inf",
            "rh_S_circular_insula_sup", "rh_S_collat_transv_ant",
            "rh_S_collat_transv_post", "rh_S_front_inf",
            "rh_S_front_middle", "rh_S_front_sup",
            "rh_S_interm_prim-Jensen", "rh_S_intrapariet_and_P_trans",
            "rh_S_oc_middle_and_Lunatus", "rh_S_oc_sup_and_transversal",
            "rh_S_occipital_ant", "rh_S_oc-temp_lat",
            "rh_S_oc-temp_med_and_Lingual", "rh_S_orbital_lateral",
            "rh_S_orbital_med-olfact", "rh_S_orbital-H_Shaped",
            "rh_S_parieto_occipital", "rh_S_pericallosal",
            "rh_S_postcentral", "rh_S_precentral-inf-part",
            "rh_S_precentral-sup-part", "rh_S_suborbital",
            "rh_S_subparietal", "rh_S_temporal_inf",
            "rh_S_temporal_sup", "rh_S_temporal_transverse"
        )
        )

    df_tmp = df_tmp %>%
        left_join(mapping_df, by="ukb_label") %>%
        select(-ukb_label) %>%
        rename(label = "desterieux_label") %>%
        mutate(
            pvalfdr_males = p.adjust(pval_males, method = "fdr"),
            pvalfdr_females = p.adjust(pval_females, method = "fdr"),
            pvalfdr_inter = p.adjust(pval_inter, method = "fdr")
        ) %>%
        mutate(
            sig_males = ifelse(pvalfdr_males < 0.05, 1, 0),
            sig_females = ifelse(pvalfdr_females < 0.05, 1, 0),
            sig_inter = ifelse(pvalfdr_inter < 0.05, 1, 0),
        ) %>%
        mutate(
            coef_sig_males = ifelse(pvalfdr_males < 0.05, coef_males, 0),
            coef_sig_females = ifelse(pvalfdr_females < 0.05, coef_females, 0),
            coef_sig_inter = ifelse(pvalfdr_inter < 0.05, coef_inter, 0)
        ) %>%
        pivot_longer(
            cols = matches("_(males|females|inter)$"),
            names_to = c("metric", "group"),
            names_pattern = "(.*)_(males|females|inter)",
            values_to = "value"
        ) %>%
        mutate(
            metric = factor(metric),
            group = factor(group, levels=c("females", "males", "inter"), labels=c("Females", "Males", "Interaction"))
        ) %>%
        pivot_wider(names_from = metric, values_from = value)

    setdiff(df_tmp$label, atlas_labels(desterieux))

    return(df_tmp)
}

# df = df_gmv
# out = "test"

plot_destrieux = function(df, out) {

    plt = ggplot(df %>% group_by(group) %>% mutate(sig = ifelse(sig == 1, 0.5, 0))) + 
        scale_fill_continuous(palette=scale_gender_cont, limits=c(-0.03, 0.03), oob = squish) +
        facet_wrap(~group) +
        # theme_light() +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5),
            strip.text.y=element_text(size=0),
            strip.text.x=element_text(size=15, hjust=0.48),
            panel.spacing = unit(0, "lines"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.position="none") +
        guides(fill="none", alpha="none", color="none")

    # All coefficients
    plt + 
        geom_brain(
            atlas=desterieux,
            position = position_brain(view ~ hemi),
            aes(fill = coef)
        )
    ggsave(paste0(out, "_all.png"), width=10, height=3)

    # Only significant coefficients
    plt + 
        geom_brain(
            atlas=desterieux,
            position = position_brain(view ~ hemi),
            aes(fill = coef_sig)
        )
    ggsave(paste0(out, "_sig.png"), width=10, height=3)

    # All but highlight significant
    plt + 
        geom_brain(
            atlas=desterieux,
            position = position_brain(view ~ hemi),
            aes(fill = coef, size=sig), color="black"
        ) + 
        scale_size_identity()
    ggsave(paste0(out, "_all_sig.png"), width=10, height=3)
}

# Process destrieux mapping
dir.create("./results/destrieux", showWarnings = FALSE)

df_ct = map_destrieux(df_results, "Mean thickness")
df_sa = map_destrieux(df_results, "Area")
df_gmv = map_destrieux(df_results, "Volume")

fwrite(df_ct, "./results/destrieux/ct_destrieux.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
fwrite(df_sa, "./results/destrieux/sa_destrieux.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
fwrite(df_gmv, "./results/destrieux/wmh_destrieux.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

fwrite(as.data.frame(unique(df_ct$FieldID)), "results/markers_ct.txt", col.names=FALSE)
fwrite(as.data.frame(unique(df_sa$FieldID)), "results/markers_sa.txt", col.names=FALSE)
fwrite(as.data.frame(unique(df_gmv$FieldID)), "results/markers_gmv.txt", col.names=FALSE)

# Plot
dir.create("./visualization/destrieux", showWarnings = FALSE)

plot_destrieux(df_ct, "./visualization/destrieux/ct")
plot_destrieux(df_sa, "./visualization/destrieux/sa")
plot_destrieux(df_gmv, "./visualization/destrieux/gmv")

#endregion

#region FreeSurfer subcortical volumes, ASEG

# df = df_results

map_aseg = function(df) {

    aseg_mapping <- list(
        "aseg_global_volume_Brain-Stem" = "Brain-Stem",
        "aseg_global_volume_CC-Anterior" = "CC_Anterior",
        "aseg_global_volume_CC-Central" = "CC_Central",
        "aseg_global_volume_CC-Mid-Anterior" = "CC_Mid_Anterior",
        "aseg_global_volume_CC-Mid-Posterior" = "CC_Mid_Posterior",
        "aseg_global_volume_CC-Posterior" = "CC_Posterior",
        "aseg_lh_volume_Thalamus-Proper" = "Left-Thalamus",
        "aseg_lh_volume_Hippocampus" = "Left-Hippocampus",
        "aseg_lh_volume_Putamen" = "Left-Putamen",
        "aseg_lh_volume_Amygdala" = "Left-Amygdala",
        "aseg_lh_volume_VentralDC" = "Left-VentralDC",
        "aseg_lh_volume_Pallidum" = "Left-Pallidum",
        "aseg_lh_volume_Caudate" = "Left-Caudate",
        "aseg_rh_volume_Thalamus-Proper" = "Right-Thalamus",
        "aseg_rh_volume_Putamen" = "Right-Putamen",
        "aseg_rh_volume_Amygdala" = "Right-Amygdala",
        "aseg_rh_volume_VentralDC" = "Right-VentralDC",
        "aseg_rh_volume_Hippocampus" = "Right-Hippocampus",
        "aseg_rh_volume_Pallidum" = "Right-Pallidum",
        "aseg_rh_volume_Caudate" = "Right-Caudate",
        "aseg_rh_volume_Cerebellum-Cortex" = "Right-Cerebellum-Cortex"
    )

    # Convert to data frame
    aseg_mapping = data.frame(
        fmrib_name = names(aseg_mapping),
        label = unname(unlist(aseg_mapping)),
        row.names = NULL,
        stringsAsFactors = FALSE
    )

    df_tmp = df %>%
        filter(Category == 190) %>%
        left_join(brain_mapping %>% select(-c(fmrib_name, Category)), by="FieldID") %>%
        filter(str_starts(ukb_field_title, "Volume")) %>%
        left_join(aseg_mapping, by="fmrib_name") %>%
        filter(is.na(label) == FALSE) %>%
        mutate(
            pvalfdr_males = p.adjust(pval_males, method = "fdr"),
            pvalfdr_females = p.adjust(pval_females, method = "fdr"),
            pvalfdr_inter = p.adjust(pval_inter, method = "fdr")
        ) %>%
        mutate(
            sig_males = ifelse(pvalfdr_males < 0.05, 1, 0),
            sig_females = ifelse(pvalfdr_females < 0.05, 1, 0),
            sig_inter = ifelse(pvalfdr_inter < 0.05, 1, 0),
        ) %>%
        mutate(
            coef_sig_males = ifelse(pvalfdr_males < 0.05, coef_males, 0),
            coef_sig_females = ifelse(pvalfdr_females < 0.05, coef_females, 0),
            coef_sig_inter = ifelse(pvalfdr_inter < 0.05, coef_inter, 0)
        ) %>%
        pivot_longer(
            cols = matches("_(males|females|inter)$"),
            names_to = c("metric", "group"),
            names_pattern = "(.*)_(males|females|inter)",
            values_to = "value"
        ) %>%
        mutate(
            metric = factor(metric),
            group = factor(group, levels=c("females", "males", "inter"), labels=c("Females", "Males", "Interaction"))
        ) %>%
        pivot_wider(names_from = metric, values_from = value)        

    return(df_tmp)
}

# df = df_vol
# out = "test"

plot_aseg = function(df, out) {

    plt = ggplot(df %>% group_by(group) %>% mutate(sig = ifelse(sig == 1, 0.5, 0))) + 
        scale_fill_continuous(palette=scale_gender_cont, limits=c(-0.03, 0.03), oob = squish) +
        facet_wrap(~group) +
        # theme_light() +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5),
            strip.text.y=element_text(size=0),
            strip.text.x=element_text(size=15, hjust=0.48),
            panel.spacing = unit(0, "lines"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            legend.position="none") +
        guides(fill="none", alpha="none", color="none")

    # All coefficients
    plt +
        geom_brain(
            atlas=aseg(),
            view=c("sagittal","coronal_1"),
            aes(fill = coef)
        )
    ggsave(paste0(out, "_all.png"), width=10, height=2)

    # Only significant coefficients
    plt +
        geom_brain(
            atlas=aseg(),
            view=c("sagittal","coronal_1"),
            aes(fill = coef_sig)
        )
    ggsave(paste0(out, "_sig.png"), width=10, height=2)

    # All but highlight significant
    plt +
        geom_brain(
            atlas=aseg(),
            view=c("sagittal","coronal_1"),
            aes(fill = coef, size=sig), color="black"
        ) +
        scale_size_identity()
    ggsave(paste0(out, "_all_sig.png"), width=10, height=2)

}

# Process aseg mapping
dir.create("./results/aseg", showWarnings = FALSE)

df_vol = map_aseg(df_results)

fwrite(df_vol, "./results/aseg/aseg.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

fwrite(as.data.frame(unique(df_vol$FieldID)), "results/markers_sub_vol.txt", col.names=FALSE)

# Plot
dir.create("./visualization/aseg", showWarnings = FALSE)

plot_aseg(df_vol, "./visualization/aseg/vol")

#endregion
