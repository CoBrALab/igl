
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(grid)
library(patchwork)
library(plotly)
library(htmlwidgets)
library(pandoc)
library(ggrepel)
library(stringr)
library(svglite)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

atc_codes = as.data.frame(fread("../../../UKB/Analyses/clean_pharma/medication_atc_categories_clean.csv"))

atc_codes = atc_codes %>%
    mutate(
        Name_1 = tolower(Name_1),
        Name_2 = tolower(Name_2),
        Name_3 = tolower(Name_3)
    ) %>%
    mutate(
        Name_1_clean = gsub("[(),-/.]", "", Name_1),
        Name_2_clean = gsub("[(),-/.]", "", Name_2),
        Name_3_clean = gsub("[(),-/.]", "", Name_3)
    ) %>%
    glimpse()

results_other = as.data.frame(fread("./results/results_other.tsv"))

exclude_other_vars = c("medication_female_6153_cholesterol", "medication_female_6153_blood_pressure", "medication_female_6153_insulin", "medication_female_6153_none",
    "medication_male_6177_cholesterol", "medication_male_6177_blood_pressure", "medication_male_6177_insulin", "medication_male_6177_none"
)

results_other = results_other %>%
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    filter(med_name != "none") %>%
    filter(!var_name %in% exclude_other_vars) %>%
    rename(
        ATC_name = "med_name"
    ) %>%
    mutate(
        ATC_level = NA,
        ATC_code = ATC_name,
        Name_1 = "Non ATC"
    ) %>%
    select(-var_name) %>%
    select(ATC_level, ATC_code, ATC_name, everything()) %>%
    glimpse()

fwrite(as.data.frame(unique(results_other$ATC_name)), "results/markers_pharma_other.txt", col.names=FALSE)

results = as.data.frame(fread("./results/results_glm.tsv"))

results_lvl3 = results %>%
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    filter(ATC_level == 3 & ATC_code != "none") %>%
    left_join(
        atc_codes %>% select(Name_1, Level_3) %>% rename(ATC_code = "Level_3"),
        by="ATC_code"
    ) %>%
    bind_rows(results_other) %>%
    mutate(
        ATC_code = factor(ATC_code),
        Name_1 = factor(Name_1, levels=c(
            "Non ATC",
            "antiinfectives for systemic use",
            "alimentary tract and metabolism",
            "blood and blood forming organs",
            "various",
            "cardiovascular system",
            "dermatologicals",
            "genito urinary system and sex hormones",
            "systemic hormonal preparations, excl. sex hormones and insulins",
            "antineoplastic and immunomodulating agents",
            "musculo-skeletal system",
            "nervous system",
            "antiparasitic products, insecticides and repellents",
            "respiratory system",
            "sensory organs"            
        ), labels=c(
            "Non ATC",
            "Anti-infectives",
            "Alimentary/metabolism",
            "Blood",
            "Various",
            "Cardiovascular",
            "Dermatologicals",
            "Genito urinary",
            "Hormonal preparations",
            "Antineoplastic/immunomodulating agents",
            "Musculo-skeletal",
            "Nervous",
            "Antiparasitic/insecticides",
            "Respiratory",
            "Sensory organs"
        ))
    ) %>%
    rename(
        ndx_males = n_case_male,
        ndx_females = n_case_female,
        nhc_males = n_control_male,
        nhc_females = n_control_female
    ) %>%
    select(-c(or_males_lower, or_males_upper, or_females_lower, or_females_upper)) %>%
    pivot_longer(cols=-c(ATC_level, ATC_code, ATC_name, Name_1), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>%
    mutate(Stat = factor(Stat), Type = factor(Type)) %>%
    pivot_wider(names_from=Stat, values_from=Value) %>%
    filter(is.na(coef) == FALSE) %>%
    mutate(
        labels = paste0(ATC_code, "\n", ATC_name, "\n", Name_1),
        or_direction = factor(ifelse(or<1, "Female", "Male"), levels=c("Female", "Male")),
        or = ifelse(or < 1, 1/or, or),
        # Calculate the percent of risk increase with direction
        perc_odds_increase = ifelse(or_direction=="Female", (or-1)*-100, (or-1)*100)
    ) %>%
    glimpse()

fwrite(results_lvl3 %>% select(-labels), "./results/results_clean.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

fwrite(as.data.frame(unique(results_lvl3$ATC_name)), "results/markers_pharma_atc.txt", col.names=FALSE)

#endregion

#region Manhattan plots

labels_to_viz = data.frame(
    ATC_name = c(
        # Non ATC
        "multivitamins",
        "oil",
        "zinc",
        "calcium",
        "vitaminD",
        "insulin",
        "aspirin",
        # Alimentary/metabolism
        "drugs for constipation",
        # "calcium",
        "multivitamins, plain",
        "insulins and analogues",
        # Blood
        "antithrombotic agents",
        # Cardiovascular
        "angiotensin ii receptor blockers (arbs), plain",
        # Dermatological
        "other dermatological preparations",
        # Genito urinary
        "drugs used in benign prostatic hypertrophy",
        "progestogens and estrogens in combination",
        # Hormonal preparations
        "thyroid preparations",
        # Anti-infectives
        # Antineoplastic
        # Musculo-skeletal
        "drugs affecting bone structure and mineralization",
        # Nervous
        "antidepressants",
        "antiepileptics",
        "opioids",
        # Antiparasitic
        # Respiratory
        "decongestants and other nasal preparations for topical use",
        # Sensory organs
        "other ophthalmologicals"
        # Various
    ), clean_names = c(
        # Non ATC
        "Multivitamins",
        "Oil",
        "Zinc",
        "Calcium",
        "VitaminD",
        "Insulin",
        "Aspirin",
        # Alimentary/metabolism
        "Drugs for constipation",
        "Calcium",
        # "Multivitamins",
        "Insulins and analogues",
        # Blood
        "Antithrombotic agents",
        # Cardiovascular
        "Angiotensin ii receptor blockers (arbs)",
        # Dermatological
        "Other dermatological preparations",
        # Genito urinary
        "Benign prostatic hypertrophy drugs",
        "Progestogens and estrogens",
        # Hormonal preparations
        "Thyroid preparations",
        # Anti-infectives
        # Antineoplastic
        # Musculo-skeletal
        "Drugs affecting bones and mineralization",
        # Nervous
        "Antidepressants",
        "Antiepileptics",
        "Opioids",
        # Antiparasitic
        # Respiratory
        "Decongestants",
        # Sensory organs
        "Other ophthalmologicals"
        # Various
    )
)

thresh_list = c(0.05, 0.01, 0.001)
thresh_list_names = c("05", "01", "001")
correction_type = c("fdr", "bonferroni")

for (t in 1:length(thresh_list)) {
    for (c in 1:length(correction_type)) {

        # Manipulate p-values
        results_tmp = results_lvl3 %>%
            # Keep only diagnoses with >10 ppl in case group (separated by sex)
            filter(ndx > 10) %>%
            # P-value correction
            mutate(
                pval_cor = p.adjust(pval, method=correction_type[c]),
                sig_cor = factor(if_else(pval_cor < thresh_list[t], 1, 0), levels=c(0,1)),
            ) %>%
            # -log10 * direction
            mutate(
                pval_log = if_else(coef < 0, -log(pval,10)*-1, -log(pval,10)),
                pval_cor_log = if_else(coef < 0, -log(pval_cor,10)*-1, -log(pval_cor,10)),
            ) %>%
            # Assign color
            mutate(
                pval_cor_color = ifelse(sig_cor == 0, "#e0e2e2", ifelse(Type == "males", scale_female_male_dis['Male'], scale_female_male_dis['Female']))
            ) %>%
            left_join(labels_to_viz, by="ATC_name") %>%
            # Keep only labels to annotate
            mutate(
                labels = stringr::str_wrap(clean_names ,30)
            )

        fwrite(results_tmp %>% select(-labels), paste0("./results/results_clean_",correction_type[c],thresh_list_names[t],".tsv"), row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

        # Plot
        yaxis_max = max(abs(results_tmp$pval_log))
        yaxis_lim = 20

        g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

        plt = ggplot(results_tmp, aes(x=Name_1, y=pval_log, color=factor(pval_cor_color), text=labels)) + 
            annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
            geom_vline(xintercept=seq(0,length(levels(results_lvl3$Name_1)))+0.5 ,color="black", alpha=0.2) +
            geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 0), height=0, alpha=0.5, size=2) + 
            geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 1), height=0, alpha=0.5, size=2) + 
            geom_hline(yintercept=0, size=0.3) +
            geom_label_repel(aes(label=labels), seed = 123, size=5, box.padding=0.1, label.padding=0.2, force=3, max.overlaps=1000, direction="y", label.r=0, nudge_x = -1.2, min.segment.length=0, force_pull=0.5) +
            geom_point(data = results_tmp %>% filter(is.na(labels) == FALSE), alpha=0.5, size=2) + 
            scale_x_discrete(name = "") + 
            scale_color_identity() +
            labs(title = NULL) +
            theme_classic() + 
            theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
                legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
                strip.text = element_text(size = 20, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
                axis.text.x = element_text(angle = 30, hjust=1))

        # Max pval
        plt + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)
        ggsave(paste0("./visualization/miami_", correction_type[c], thresh_list_names[t], "_max.png"), width=20, height=10)
        print(paste0("./visualization/miami_", correction_type[c], thresh_list_names[t], "_max.png"))

        # Pval capped at 20
        plt = plt + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_lim,yaxis_lim), oob=scales::squish)
        ggsave(paste0("./visualization/miami_", correction_type[c], thresh_list_names[t], "_lim.png"), width=20, height=10)
        print(paste0("./visualization/miami_", correction_type[c], thresh_list_names[t], "_lim.png"))

        # Plot % of significant dx by chapter/sex/IGL direction
        chapter_totals = results_tmp %>%
            distinct(Name_1, ATC_code) %>%
            count(Name_1, name = "n_total") %>%
            crossing(
                Type = c("females", "males"),
                direction = c("positive", "negative")
            )

        results_perc = results_tmp %>%
            mutate(
                direction = if_else(coef > 0, "positive", "negative"),
                sig = pval_cor < 0.05
            ) %>%
            filter(sig) %>%
            group_by(Name_1, Type, direction) %>%
            summarise(n_sig = n(), .groups = "drop") %>%
            right_join(chapter_totals, by = c("Name_1", "Type", "direction")) %>%
            mutate(
                n_sig = ifelse(is.na(n_sig), 0.001, n_sig),
                perc_sig = n_sig / n_total,
                perc_sig_dir = ifelse(direction == "negative", perc_sig*-1, perc_sig)
            ) %>%
            mutate(
                Type = factor(Type, levels=c("females", "males"), labels=c("Female", "Male"))
            ) %>%
            glimpse()

        g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

        plt2 = ggplot(results_perc, aes(x=Name_1, y=perc_sig_dir, fill=Type)) +
            annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-1, ymax=1) + 
            geom_col(position=position_dodge()) + 
            geom_hline(yintercept = 0, linewidth=1) + 
            geom_vline(xintercept=seq(0,length(levels(results_perc$Name_1)))+0.5 ,color="black", alpha=0.2) +
            geom_text(
                aes(label = sprintf("%.0f%%", abs(perc_sig_dir) * 100),vjust = ifelse(perc_sig_dir > 0, -0.3, 1.3)),
                position = position_dodge(width = 0.9), size = 5
            ) +
            scale_fill_manual(values=scale_female_male_dis) +
            scale_x_discrete(name="", drop = FALSE, expand = expansion(add = c(1.2, 0.6))) +
            scale_y_continuous(
                name="% of diagnoses", breaks=c(-0.75, -0.5, -0.25, 0, 0.25), labels=c("75%", "50%", "25%", "0%", "25%"),
                expand = expansion(add = c(0.1, 0.1))
            ) +
            theme_classic() + 
            theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
                legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
                strip.text = element_text(size = 20, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
                axis.text.x = element_text(angle = 30, hjust=1))

        plt = plt + 
            scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)

        plt2 = plt2 + 
            theme(
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.title.x = element_blank()
            )

        plt2 / plt  + plot_layout(heights = c(1, 2.5))
        ggsave(paste0("./visualization/miami_",correction_type[c],thresh_list_names[t],"_max_perc.png"), width=20, height=12)
        print(paste0("./visualization/miami_",correction_type[c],thresh_list_names[t],"_max_perc.png"))

    }
}

#endregion
