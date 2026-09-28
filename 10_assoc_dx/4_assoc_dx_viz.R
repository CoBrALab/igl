
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

labels_to_viz = data.frame(
    diagnosis = c(
        # Perinatal
        # Eye
        # Mental
        "mental and behavioural disorders due to use of tobacco",
        "mental and behavioural disorders due to use of alcohol",
        "depressive episode",
        "other anxiety disorders",
        "schizophrenia",
        "bipolar affective disorder",
        # Infectious/parasitic
        # Endocrine/metabolic
        "unspecified diabetes mellitus",
        "noninsulindependent diabetes mellitus",
        "obesity",
        # Pregnancy
        "perineal laceration during delivery",
        "single spontaneous delivery",
        "labour and delivery complicated by foetal stress distress",
        # Circulatory
        "essential primary hypertension",
        # Genitourinary
        "hyperplasia of prostate",
        "female genital prolapse",
        # Musculoskeletal
        "gout",
        "osteoporosis without pathological fracture",
        "other arthrosis",        
        # Congenital malformations
        "irritable bowel syndrome",
        # Digestive
        # Ear/mastoid
        # Nervous
        "migraine",
        "sleep disorders",
        # Respiratory
        "other chronic obstructive pulmonary disease",
        # Skin
        "skin changes due to chronic exposure to nonionising radiation"
        # Blood
    ), clean_names = c(
        # Perinatal
        # Eye
        # Mental
        "Tobacco use disorder",
        "Alcohol use disorder",
        "Depressive episode",
        "Other anxiety disorders",
        "Schizophrenia",
        "Bipolar affective disorder",
        # Infectious/parasitic
        # Endocrine/metabolic
        "Unspecified diabetes",
        "Non-insulin-dependent diabetes",
        "Obesity",
        # Pregnancy
        "Perineal laceration during delivery",
        "Single spontaneous delivery",
        "Foetal stress distress",
        # Circulatory
        "Essential primary hypertension",
        # Genitourinary
        "Hyperplasia of prostate",
        "Female genital prolapse",
        # Musculoskeletal
        "Gout",
        "Osteoporosis w/o fracture",
        "Other arthrosis",
        # Congenital malformations
        "Irritable bowel syndrome",
        # Digestive
        # Ear/mastoid
        # Nervous
        "Migraine",
        "Sleep disorders",
        # Respiratory
        "Other COPD",
        # Skin
        "Non-ionizing radiation skin changes"
        # Blood
    )
)

#region Load data

results_glm = as.data.frame(fread("./results/results_dx_glm.tsv"))

results_glm = results_glm %>%
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    mutate(
        icd_code = factor(icd_code),
        categ = factor(categ),
        chapter = factor(chapter,
            levels=c(
                "Certain conditions originating in the perinatal period",
                "Diseases of the eye and adnexa",
                "Mental, Behavioral and Neurodevelopmental disorders",
                "Congenital malformations, deformations and chromosomal abnormalities",
                "Certain infectious and parasitic diseases",
                "Endocrine, nutritional and metabolic diseases",
                "Pregnancy, childbirth and the puerperium",
                "Diseases of the circulatory system",
                "Diseases of the genitourinary system",
                "Diseases of the digestive system",
                "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
                "Diseases of the musculoskeletal system and connective tissue",
                "Diseases of the ear and mastoid process",
                "Diseases of the nervous system",
                "Diseases of the respiratory system",
                "Diseases of the skin and subcutaneous tissue"
            ),
            labels=c(
                "Perinatal",
                "Eye",
                "Mental",
                "Congenital malformations",
                "Infectious/parasitic",
                "Endocrine/metabolic",
                "Pregnancy",
                "Circulatory",
                "Genitourinary",
                "Digestive",
                "Blood",
                "Musculoskeletal",
                "Ear/mastoid",
                "Nervous",
                "Respiratory",
                "Skin"
            )
        )
    ) %>%
    rename(
        ndx_males = n_dx_males,
        ndx_females = n_dx_females,
        nhc_males = n_hc_males,
        nhc_females = n_hc_females
    ) %>% 
    select(-c(or_males_lower, or_males_upper, or_females_lower, or_females_upper)) %>%
    pivot_longer(cols=-c(icd_code, chapter, categ, diagnosis), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>%
    mutate(Stat = factor(Stat), Type = factor(Type)) %>%
    pivot_wider(names_from=Stat, values_from=Value) %>%
    filter(is.na(coef) == FALSE) %>%
    mutate(
        labels = paste0(icd_code, "\n", diagnosis, "\n", categ),
        or_direction = factor(ifelse(or<1, "Female", "Male"), levels=c("Female", "Male")),
        or = ifelse(or < 1, 1/or, or),
        # Calculate the percent of risk increase with direction
        perc_odds_increase = ifelse(or_direction=="Female", (or-1)*-100, (or-1)*100)
    ) %>%
    glimpse()

#endregion

#region Individual plots for paper

dir.create("visualization/indiv_paper", showWarnings = FALSE)

# Load
df_raw = as.data.frame(fread("../impute_input/results/df_imputed_tp_all_combined.tsv"))
df_raw = df_raw %>%
    filter(instanceID == 0) %>%
    select(ID, Date_of_attending_assessment_centre_53_0) %>%
    rename(Date_assessment_centre = "Date_of_attending_assessment_centre_53_0") %>%
    glimpse()

firstocc = as.data.frame(fread("../../../UKB/Analyses/clean_firstocc/results/firstocc_ses0_final.tsv"))
firstocc = firstocc %>%
    mutate(
        DX_date = as.Date(DX_date),
        chapter = factor(chapter),
        categ = factor(categ)
    ) %>%
    select(-c(Sex, Year_birth, Month_birth, date_birth, age_at_dx)) %>%
    glimpse()

df_gender = as.data.frame(fread("../gender_score_new/results/all_tp0/gender_score_combined.tsv"))
df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    left_join(df_raw, by="ID") %>%
    glimpse()

var_df = as.data.frame(fread("../gender_score_new/variable_mapping.tsv"))

# Run for a few dx
dx_list = c(
    "F20", "F31", "F32", "F41",
    "E11", "E66",
    "O70",
    "I10",
    "N40", "N81",
    "M10", "M19", "M81",
    "K58",
    "G43", "G47",
    "J44",
    "L57"
)

for (d in 1:length(dx_list)) {

    # Clean dataframes
    print(d)

    dx_icd = dx_list[d]
    dx_name = unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(dx_name))
    dx_chapter = as.character(unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(chapter)))
    dx_categ = as.character(unique(firstocc %>% filter(icd_code == dx_icd) %>% pull(categ)))
    dx_n = sum((df_gender$ID %in% (firstocc %>% filter(icd_code == dx_icd) %>% pull(ID))) == TRUE)
    
    print(dx_name)

    df_dx = firstocc %>%
        filter(icd_code == dx_icd) %>%
        left_join(df_gender, by="ID") %>%
        mutate(days_gender_dx = as.numeric(difftime(as.Date(DX_date), as.Date(Date_assessment_centre), units = "days"))) %>% 
        filter(complete.cases(.)) %>%
        mutate(Group = "DX") %>% 
        select(ID, Date_assessment_centre, Sex, Age, Gender_score, Group)
    
    ids_not_controls = unique(firstocc %>% filter(chapter == dx_chapter) %>% pull(ID))

    df_hc = df_gender %>%
        filter(!ID %in% ids_not_controls) %>%
        mutate(Group = "HC") %>%
        select(ID, Date_assessment_centre, Sex, Age, Gender_score, Group)

    df_all = rbind(df_dx, df_hc)
    df_all$Group = factor(df_all$Group, levels=c("HC", "DX"))

    # Female
    if (length(df_dx %>% filter(Sex == "Female") %>% pull(ID)) > 11) {
        lm_females = glm(Group ~ Gender_score + Age, data = df_all %>% filter(Sex == "Female"), family = binomial)
        lm_females_coef = summary(lm_females)$coefficients
        lm_females_or = as.data.frame(exp(cbind(OR = coef(lm_females), confint(lm_females))))

    } else {
        lm_females_coef = matrix(data=NA, nrow=3, ncol=4)
        lm_females_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
        colnames(lm_females_or) = c("OR", "2.5 %", "97.5 %")
    }

    # Male
    if (length(df_dx %>% filter(Sex == "Male") %>% pull(ID)) > 11) {
        lm_males = glm(Group ~ Gender_score + Age, data = df_all %>% filter(Sex == "Male"), family = binomial)
        lm_males_coef = summary(lm_males)$coef
        lm_males_or = as.data.frame(exp(cbind(OR = coef(lm_males), confint(lm_males))))

    } else {
        lm_males_coef = matrix(data=NA, nrow=3, ncol=4)
        lm_males_or = as.data.frame(matrix(data=NA, nrow=3, ncol=3))
        colnames(lm_males_or) = c("OR", "2.5 %", "97.5 %")
    }

    # Save results
    results_i = data.frame(
        icd_code = dx_icd,
        diagnosis = dx_name,
        chapter = dx_chapter,
        categ = dx_categ,
        n_dx_males = nrow(df_all %>% filter(Sex == "Male" & Group == "DX")),
        n_dx_females = nrow(df_all %>% filter(Sex == "Female" & Group == "DX")),
        n_hc_males = nrow(df_all %>% filter(Sex == "Male" & Group == "HC")),
        n_hc_females = nrow(df_all %>% filter(Sex == "Female" & Group == "HC")),
        coef_males = lm_males_coef[2,1],
        pval_males = lm_males_coef[2,4],
        or_males = lm_males_or[2,1],
        or_males_lower = lm_males_or[2,2],
        or_males_upper = lm_males_or[2,3],
        coef_females = lm_females_coef[2,1],
        pval_females = lm_females_coef[2,4],
        or_females = lm_females_or[2,1],
        or_females_lower = lm_females_or[2,2],
        or_females_upper = lm_females_or[2,3]
    )

    # Plot ORs
    df_plot = rbind(
        lm_males_or[2,] %>% mutate(Sex="Male"),
        lm_females_or[2,] %>% mutate(Sex="Female"))

    df_plot = df_plot %>%
        rename(lower = "2.5 %", upper="97.5 %") %>%
        mutate(Sex = factor(Sex, levels=c("Male", "Female")))

    if (sum(is.na(df_plot$OR))<2) {

        # Risk when lifestyle is completly masculine (gender_score = 1)
        lm_labels = c(
                Female=paste0("Female: OR = ", round(results_i$or_females,3), " (", round(results_i$or_females_lower,3), " - ", round(results_i$or_females_upper,3), ")"),
                Male=paste0("Male:     OR = ", round(results_i$or_males,3), " (", round(results_i$or_males_lower,3), " - ", round(results_i$or_males_upper,3), ")")
            )
        
        # Calculate plot limits
        max_ratio = max(max(df_plot$upper, 1 / df_plot$lower, na.rm=TRUE),2, na.rm = TRUE)
        y_min = 1 / max_ratio
        y_max = max_ratio

        # Make background gradient transformed in log-space
        log_vals = seq(log10(y_min), log10(y_max), length.out = 200)
        or_vals = 10^log_vals  # convert back to OR scale
        scale_gender_cont_tmp = scale_gender_cont[seq(20,80)]
        color_index = scales::rescale(log_vals, to = c(1, length(scale_gender_cont_tmp)) )
        g = rasterGrob(t(scale_gender_cont_tmp[round(color_index)]), width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

        plt1 = ggplot(df_plot, aes(x=Sex, y=OR, color=Sex)) + 
            annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=y_min * (1-0.12), ymax=y_max * ((1+0.12))) + 
            geom_point(size=4) + 
            geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4) + 
            geom_hline(yintercept = 1) + 
            scale_y_log10(name="log(Odds Ratio)", limits = c(y_min, y_max), expand = expansion(mult=0.03), breaks=c(0.25, 0.5, 1, 2, 4)) +
            scale_x_discrete(name = "") +
            scale_color_manual(values=scale_female_male_dis, labels=lm_labels) +
            coord_flip() +
            ggtitle(paste0("Risk when gender is most masculine\n", dx_icd, ";\n", dx_name, "\nn = (M",results_i$n_dx_males,"; F",results_i$n_dx_females, ")")) +
            theme_light() + 
            theme(
                text = element_text(size=15),
                plot.title = element_text(hjust = 0.5, size=10, face="bold"),
                legend.title = element_blank(),
                legend.position="bottom",
                legend.text=element_text(size=10),
                strip.text = element_text(size = 10, color="black", face="bold"),
                strip.background = element_rect(fill="#ffffff"),
                axis.text.y  = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank()
                ) + 
            guides(color = guide_legend(ncol = 1, byrow = TRUE))
        ggsave(paste0("./visualization/indiv_paper/", dx_icd, "_", gsub(" ", "_", dx_name), "_masculine.png"), width=3, height=3)
        print(paste0("./visualization/indiv_paper/", dx_icd, "_", gsub(" ", "_", dx_name), "_masculine.png"))

        # Risk when lifestyle is completly feminine (gender_score = 0)
        lm_labels = c(
                Female=paste0("Female: OR = ", round(1/results_i$or_females,3), " (", round(1/results_i$or_females_lower,3), " - ", round(1/results_i$or_females_upper,3), ")"),
                Male=paste0("Male:     OR = ", round(1/results_i$or_males,3), " (", round(1/results_i$or_males_lower,3), " - ", round(1/results_i$or_males_upper,3), ")")
            )
        
        # Calculate plot limits
        max_ratio = max(max(df_plot$upper, 1 / df_plot$lower, na.rm=TRUE),2, na.rm = TRUE)
        y_min = 1 / max_ratio
        y_max = max_ratio

        # Make background gradient transformed in log-space
        log_vals = seq(log10(y_min), log10(y_max), length.out = 200)
        or_vals = 10^log_vals  # convert back to OR scale
        scale_gender_cont_tmp = scale_gender_cont[seq(80,20)]
        color_index = scales::rescale(log_vals, to = c(1, length(scale_gender_cont_tmp)) )
        g = rasterGrob(t(scale_gender_cont_tmp[round(color_index)]), width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

        plt2 = ggplot(df_plot, aes(x=Sex, y=1/OR, color=Sex)) + 
            annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=y_min * (1-0.12), ymax=y_max * ((1+0.12))) + 
            geom_point(size=4) + 
            geom_errorbar(aes(ymin = 1/lower, ymax = 1/upper), width = 0.4) + 
            geom_hline(yintercept = 1) + 
            scale_y_log10(name="log(Odds Ratio)", limits = c(y_min, y_max), expand = expansion(mult=0.03), breaks=c(0.25, 0.5, 1, 2, 4)) +
            scale_x_discrete(name = "") +
            scale_color_manual(values=scale_female_male_dis, labels=lm_labels) +
            coord_flip() +
            ggtitle(paste0("Risk when gender is most feminine\n", dx_icd, ";\n", dx_name, "\nn = (M",results_i$n_dx_males,"; F",results_i$n_dx_females, ")")) +
            theme_light() + 
            theme(
                text = element_text(size=15),
                plot.title = element_text(hjust = 0.5, size=10, face="bold"),
                legend.title = element_blank(),
                legend.position="bottom",
                legend.text=element_text(size=10),
                strip.text = element_text(size = 10, color="black", face="bold"),
                strip.background = element_rect(fill="#ffffff"),
                axis.text.y  = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank()
                ) + 
            guides(color = guide_legend(ncol = 1, byrow = TRUE))
        ggsave(paste0("./visualization/indiv_paper/", dx_icd, "_", gsub(" ", "_", dx_name), "_feminine.png"), width=3, height=3)
        print(paste0("./visualization/indiv_paper/", dx_icd, "_", gsub(" ", "_", dx_name), "_feminine.png"))
    
    }
}

#endregion

#region Manhattan plots

# Manipulate p-values
results_tmp = results_glm %>%
    # Keep only diagnoses with >20 ppl in case group (separated by sex)
    filter(ndx >= 20) %>%
    # P-value correction
    mutate(
        pval_cor = p.adjust(pval, method="fdr"),
        sig_cor = factor(if_else(pval_cor < 0.05, 1, 0), levels=c(0,1)),
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
    left_join(labels_to_viz, by="diagnosis") %>%
    # Keep only labels to annotate
    mutate(
        labels = stringr::str_wrap(clean_names ,30)
    ) %>%
    glimpse()

fwrite(results_tmp %>% select(-labels), "./results/results_dx_glm_clean.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

fwrite(as.data.frame(unique(results_tmp$icd_code)), "results/markers_dx.txt", col.names=FALSE)

# Plot p-values
yaxis_max = max(abs(results_tmp$pval_log))
yaxis_max_all = yaxis_max
yaxis_lim = 20

g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

plt = ggplot(results_tmp, aes(x=chapter, y=pval_log, color=factor(pval_cor_color), text=labels)) + 
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    geom_vline(xintercept=seq(0,length(levels(results_glm$chapter)))+0.5 ,color="black", alpha=0.2) +
    geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 0), height=0, alpha=0.5) + 
    geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 1), height=0, alpha=0.5) + 
    geom_hline(yintercept = 0, linewidth=0.5) +
    geom_label_repel(aes(label=labels), seed = 123, size=5, box.padding=0.1, label.padding=0.2, force=3, max.overlaps=20, direction="y", label.r=0, nudge_x = -1.2, min.segment.length=0, force_pull=0.5) +
    geom_point(data = results_tmp %>% filter(is.na(labels) == FALSE), alpha=0.5) + 
    scale_x_discrete(name = "", expand = expansion(add = c(0, 0.6))) + 
    scale_color_identity() +
    labs(title = NULL) +
    theme_classic() + 
    theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
        strip.text = element_text(size = 20, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
        axis.text.x = element_text(angle = 30, hjust=1))

# Max pval
plt + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)
ggsave(paste0("./visualization/miami_fdr05_max.png"), width=20, height=10)
print(paste0("./visualization/miami_fdr05_max.png"))

# Pval capped at 20
plt + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_lim,yaxis_lim), oob=scales::squish)
ggsave(paste0("./visualization/miami_fdr05_lim.png"), width=20, height=10)
print(paste0("./visualization/miami_fdr05_lim.png"))

# Plot % of significant dx by chapter/sex/IGL direction
chapter_totals = results_tmp %>%
    distinct(chapter, icd_code) %>%
    count(chapter, name = "n_total") %>%
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
    group_by(chapter, Type, direction) %>%
    summarise(n_sig = n(), .groups = "drop") %>%
    right_join(chapter_totals, by = c("chapter", "Type", "direction")) %>%
    mutate(
        n_sig = ifelse(is.na(n_sig), 0.001, n_sig),
        perc_sig = n_sig / n_total,
        perc_sig_dir = ifelse(direction == "negative", perc_sig*-1, perc_sig)
    ) %>%
    mutate(
        Type = factor(Type, levels=c("females", "males"), labels=c("Female", "Male"))
    ) %>%
    mutate(chapter = factor(chapter,
        levels=c(
            "Perinatal",
            "Eye",
            "Mental",
            "Congenital malformations",
            "Infectious/parasitic",
            "Endocrine/metabolic",
            "Pregnancy",
            "Circulatory",
            "Genitourinary",
            "Digestive",
            "Blood",
            "Musculoskeletal",
            "Ear/mastoid",
            "Nervous",
            "Respiratory",
            "Skin"
        )
    )) %>%
    glimpse()

yaxis_max_perc = max(results_perc$perc_sig_dir)
yaxis_min_perc = min(results_perc$perc_sig_dir)

g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

plt2 = ggplot(results_perc, aes(x=chapter, y=perc_sig_dir, fill=Type)) +
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-1, ymax=1) + 
    geom_col(position=position_dodge()) + 
    geom_hline(yintercept = 0, linewidth=1) + 
    geom_vline(xintercept=seq(0,length(levels(results_perc$chapter)))+0.5 ,color="black", alpha=0.2) +
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
ggsave(paste0("./visualization/miami_fdr05_max_perc.png"), width=20, height=12)
print(paste0("./visualization/miami_fdr05_max_perc.png"))

#endregion

#region Manhattan plots by ethnicity (GLM)

results_glm_eth = as.data.frame(fread("./results/results_dx_glm_ethnicity.tsv"))

list_eth = c("white", "black", "asian")

for (e in 1:length(list_eth)) {

    # Clean results for specific ethnicity
    results_tmp = results_glm_eth %>%
        select(c(icd_code, diagnosis, chapter, categ, n_dx_males, n_dx_females, n_hc_males, n_hc_females, contains(list_eth[e]))) %>%
        rename_with(~ str_remove(., paste0("_",list_eth[e],"$"))) %>%
        mutate(
            icd_code = factor(icd_code),
            categ = factor(categ),
            chapter = factor(chapter,
                levels=c(
                    "Certain conditions originating in the perinatal period",
                    "Diseases of the eye and adnexa",
                    "Mental, Behavioral and Neurodevelopmental disorders",
                    "Congenital malformations, deformations and chromosomal abnormalities",
                    "Certain infectious and parasitic diseases",
                    "Endocrine, nutritional and metabolic diseases",
                    "Pregnancy, childbirth and the puerperium",
                    "Diseases of the circulatory system",
                    "Diseases of the genitourinary system",
                    "Diseases of the digestive system",
                    "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
                    "Diseases of the musculoskeletal system and connective tissue",
                    "Diseases of the ear and mastoid process",
                    "Diseases of the nervous system",
                    "Diseases of the respiratory system",
                    "Diseases of the skin and subcutaneous tissue"
                ),
                labels=c(
                    "Perinatal",
                    "Eye",
                    "Mental",
                    "Congenital malformations",
                    "Infectious/parasitic",
                    "Endocrine/metabolic",
                    "Pregnancy",
                    "Circulatory",
                    "Genitourinary",
                    "Digestive",
                    "Blood",
                    "Musculoskeletal",
                    "Ear/mastoid",
                    "Nervous",
                    "Respiratory",
                    "Skin"
                )
            )
        ) %>%
        rename(
            ndx_males = n_dx_males,
            ndx_females = n_dx_females,
            nhc_males = n_hc_males,
            nhc_females = n_hc_females,
            coefinter_males = coef_inter_males,
            coefinter_females = coef_inter_females,
            pvalinter_males = pval_inter_males,
            pvalinter_females = pval_inter_females
        ) %>% 
        select(-c(or_males_lower, or_males_upper, or_females_lower, or_females_upper)) %>%
        pivot_longer(cols=-c(icd_code, chapter, categ, diagnosis), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>%
        mutate(Stat = factor(Stat), Type = factor(Type)) %>%
        pivot_wider(names_from=Stat, values_from=Value) %>%
        filter(is.na(coef) == FALSE) %>%
        mutate(
            labels = paste0(icd_code, "\n", diagnosis, "\n", categ)
        ) %>%
        # Keep only diagnoses with >20 ppl in case group (separated by sex)
        filter(ndx >= 20) %>%
        # P-value correction
        mutate(
            pval_cor = p.adjust(pval, method="fdr"),
            pvalinter_cor = p.adjust(pvalinter, method="fdr"),
            sig_cor = factor(if_else(pval_cor < 0.05, 1, 0), levels=c(0,1)),
            siginter_cor = factor(if_else(pvalinter_cor < 0.05, 1, 0), levels=c(0,1))
        ) %>%
        # -log10 * direction
        mutate(
            pval_log = if_else(coef < 0, -log(pval+.Machine$double.xmin,10)*-1, -log(pval+.Machine$double.xmin,10)),
            pval_cor_log = if_else(coef < 0, -log(pval_cor+.Machine$double.xmin,10)*-1, -log(pval_cor+.Machine$double.xmin,10)),
            pvalinter_log = if_else(coefinter < 0, -log(pvalinter+.Machine$double.xmin,10)*-1, -log(pvalinter+.Machine$double.xmin,10)),
            pvalinter_cor_log = if_else(coefinter < 0, -log(pvalinter_cor+.Machine$double.xmin,10)*-1, -log(pvalinter_cor+.Machine$double.xmin,10))
        ) %>%
        # Assign color
        mutate(
            pval_cor_color = ifelse(sig_cor == 0, "#e0e2e2", ifelse(Type == "males", scale_female_male_dis['Male'], scale_female_male_dis['Female'])),
            pvalinter_cor_color = ifelse(siginter_cor == 0, "#e0e2e2", ifelse(Type == "males", scale_female_male_dis['Male'], scale_female_male_dis['Female']))
        ) %>%
        left_join(labels_to_viz, by="diagnosis") %>%
        # Keep only labels to annotate
        mutate(
            labels = stringr::str_wrap(clean_names ,30)
        )

    fwrite(results_tmp %>% select(-labels), paste0("./results/results_dx_glm_",list_eth[e],"_clean.tsv"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

    # Clean % of significant dx by chapter/sex/IGL direction
    chapter_totals = results_tmp %>%
        distinct(chapter, icd_code) %>%
        count(chapter, name = "n_total") %>%
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
        group_by(chapter, Type, direction) %>%
        summarise(n_sig = n(), .groups = "drop") %>%
        right_join(chapter_totals, by = c("chapter", "Type", "direction")) %>%
        mutate(
            n_sig = ifelse(is.na(n_sig), 0.001, n_sig),
            perc_sig = n_sig / n_total,
            perc_sig_dir = ifelse(direction == "negative", perc_sig*-1, perc_sig)
        ) %>%
        mutate(
            Type = factor(Type, levels=c("females", "males"), labels=c("Female", "Male"))
        ) %>%
        mutate(chapter = factor(chapter,
            levels=c(
                "Perinatal",
                "Eye",
                "Mental",
                "Congenital malformations",
                "Infectious/parasitic",
                "Endocrine/metabolic",
                "Pregnancy",
                "Circulatory",
                "Genitourinary",
                "Digestive",
                "Blood",
                "Musculoskeletal",
                "Ear/mastoid",
                "Nervous",
                "Respiratory",
                "Skin"
            )
        ))

    results_perc_inter = results_tmp %>%
        mutate(
            direction = if_else(coefinter > 0, "positive", "negative"),
            sig = pvalinter_cor < 0.05
        ) %>%
        filter(siginter_cor == 1) %>%
        group_by(chapter, Type, direction) %>%
        summarise(n_sig = n(), .groups = "drop") %>%
        right_join(chapter_totals, by = c("chapter", "Type", "direction")) %>%
        mutate(
            n_sig = ifelse(is.na(n_sig), 0.001, n_sig),
            perc_sig = n_sig / n_total,
            perc_sig_dir = ifelse(direction == "negative", perc_sig*-1, perc_sig)
        ) %>%
        mutate(
            Type = factor(Type, levels=c("females", "males"), labels=c("Female", "Male"))
        ) %>%
        mutate(chapter = factor(chapter,
            levels=c(
                "Perinatal",
                "Eye",
                "Mental",
                "Congenital malformations",
                "Infectious/parasitic",
                "Endocrine/metabolic",
                "Pregnancy",
                "Circulatory",
                "Genitourinary",
                "Digestive",
                "Blood",
                "Musculoskeletal",
                "Ear/mastoid",
                "Nervous",
                "Respiratory",
                "Skin"
            )
        ))

    # Plot main effect
    yaxis_max = 100
    yaxis_lim = 20

    g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

    plt_main = ggplot(results_tmp, aes(x=chapter, y=pval_log, color=factor(pval_cor_color), text=labels)) + 
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
        geom_vline(xintercept=seq(0,length(levels(results_tmp$chapter)))+0.5 ,color="black", alpha=0.2) +
        geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 0), height=0, alpha=0.5) + 
        geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 1), height=0, alpha=0.5) + 
        geom_hline(yintercept = 0, linewidth=0.5) +
        geom_label_repel(aes(label=labels), seed = 123, size=4, box.padding=0.1, label.padding=0.1, force=3, max.overlaps=20, direction="y", label.r=0, nudge_x = -1.2, min.segment.length=0, force_pull=0.5) +
        geom_point(data = results_tmp %>% filter(is.na(labels) == FALSE), alpha=0.5) + 
        scale_x_discrete(name = "") + 
        scale_color_identity() +
        labs(title = NULL) +
        theme_classic() + 
        theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5, size=18, face="bold"),
            legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
            strip.text = element_text(size = 18, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
            axis.text.x = element_text(angle = 30, hjust=1))

    # Max pval
    plt_main + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)
    ggsave(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_fdr05_max.png"), width=20, height=10)
    print(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_fdr05_max.png"))

    # Pval capped at 20
    plt_main + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_lim,yaxis_lim), oob=scales::squish)
    ggsave(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_fdr05_lim.png"), width=20, height=10)
    print(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_fdr05_lim.png"))

    # Plot % of diagnoses for main effect
    g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

    plt_main_perc = ggplot(results_perc, aes(x=chapter, y=perc_sig_dir, fill=Type)) +
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-1, ymax=1) + 
        geom_col(position=position_dodge(), na.rm = FALSE) + 
        geom_hline(yintercept = 0, linewidth=1) + 
        geom_vline(xintercept=seq(0,length(levels(results_perc$chapter)))+0.5 ,color="black", alpha=0.2) +
        geom_text(
            aes(label = sprintf("%.0f%%", abs(perc_sig_dir) * 100),vjust = ifelse(perc_sig_dir > 0, -0.3, 1.3)),
            position = position_dodge(width = 0.9), size = 5
        ) +
        scale_fill_manual(values=scale_female_male_dis) +
        scale_x_discrete(name="", drop = FALSE, expand = expansion(add = c(1.2, 0.6))) +
        scale_y_continuous(
            name="% of diagnoses", breaks=c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), labels=c("75%", "50%", "25%", "0%", "25%", "50%", "75%"),
            expand = expansion(add = c(0.2, 0.2))
        ) +
        theme_classic() + 
        theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5, size=18, face="bold"),
            legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
            strip.text = element_text(size = 18, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
            axis.text.x = element_text(angle = 30, hjust=1))

    # Concatenate plots

    plt_main = plt_main + 
        scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)

    plt_main_perc = plt_main_perc + 
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank()
        )

    plt_main_perc / plt_main  + plot_layout(heights = c(1, 2.5))
    ggsave(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_fdr05_max_perc.png"), width=20, height=8)
    print(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_fdr05_max_perc.png"))

    # Plot ethnicity interaction
    yaxis_max = 50
    yaxis_lim = 10

    g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

    plt_inter = ggplot(results_tmp, aes(x=chapter, y=pvalinter_log, color=factor(pvalinter_cor_color), text=labels)) + 
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
        geom_vline(xintercept=seq(0,length(levels(results_tmp$chapter)))+0.5 ,color="black", alpha=0.2) +
        geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 0), height=0, alpha=0.5) + 
        geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 1), height=0, alpha=0.5) + 
        geom_hline(yintercept = 0, linewidth=0.5) +
        geom_label_repel(aes(label=labels), seed = 123, size=4, box.padding=0.1, label.padding=0.1, force=3, max.overlaps=20, direction="y", label.r=0, nudge_x = -1.2, min.segment.length=0, force_pull=0.5) +
        geom_point(data = results_tmp %>% filter(is.na(labels) == FALSE), alpha=0.5) + 
        scale_x_discrete(name = "") + 
        scale_color_identity() +
        labs(title = NULL) +
        theme_classic() + 
        theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5, size=18, face="bold"),
            legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
            strip.text = element_text(size = 18, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
            axis.text.x = element_text(angle = 30, hjust=1))

    # Max pval
    plt_inter + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)
    ggsave(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_inter_fdr05_max.png"), width=20, height=10)
    print(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_inter_fdr05_max.png"))

    # Pval capped at 1-
    plt_inter + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_lim,yaxis_lim), oob=scales::squish)
    ggsave(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_inter_fdr05_lim.png"), width=20, height=10)
    print(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_inter_fdr05_lim.png"))

    # Plot % of diagnoses for main effect
    g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

    plt_inter_perc = ggplot(results_perc_inter, aes(x=chapter, y=perc_sig_dir, fill=Type)) +
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-1, ymax=1) + 
        geom_col(position=position_dodge(), na.rm = FALSE) + 
        geom_hline(yintercept = 0, linewidth=1) + 
        geom_vline(xintercept=seq(0,length(levels(results_perc_inter$chapter)))+0.5 ,color="black", alpha=0.2) +
        geom_text(
            aes(label = sprintf("%.0f%%", abs(perc_sig_dir) * 100),vjust = ifelse(perc_sig_dir > 0, -0.3, 1.3)),
            position = position_dodge(width = 0.9), size = 5
        ) +
        scale_fill_manual(values=scale_female_male_dis) +
        scale_x_discrete(name="", drop = FALSE, expand = expansion(add = c(1.2, 0.6))) +
        scale_y_continuous(
            name="% of diagnoses", breaks=c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), labels=c("75%", "50%", "25%", "0%", "25%", "50%", "75%"),
            expand = expansion(add = c(0.2, 0.2))
        ) +
        theme_classic() + 
        theme(text = element_text(size=18), plot.title = element_text(hjust = 0.5, size=18, face="bold"),
            legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
            strip.text = element_text(size = 18, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
            axis.text.x = element_text(angle = 30, hjust=1))

    # Concatenate plots

    plt_inter = plt_inter + 
        scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)

    plt_inter_perc = plt_inter_perc + 
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank()
        )

    plt_inter_perc / plt_inter  + plot_layout(heights = c(1, 2.5))
    ggsave(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_inter_fdr05_max_inter.png"), width=20, height=8)
    print(paste0("./visualization/glm_ethnicity/miami_",list_eth[e],"_inter_fdr05_max_inter.png"))

}

#endregion

#region Manhattan plot for pregnancy covaried for live births results

results_glm = as.data.frame(fread("./results/results_dx_glm_cov_births.tsv"))

results_glm = results_glm %>%
    mutate(
        icd_code = factor(icd_code),
        categ = factor(categ),
        chapter = factor(chapter,
            levels=c(
                "Certain conditions originating in the perinatal period",
                "Diseases of the eye and adnexa",
                "Mental, Behavioral and Neurodevelopmental disorders",
                "Congenital malformations, deformations and chromosomal abnormalities",
                "Certain infectious and parasitic diseases",
                "Endocrine, nutritional and metabolic diseases",
                "Pregnancy, childbirth and the puerperium",
                "Diseases of the circulatory system",
                "Diseases of the genitourinary system",
                "Diseases of the musculoskeletal system and connective tissue",
                "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
                "Diseases of the digestive system",
                "Diseases of the ear and mastoid process",
                "Diseases of the nervous system",
                "Diseases of the respiratory system",
                "Diseases of the skin and subcutaneous tissue"
            ),
            labels=c(
                "Perinatal",
                "Eye",
                "Mental",
                "Congenital malformations",
                "Infectious/parasitic",
                "Endocrine/metabolic",
                "Pregnancy",
                "Circulatory",
                "Genitourinary",
                "Musculoskeletal",
                "Blood",
                "Digestive",
                "Ear/mastoid",
                "Nervous",
                "Respiratory",
                "Skin"
            )
        )
    ) %>%
    rename(
        ndx_males = n_dx_males,
        ndx_females = n_dx_females,
        nhc_males = n_hc_males,
        nhc_females = n_hc_females
    ) %>% 
    select(-c(or_males_lower, or_males_upper, or_females_lower, or_females_upper)) %>%
    pivot_longer(cols=-c(icd_code, chapter, categ, diagnosis), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>%
    mutate(Stat = factor(Stat), Type = factor(Type)) %>%
    pivot_wider(names_from=Stat, values_from=Value) %>%
    filter(is.na(coef) == FALSE) %>%
    mutate(
        labels = paste0(icd_code, "\n", diagnosis, "\n", categ),
        or_direction = factor(ifelse(or<1, "Female", "Male"), levels=c("Female", "Male")),
        or = ifelse(or < 1, 1/or, or),
        # Calculate the percent of risk increase with direction
        perc_odds_increase = ifelse(or_direction=="Female", (or-1)*-100, (or-1)*100)
    ) %>%
    glimpse()

# Manipulate p-values
results_tmp = results_glm %>%
    # Keep only diagnoses with >20 ppl in case group (separated by sex)
    filter(ndx >= 20) %>%
    # P-value correction
    mutate(
        pval_cor = p.adjust(pval, method="fdr"),
        sig_cor = factor(if_else(pval_cor < 0.05, 1, 0), levels=c(0,1)),
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
    left_join(labels_to_viz, by="diagnosis") %>%
    # Keep only labels to annotate
    mutate(
        labels = stringr::str_wrap(clean_names ,30)
    ) %>%
    glimpse()

fwrite(results_tmp %>% select(-labels), "./results/results_dx_glm_cov_births_clean.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

# Plot p-values
yaxis_max = yaxis_max_all
yaxis_lim = 20

g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

plt = ggplot(results_tmp, aes(x=chapter, y=pval_log, color=factor(pval_cor_color), text=labels)) + 
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    geom_vline(xintercept=seq(0,length(unique(results_glm$chapter)))+0.5 ,color="black", alpha=0.2) +
    geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 0), height=0, alpha=0.5) + 
    geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 1), height=0, alpha=0.5) + 
    geom_hline(yintercept = 0, linewidth=0.5) +
    geom_label_repel(aes(label=labels), seed = 123, size=5, box.padding=0.1, label.padding=0.2, force=3, max.overlaps=20, direction="y", label.r=0, nudge_x = -1.2, min.segment.length=0, force_pull=0.5) +
    geom_point(data = results_tmp %>% filter(is.na(labels) == FALSE), alpha=0.5) + 
    scale_x_discrete(name = "", expand = expansion(add = c(0, 0.6))) + 
    scale_color_identity() +
    labs(title = NULL) +
    theme_classic() + 
    theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
        strip.text = element_text(size = 20, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
        axis.text.x = element_text(angle = 30, hjust=1))

# Max pval
plt + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)
ggsave(paste0("./visualization/miami_fdr05_cov_births_max.png"), width=5, height=10)
print(paste0("./visualization/miami_fdr05_cov_birth_max.png"))

# Pval capped at 20
plt + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_lim,yaxis_lim), oob=scales::squish)
ggsave(paste0("./visualization/miami_fdr05_cov_birth_lim.png"), width=5, height=10)
print(paste0("./visualization/miami_fdr05_cov_birth_lim.png"))

# Plot % of significant dx by chapter/sex/IGL direction
chapter_totals = results_tmp %>%
    distinct(chapter, icd_code) %>%
    count(chapter, name = "n_total") %>%
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
    group_by(chapter, Type, direction) %>%
    summarise(n_sig = n(), .groups = "drop") %>%
    right_join(chapter_totals, by = c("chapter", "Type", "direction")) %>%
    mutate(
        n_sig = ifelse(is.na(n_sig), 0.001, n_sig),
        perc_sig = n_sig / n_total,
        perc_sig_dir = ifelse(direction == "negative", perc_sig*-1, perc_sig)
    ) %>%
    mutate(
        Type = factor(Type, levels=c("females", "males"), labels=c("Female", "Male"))
    ) %>%
    mutate(chapter = factor(chapter,
        levels=c(
            "Perinatal",
            "Eye",
            "Mental",
            "Congenital malformations",
            "Infectious/parasitic",
            "Endocrine/metabolic",
            "Pregnancy",
            "Circulatory",
            "Genitourinary",
            "Musculoskeletal",
            "Blood",
            "Digestive",
            "Ear/mastoid",
            "Nervous",
            "Respiratory",
            "Skin"
        )
    )) %>%
    glimpse()

g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

plt2 = ggplot(results_perc, aes(x=chapter, y=perc_sig_dir, fill=Type)) +
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-1, ymax=1) + 
    geom_col(position=position_dodge()) + 
    geom_hline(yintercept = 0, linewidth=1) + 
    geom_vline(xintercept=seq(0,length(unique(results_perc$chapter)))+0.5 ,color="black", alpha=0.2) +
    geom_text(
        aes(label = sprintf("%.0f%%", abs(perc_sig_dir) * 100),vjust = ifelse(perc_sig_dir > 0, -0.3, 1.3)),
        position = position_dodge(width = 0.9), size = 5
    ) +
    scale_fill_manual(values=scale_female_male_dis) +
    scale_x_discrete(name="", expand = expansion(add = c(1.2, 0.6))) +
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
ggsave(paste0("./visualization/miami_fdr05_cov_births_max_perc.png"), width=5, height=12)
print(paste0("./visualization/miami_fdr05_cov_births_max_perc.png"))

#endregion

#region Manhattan plot for pregnancy covaried for live births results (black IGL)

results_glm = as.data.frame(fread("./results/results_dx_glm_black_cov_births.tsv"))

results_glm = results_glm %>%
    mutate(
        icd_code = factor(icd_code),
        categ = factor(categ),
        chapter = factor(chapter,
            levels=c(
                "Certain conditions originating in the perinatal period",
                "Diseases of the eye and adnexa",
                "Mental, Behavioral and Neurodevelopmental disorders",
                "Congenital malformations, deformations and chromosomal abnormalities",
                "Certain infectious and parasitic diseases",
                "Endocrine, nutritional and metabolic diseases",
                "Pregnancy, childbirth and the puerperium",
                "Diseases of the circulatory system",
                "Diseases of the genitourinary system",
                "Diseases of the musculoskeletal system and connective tissue",
                "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
                "Diseases of the digestive system",
                "Diseases of the ear and mastoid process",
                "Diseases of the nervous system",
                "Diseases of the respiratory system",
                "Diseases of the skin and subcutaneous tissue"
            ),
            labels=c(
                "Perinatal",
                "Eye",
                "Mental",
                "Congenital malformations",
                "Infectious/parasitic",
                "Endocrine/metabolic",
                "Pregnancy",
                "Circulatory",
                "Genitourinary",
                "Musculoskeletal",
                "Blood",
                "Digestive",
                "Ear/mastoid",
                "Nervous",
                "Respiratory",
                "Skin"
            )
        )
    ) %>%
    rename(
        ndx_males = n_dx_males,
        ndx_females = n_dx_females,
        nhc_males = n_hc_males,
        nhc_females = n_hc_females
    ) %>% 
    select(-c(or_males_lower, or_males_upper, or_females_lower, or_females_upper)) %>%
    pivot_longer(cols=-c(icd_code, chapter, categ, diagnosis), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>%
    mutate(Stat = factor(Stat), Type = factor(Type)) %>%
    pivot_wider(names_from=Stat, values_from=Value) %>%
    filter(is.na(coef) == FALSE) %>%
    mutate(
        labels = paste0(icd_code, "\n", diagnosis, "\n", categ),
        or_direction = factor(ifelse(or<1, "Female", "Male"), levels=c("Female", "Male")),
        or = ifelse(or < 1, 1/or, or),
        # Calculate the percent of risk increase with direction
        perc_odds_increase = ifelse(or_direction=="Female", (or-1)*-100, (or-1)*100)
    ) %>%
    glimpse()

# Manipulate p-values
results_tmp = results_glm %>%
    # Keep only diagnoses with >20 ppl in case group (separated by sex)
    filter(ndx >= 20) %>%
    # P-value correction
    mutate(
        pval_cor = p.adjust(pval, method="fdr"),
        sig_cor = factor(if_else(pval_cor < 0.05, 1, 0), levels=c(0,1)),
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
    left_join(labels_to_viz, by="diagnosis") %>%
    # Keep only labels to annotate
    mutate(
        labels = stringr::str_wrap(clean_names ,30)
    ) %>%
    glimpse()

fwrite(results_tmp %>% select(-labels), "./results/results_dx_glm_black_cov_births_clean.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

# Plot p-values
yaxis_max = 100
yaxis_lim = 20

g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

plt = ggplot(results_tmp, aes(x=chapter, y=pval_log, color=factor(pval_cor_color), text=labels)) + 
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
    geom_vline(xintercept=seq(0,length(unique(results_glm$chapter)))+0.5 ,color="black", alpha=0.2) +
    geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 0), height=0, alpha=0.5) + 
    geom_jitter(data = results_tmp %>% filter(is.na(labels) == TRUE & sig_cor == 1), height=0, alpha=0.5) + 
    geom_hline(yintercept = 0, linewidth=0.5) +
    geom_label_repel(aes(label=labels), seed = 123, size=5, box.padding=0.1, label.padding=0.2, force=3, max.overlaps=20, direction="y", label.r=0, nudge_x = -1.2, min.segment.length=0, force_pull=0.5) +
    geom_point(data = results_tmp %>% filter(is.na(labels) == FALSE), alpha=0.5) + 
    scale_x_discrete(name = "", expand = expansion(add = c(0, 0.6))) + 
    scale_color_identity() +
    labs(title = NULL) +
    theme_classic() + 
    theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        legend.title = element_blank(), legend.position="none", legend.text=element_text(size=13),
        strip.text = element_text(size = 20, color="black", face="bold"), strip.background = element_rect(fill="#ffffff"),
        axis.text.x = element_text(angle = 30, hjust=1))

# Max pval
plt + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_max,yaxis_max), oob=scales::squish)
ggsave(paste0("./visualization/miami_fdr05_black_cov_births_max.png"), width=5, height=10)
print(paste0("./visualization/miami_fdr05_black_cov_birth_max.png"))

# Pval capped at 20
plt + scale_y_continuous(name = "-log10(p) * Direction", limits=c(-yaxis_lim,yaxis_lim), oob=scales::squish)
ggsave(paste0("./visualization/miami_fdr05_black_cov_birth_lim.png"), width=5, height=10)
print(paste0("./visualization/miami_fdr05_black_cov_birth_lim.png"))

# Plot % of significant dx by chapter/sex/IGL direction
chapter_totals = results_tmp %>%
    distinct(chapter, icd_code) %>%
    count(chapter, name = "n_total") %>%
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
    group_by(chapter, Type, direction) %>%
    summarise(n_sig = n(), .groups = "drop") %>%
    right_join(chapter_totals, by = c("chapter", "Type", "direction")) %>%
    mutate(
        n_sig = ifelse(is.na(n_sig), 0.001, n_sig),
        perc_sig = n_sig / n_total,
        perc_sig_dir = ifelse(direction == "negative", perc_sig*-1, perc_sig)
    ) %>%
    mutate(
        Type = factor(Type, levels=c("females", "males"), labels=c("Female", "Male"))
    ) %>%
    mutate(chapter = factor(chapter,
        levels=c(
            "Perinatal",
            "Eye",
            "Mental",
            "Congenital malformations",
            "Infectious/parasitic",
            "Endocrine/metabolic",
            "Pregnancy",
            "Circulatory",
            "Genitourinary",
            "Musculoskeletal",
            "Blood",
            "Digestive",
            "Ear/mastoid",
            "Nervous",
            "Respiratory",
            "Skin"
        )
    )) %>%
    glimpse()

g = rasterGrob((scale_gender_cont[seq(80,20)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

plt2 = ggplot(results_perc, aes(x=chapter, y=perc_sig_dir, fill=Type)) +
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-1, ymax=1) + 
    geom_col(position=position_dodge()) + 
    geom_hline(yintercept = 0, linewidth=1) + 
    geom_vline(xintercept=seq(0,length(unique(results_perc$chapter)))+0.5 ,color="black", alpha=0.2) +
    geom_text(
        aes(label = sprintf("%.0f%%", abs(perc_sig_dir) * 100),vjust = ifelse(perc_sig_dir > 0, -0.3, 1.3)),
        position = position_dodge(width = 0.9), size = 5
    ) +
    scale_fill_manual(values=scale_female_male_dis) +
    scale_x_discrete(name="", expand = expansion(add = c(1.2, 0.6))) +
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
ggsave(paste0("./visualization/miami_fdr05_black_cov_births_max_perc.png"), width=5, height=12)
print(paste0("./visualization/miami_fdr05_black_cov_births_max_perc.png"))

#endregion
