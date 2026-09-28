
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(RColorBrewer)
library(grid)
library(ggpubr)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

df_gender = as.data.frame(fread("../gender_score_new/results/all_tp0/gender_score_combined.tsv"))

df_genetic = as.data.frame(fread("../../../UKB/tabular/df_genetic_cov/UKBB_genetic_cov_wider.tsv"))

df_orientation = as.data.frame(fread("../../../UKB/tabular/df_samesex_intercourse/UKBB_samesex_intercourse_wide.tsv"))
df_orientation = df_orientation %>%
    filter(InstanceID == 0) %>%
    rename(ID = "SubjectID", same_sex_intercourse = "Ever had same-sex intercourse_2159", number_same_sex_partners = "Lifetime number of same-sex sexual partners_3669") %>%
    select(ID, same_sex_intercourse, number_same_sex_partners) %>%
    glimpse()

#endregion

#region Sex incongruent

df_tmp = df_gender %>%
    mutate(selfreport_genetic = factor(
        paste0(Sex, " - ", Genetic_sex),
        levels=c("Female - Female", "Female - Male", "Male - Female", "Male - Male"))) %>%
    # Remove ppl with sex aneuploidy
    filter(Sex_aneuploidy == 0) %>%
    glimpse()

counts = df_tmp %>%
    count(selfreport_genetic) %>%
    mutate(label = paste0(selfreport_genetic, " (n=", n, ")"))
counts$label[1] = "Female - Female"
counts$label[4] = "Male - Male"

my_comparisons = list(
    c("Female - Female", "Female - Male"), c("Female - Female", "Male - Female"),
    c("Male - Male", "Female - Male"), c("Male - Male", "Male - Female"))

# Violin plots

scale_female_male_congruent_incongruent = c("Female - Female"="#4d0084","Female - Male" = "#A82EFF", "Male - Female"="#409432", "Male - Male"="#097800")
g = rasterGrob(rev(scale_gender_cont[seq(20,80)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

plt = ggplot(df_tmp, aes(x=selfreport_genetic, y = Probability_male_mean, fill=selfreport_genetic)) + 
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=1) + 
    geom_violin(trim=TRUE, scale="width", alpha=0.8) + 
    geom_boxplot(width=0.2, alpha=1) + 
    geom_jitter(data = df_tmp %>% filter(selfreport_genetic %in% c("Female - Male", "Male - Female")), height=0, width=0.2, alpha=0.5) +
    scale_x_discrete(name="", labels=counts$label) +
    scale_y_continuous(name="\n\n\n\nIGL", limits=c(0,1.25), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
    scale_fill_manual(values = scale_female_male_congruent_incongruent, guide = "none") +
    ggtitle("Sex: Self-report - Genetic") +
    theme_light() + 
    theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size=13),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

plt + stat_compare_means(comparisons = my_comparisons, method="t.test", label="p.signif", label.y = c(1.0, 1.08, 1.16, 1.0), hide.ns=FALSE, vjust=0.3)
ggsave("./visualization/sex_incongruent_violin_sig.png", width=10, height=10)

plt + stat_compare_means(comparisons = my_comparisons, method="t.test", label="p.format", label.y = c(1.0, 1.08, 1.16, 1.0), hide.ns=FALSE, vjust=0.3)
ggsave("./visualization/sex_incongruent_violin_pval.png", width=10, height=10)

#endregion

#region Sexual orientation

df_tmp = df_gender %>%
    left_join(df_orientation, by="ID") %>%
    # Remove sex incongruent
    filter(Sex == Genetic_sex) %>%
    mutate(same_sex_intercourse_label = factor(case_when(
        (same_sex_intercourse == "No" &  Sex == "Male") ~ "Male - No same-sex intercourse",
        (same_sex_intercourse == "Yes" &  Sex == "Male") ~ "Male - Had same-sex intercourse",
        (same_sex_intercourse == "No" &  Sex == "Female") ~ "Female - No same-sex intercourse",
        (same_sex_intercourse == "Yes" &  Sex == "Female") ~ "Female - Had same-sex intercourse",
    ), levels=c(
        "Female - No same-sex intercourse", "Female - Had same-sex intercourse",
        "Male - Had same-sex intercourse", "Male - No same-sex intercourse"
    ))) %>%
    filter(!is.na(same_sex_intercourse_label)) %>%
    glimpse()

my_comparisons <- list(
    c("Female - No same-sex intercourse", "Female - Had same-sex intercourse"),
    c("Female - No same-sex intercourse", "Male - Had same-sex intercourse"),
    c("Male - No same-sex intercourse", "Female - Had same-sex intercourse"),
    c("Male - Had same-sex intercourse", "Male - No same-sex intercourse")
    )

# Violin plots

scale_female_sex_orientation = c("Female - No same-sex intercourse"="#4d0084","Female - Had same-sex intercourse" = "#A82EFF", "Male - Had same-sex intercourse"="#409432", "Male - No same-sex intercourse"="#097800")
g = rasterGrob(rev(scale_gender_cont[seq(20,80)]), width=unit(1,"npc"), height = unit(1,"npc"), interpolate = TRUE)

# Violin plots
plt = ggplot(df_tmp, aes(x=same_sex_intercourse_label, y = Probability_male_mean, fill=same_sex_intercourse_label)) + 
    annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=1) + 
    geom_violin(trim=TRUE, scale="width", alpha=0.8) + 
    geom_boxplot(width=0.2, alpha=1) + 
    scale_x_discrete(name="") +
    scale_y_continuous(name="\n\n\n\n\nIGL", limits=c(0,1.25), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
    scale_fill_manual(values = scale_female_sex_orientation, guide = "none") +
    ggtitle("Sex - Same-sex intercourse") +
    theme_light() + 
    theme(text = element_text(size=25), plot.title = element_text(hjust = 0.5, size=25, face="bold"),
        legend.title = element_blank(), legend.position="bottom", legend.text=element_text(size=13),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

plt + stat_compare_means(comparisons = my_comparisons, method="t.test", label="p.signif", label.y = c(1.0, 1.08, 1.16, 1.0), hide.ns=FALSE, vjust=0.3)
ggsave("./visualization/sex_orientation_violin_sig.png", width=10, height=10)

plt + stat_compare_means(comparisons = my_comparisons, method="t.test", label="p.format", label.y = c(1.0, 1.08, 1.16, 1.0), hide.ns=FALSE, vjust=0.3)
ggsave("./visualization/sex_orientation_violin_pval.png", width=10, height=10)

#endregion
