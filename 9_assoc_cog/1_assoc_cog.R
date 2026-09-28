
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
library(stringr)
library(effects)
library(lme4)
library(lmerTest)
library(splines)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

df_cog = as.data.frame(fread("../../../UKB/Analyses/clean_cog/results/df_cog.tsv"))

df_cog = df_cog %>% 
    mutate(prospective_memory_20018 = factor(prospective_memory_20018)) %>%
    filter(InstanceID == 2)

df_raw = as.data.frame(fread("../impute_input/results/df_imputed_tp_all_combined.tsv"))

df_gender = as.data.frame(fread("../gender_score_new/results/all_tp2/gender_score_combined.tsv"))
df_gender = df_gender %>%
    select(ID, Age, Sex, Genetic_sex, Sex_aneuploidy, Probability_male_mean) %>%
    rename(Gender_score = "Probability_male_mean") %>%
    mutate(Sex = factor(Sex, levels=c("Female", "Male")), Genetic_sex = factor(Genetic_sex, levels=c("Female", "Male"))) %>%
    glimpse()

df_cov = as.data.frame(fread("../../../UKB/tabular/df_body_cov/UKBB_body_cov_wide.tsv"))
df_cov = df_cov %>%
    filter(InstanceID == 2) %>%
    rename(ID = "SubjectID", height = "Standing height_50", assessment_center = "UK Biobank assessment centre_54", weight = "Weight_21002") %>%
    select(ID, assessment_center, height, weight) %>%
    mutate(assessment_center = factor(assessment_center)) %>%
    filter(complete.cases(.)) %>%
    glimpse()

#endregion

#region Association of every cognitive variable with gender

# name_field="reaction_time_20023"

# Function
assoc_gender = function(name_field) {
    
    df_lm <<- df_cog %>% 
        select(ID, all_of(name_field)) %>% # Select brain var
        left_join(df_gender, by="ID") %>%
        left_join(df_cov, by="ID") %>%
        rename(Cognition = all_of(name_field)) %>%
        filter(complete.cases(.))

    if (nrow(df_lm) < 5) { return("Fewer than 5 subjects with data") }
    if ( is.numeric(df_lm$Cognition) == FALSE ) { return("Non-numeric brain variable") }

    # Run linear models
    lm_males = summary(lmer(scale(Cognition) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + (1|assessment_center), data = df_lm %>% filter(Sex == "Male")))
    lm_females = summary(lmer(scale(Cognition) ~ scale(Gender_score) + bs(Age, degree = 3, df = 4) + (1|assessment_center), data = df_lm %>% filter(Sex == "Female")))
    lm_inter = summary(lmer(scale(Cognition) ~ Sex + Gender_score + Gender_score*Sex + bs(Age, degree = 3, df = 4) + (1|assessment_center), data = df_lm))

    results_i = data.frame(
            Cog_test = name_field,
            n_females = nrow(df_lm %>% filter(Sex == "Female")),
            n_males = nrow(df_lm %>% filter(Sex == "Male")),
            n_all = nrow(df_lm),
            coef_males = lm_males$coefficients["scale(Gender_score)","Estimate"],
            pval_males = lm_males$coefficients["scale(Gender_score)","Pr(>|t|)"],
            coef_females = lm_females$coefficients["scale(Gender_score)","Estimate"],
            pval_females = lm_females$coefficients["scale(Gender_score)","Pr(>|t|)"],
            coef_inter = lm_inter$coefficients["SexMale:Gender_score","Estimate"],
            pval_inter = lm_inter$coefficients["SexMale:Gender_score","Pr(>|t|)"],
            coef_sexdiff = lm_inter$coefficients["SexMale", "Estimate"],
            pval_sexdiff = lm_inter$coefficients["SexMale", "Pr(>|t|)"],
            coef_igl_female = lm_inter$coefficients["Gender_score", "Estimate"],
            coef_igl_male = lm_inter$coefficients["Gender_score", "Estimate"] + lm_inter$coefficients["SexMale:Gender_score", "Estimate"]
        )
    
    return(results_i)
}

# Run for every cognitive test
cog_names = colnames(df_cog)[seq(3, ncol(df_cog))]

results = list()
for (i in 1:length(cog_names)) {
    results[[i]] = assoc_gender(cog_names[i])
}
results = Filter(is.data.frame, results)
results = do.call(rbind, results)

fwrite(results, "./results/results_cog.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

results = as.data.frame(fread("./results/results_cog.tsv"))

# Clean results
df_results = results %>%
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    rename(n_inter = "n_all") %>%
    mutate(
        Cog_test = factor(Cog_test,
            levels=c("pairs_matching_399", "numeric_memory_4282", "trail_making_test_1_6348", "trail_making_test_2_6350", "matrix_pattern_6373", "fluid_intelligence_20016", "reaction_time_20023", "tower_rearranging_21004", "symbol_substitution_23324"),
            labels=c("Pairs matching", "Numeric memory", "Trail making test #1", "Trail making test #2", "Matrix pattern", "Fluid intelligence", "Reactiom time", "Tower rearranging", "Symbol substitution"))
    ) %>%
    glimpse()

#endregion

#region Functions

# Function to plot associations (NO RANK)

# df_func = df_results
# width_height = c(5,5)
# list_breaks = seq(-1,1,by=0.025)
# out="test"

plot_assoc = function(df_func, width_height, list_breaks, out) {

    df_func = df_func %>%
        mutate(
            pvalfdr_males = p.adjust(pval_males, method="fdr"),
            pvalfdr_females = p.adjust(pval_females, method="fdr"),
            pvalfdr_inter = p.adjust(pval_inter, method="fdr")
        ) %>%
        # Remove effects when n<5000 (some fields of Cardiac and aortic function 2)
        filter(n_inter > 10000) %>%
        pivot_longer(cols=-c(Cog_test), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>% 
        mutate(Stat = factor(Stat), Type = factor(Type)) %>%
        pivot_wider(names_from=Stat, values_from=Value) %>%
        mutate(fdrsig = ifelse(pvalfdr<0.05, 1, 0))

    fwrite(df_func, paste0(out, ".tsv"), row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

    limits_plot = c(min(df_func %>% filter(Type %in% c("males", "females")) %>% pull(coef)), max(df_func %>% filter(Type %in% c("males", "females")) %>% pull(coef)))

    # Prepare dataset for males
    df_males = df_func %>% 
        filter(Type %in% c("males")) %>%
        arrange(coef) %>%
        mutate(
            Cog_test = reorder(Cog_test, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, scale_gender_dis[2], scale_gender_dis[1]))
        )

    label_colors_males <- df_males$color
    names(label_colors_males) <- df_males$Cog_test

    # Prepare dataset for females
    df_females = df_func %>% 
        filter(Type %in% c("females")) %>%
        arrange(coef) %>%
        mutate(
            Cog_test = reorder(Cog_test, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, scale_gender_dis[2], scale_gender_dis[1]))
        )

    label_colors_females <- df_females$color
    names(label_colors_females) <- df_females$Cog_test

    # Prepare dataset for interaction
    df_inter = df_func %>% 
        filter(Type %in% c("inter")) %>%
        arrange(coef) %>%
        mutate(
            Cog_test = reorder(Cog_test, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, "#1006f9", "#f90609"))
        )

    label_colors_inter <- df_inter$color
    names(label_colors_inter) <- df_inter$Cog_test

    # Plot for males
    plt_males = ggplot(df_males, aes(x = Cog_test, y = coef, fill = color)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_identity() +
        scale_x_discrete(name="") +
        scale_y_continuous(name="Standardized Beta", limits=limits_plot, breaks=list_breaks) +
        ggtitle("Males") +
        theme_light() +
        theme(axis.text.y = element_text(size = 9, color = label_colors_males),
            axis.text.x = element_text(size = 12),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12),
            plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
        )
    ggsave(paste0(out, "_males.png"), width=width_height[1], height=width_height[2])

    # Plot for females
    plt_females = ggplot(df_females, aes(x = Cog_test, y = coef, fill = color)) +
        geom_bar(stat = "identity") +
        scale_fill_identity() +
        scale_x_discrete(name="", position="top") +
        scale_y_continuous(name="Standardized Beta", limits=limits_plot, breaks=list_breaks) +
        coord_flip() +
        ggtitle("Females") +
        theme_light() +
        theme(axis.text.y = element_text(size = 9, color = label_colors_females, hjust = 1),
            axis.text.x = element_text(size = 12),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12),
            plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
        )
    ggsave(paste0(out, "_females.png"), width=width_height[1], height=width_height[2])

    # Plot for inter
    plt_inter = ggplot(df_inter, aes(x = Cog_test, y = coef, fill = color)) +
        geom_bar(stat = "identity") +
        scale_fill_identity() +
        scale_x_discrete(name="") +
        scale_y_continuous(name="Standardized Beta") +
        coord_flip() +
        ggtitle("Interaction") +
        theme_light() +
        theme(axis.text.y = element_text(size = 9, color = label_colors_inter, hjust = 1),
        axis.text.x = element_text(size = 12),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        plot.margin = margin(t = 20, r = 10, b = 10, l = 10) 
        )
    ggsave(paste0(out, "_inter.png"), width=width_height[1], height=width_height[2])

    plt_males + plt_females + plt_inter
    ggsave(paste0(out, ".png"), width=width_height[1]*3, height=width_height[2])
    print(paste0(out, ".png"))
}

# Function to plot blood associations (RANK)

# df_func = df_results
# width_height = c(2,5)
# list_exp = c(0.068, 0.072)
# out="test"

plot_assoc_rank = function(df_func, width_height, list_exp, out) {

    df_func = df_func %>%
        mutate(
            pvalfdr_males = p.adjust(pval_males, method="fdr"),
            pvalfdr_females = p.adjust(pval_females, method="fdr"),
            pvalfdr_inter = p.adjust(pval_inter, method="fdr")
        ) %>%
        # Remove effects when n<5000 (some fields of Cardiac and aortic function 2)
        filter(n_inter > 10000) %>%
        pivot_longer(cols=-c(Cog_test), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>% 
        mutate(Stat = factor(Stat), Type = factor(Type)) %>%
        pivot_wider(names_from=Stat, values_from=Value) %>%
        mutate(fdrsig = ifelse(pvalfdr<0.05, 1, 0))

    limits_plot = c(min(df_func %>% filter(Type %in% c("males", "females")) %>% pull(coef)), max(df_func %>% filter(Type %in% c("males", "females")) %>% pull(coef)))

    df_rank = df_func %>%
        group_by(Type) %>%
        mutate(rank_coef = rank(coef, na.last = "keep")) %>%
        ungroup() %>%
        pivot_wider(names_from=Type, values_from = c(n, coef, pval, pvalfdr, fdrsig, rank_coef)) %>%
        mutate(rank_median = apply(select(., rank_coef_females, rank_coef_males), 1, median)) %>%
        mutate(
            color_males = ifelse(fdrsig_males == 0, "#e0e2e2", ifelse(coef_males > 0, scale_gender_dis[2], scale_gender_dis[1])),
            color_females = ifelse(fdrsig_females == 0, "#e0e2e2", ifelse(coef_females > 0, scale_gender_dis[2], scale_gender_dis[1]))
        )

    df_first_half = df_rank %>%
        select(Cog_test, rank_coef_females, rank_median, color_females) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Cog_test)
    
    df_sec_half = df_rank %>%
        select(Cog_test, rank_median, rank_coef_males, color_males) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Cog_test)

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Cog_test), color="#e0e2e2", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Cog_test), color="#e0e2e2", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#e0e2e2"), aes(x=Rank_type, y=Rank, group=Cog_test, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#e0e2e2"), aes(x=Rank_type, y=Rank, group=Cog_test, color=color_males), linewidth=1.5) +
        scale_x_discrete(limits = c("rank_coef_males", "rank_median", "rank_coef_females"), labels = c("Male", "Median", "Female"), expand = c(0, 0)) +
        scale_y_continuous(expand = expansion(mult=list_exp)) +
        scale_color_identity() +
        theme_void() +
        theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.spacing = unit(0, "lines"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"))
    ggsave(paste0(out, "_all.png"), width=width_height[1], height=width_height[2])
    print(paste0(out, "_all.png"))

    # Plot only when interaction is significant 
    df_first_half = df_rank %>%
        select(Cog_test, rank_coef_females, rank_median, color_females, fdrsig_inter) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Cog_test) %>%
        mutate(color_females = ifelse(fdrsig_inter == 1, color_females, "#FFFFFF"))
    
    df_sec_half = df_rank %>%
        select(Cog_test, rank_median, rank_coef_males, color_males, fdrsig_inter) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Cog_test) %>%
        mutate(color_males = ifelse(fdrsig_inter == 1, color_males, "#FFFFFF"))

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Cog_test), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Cog_test), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Cog_test, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Cog_test, color=color_males), linewidth=1.5) +
        scale_x_discrete(limits = c("rank_coef_males", "rank_median", "rank_coef_females"), labels = c("Male", "Median", "Female"), expand = c(0, 0)) +
        scale_y_continuous(expand = expansion(mult=list_exp)) +
        scale_color_identity() +
        theme_void() +
        theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.spacing = unit(0, "lines"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"))
    ggsave(paste0(out, "_sig_inter.png"), width=width_height[1], height=width_height[2])
    print(paste0(out, "_sig_inter.png"))

    # Plot only when change direction
    df_first_half = df_rank %>%
        select(Cog_test, coef_females, coef_males, rank_coef_females, rank_median, color_females, fdrsig_inter) %>%
        mutate(Change_dir = ifelse(sign(coef_females) != sign(coef_males), "YES", "NO")) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Cog_test) %>%
        mutate(color_females = ifelse(Change_dir == "YES", color_females, "#FFFFFF"))
    
    df_sec_half = df_rank %>%
        select(Cog_test, coef_females, coef_males, rank_coef_males, rank_median, color_males, fdrsig_inter) %>%
        mutate(Change_dir = ifelse(sign(coef_females) != sign(coef_males), "YES", "NO")) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Cog_test) %>%
        mutate(color_males = ifelse(Change_dir == "YES", color_males, "#FFFFFF"))

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Cog_test), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Cog_test), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Cog_test, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Cog_test, color=color_males), linewidth=1.5) +
        scale_x_discrete(limits = c("rank_coef_males", "rank_median", "rank_coef_females"), labels = c("Male", "Median", "Female"), expand = c(0, 0)) +
        scale_y_continuous(expand = expansion(mult=list_exp)) +
        scale_color_identity() +
        theme_void() +
        theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "white", color = NA),
            panel.spacing = unit(0, "lines"),
            plot.margin = unit(c(0, 0, 0, 0), "cm"))
    ggsave(paste0(out, "_change_dir.png"), width=width_height[1], height=width_height[2])
    print(paste0(out, "_change_dir.png"))
}

# Function to combine graphs

# prefix = "./visualization/body_comp_dxa/body_comp_dxa"
# wdt_lines_bar = c(790, 1200)
# hgt = 3000
# off_lines_bar_males = c(75,860)
# off_lines_bar_females = c(1235,38)

combine_bar_rank = function(prefix, wdt_lines_bar, hgt, off_lines_bar_males, off_lines_bar_females) {

    # Separate labels and barplots
    crop = paste0(wdt_lines_bar[1],"x",hgt,"+",off_lines_bar_males[1],"+0")
    command = paste0("convert ",prefix,"_males.png -crop ",crop," ",prefix,"_males_labels.png")
    system(command)

    crop = paste0(wdt_lines_bar[2],"x",hgt,"+",off_lines_bar_males[2],"+0")
    command = paste0("convert ",prefix,"_males.png -crop ",crop," ",prefix,"_males_barplot.png")
    system(command)

    crop = paste0(wdt_lines_bar[1],"x",hgt,"+",off_lines_bar_females[1],"+0")
    command = paste0("convert ",prefix,"_females.png -crop ",crop," ",prefix,"_females_labels.png")
    system(command)

    crop = paste0(wdt_lines_bar[2],"x",hgt,"+",off_lines_bar_females[2],"+0")
    command = paste0("convert ",prefix,"_females.png -crop ",crop," ",prefix,"_females_barplot.png")
    system(command)

    # Concatenate plots
    command = paste0("convert -gravity Center ",prefix,"_males_barplot.png ",prefix,"_males_labels.png ",prefix,"_rank_all.png ",prefix,"_females_labels.png ",prefix,"_females_barplot.png +append ",prefix,"_bar_rank_all.png")
    system(command)
    print(paste0(prefix,"_bar_rank_all.png"))

    command = paste0("convert -gravity Center ",prefix,"_males_barplot.png ",prefix,"_males_labels.png ",prefix,"_rank_sig_inter.png ",prefix,"_females_labels.png ",prefix,"_females_barplot.png +append ",prefix,"_bar_rank_sig_inter.png")
    system(command)
    print(paste0(prefix,"_bar_rank_sig_inter.png"))

    command = paste0("convert -gravity Center ",prefix,"_males_barplot.png ",prefix,"_males_labels.png ",prefix,"_rank_change_dir.png ",prefix,"_females_labels.png ",prefix,"_females_barplot.png +append ",prefix,"_bar_rank_change_dir.png")
    system(command)
    print(paste0(prefix,"_bar_rank_change_dir.png"))
}

#endregion

#region Run

# All variables
dir.create("./visualization/all", showWarnings = FALSE)

plot_assoc(
    df_results,
    c(3,4.1),
    seq(-0.3,0.3,by=0.03),
    "./visualization/all/all"
)

plot_assoc_rank(
    df_results,
    c(1,4.1),
    c(0.26, 0.28),
    "./visualization/all/all_rank"
)

combine_bar_rank(
    "./visualization/all/all",
    wdt_lines_bar = c(335, 440),
    hgt = 3000,
    off_lines_bar_males = c(90,425),
    off_lines_bar_females = c(470,35)
)

#endregion


