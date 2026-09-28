
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)
library(scales)
library(RColorBrewer)
library(grid)
library(ggbump)
library(stringr)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

results = as.data.frame(fread("./results/results_mental_health.tsv"))
mental_var_mapping = as.data.frame(fread("../../../UKB/Analyses/clean_mental_health/variable_mapping.tsv"))

# Clean results
df_results = results %>%
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    rename(n_inter = "n_all") %>%
    # Remove variables used in gender score creation
    filter(!Mental_var %in% c(
        "Work_job_satisfaction_4537_0",
        "Health_satisfaction_4548_0",
        "Family_relationship_satisfaction_4559_0",
        "Friendships_satisfaction_4570_0",
        "Financial_situation_satisfaction_4581_0"
    )) %>%
    left_join(mental_var_mapping %>% rename(Mental_var = Variable) %>% select(-Category), by="Mental_var") %>%
    glimpse()

#endregion

#region Functions

# Function to plot associations (NO RANK)

# df_func = df_results
# width_height = c(5,9)
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
        pivot_longer(cols=-c(Category, Mental_var, Variable_clean), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>% 
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
            Variable_clean = reorder(Variable_clean, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, scale_gender_dis[2], scale_gender_dis[1]))
        )

    label_colors_males <- df_males$color
    names(label_colors_males) <- df_males$Variable_clean

    # Prepare dataset for females
    df_females = df_func %>% 
        filter(Type %in% c("females")) %>%
        arrange(coef) %>%
        mutate(
            Variable_clean = reorder(Variable_clean, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, scale_gender_dis[2], scale_gender_dis[1]))
        )

    label_colors_females <- df_females$color
    names(label_colors_females) <- df_females$Variable_clean

    # Prepare dataset for interaction
    df_inter = df_func %>% 
        filter(Type %in% c("inter")) %>%
        arrange(coef) %>%
        mutate(
            Variable_clean = reorder(Variable_clean, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, "#1006f9", "#f90609"))
        )

    label_colors_inter <- df_inter$color
    names(label_colors_inter) <- df_inter$Variable_clean

    # Plot for males
    plt_males = ggplot(df_males, aes(x = Variable_clean, y = coef, fill = color)) +
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
    plt_females = ggplot(df_females, aes(x = Variable_clean, y = coef, fill = color)) +
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
    plt_inter = ggplot(df_inter, aes(x = Variable_clean, y = coef, fill = color)) +
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
# width_height = c(2,9)
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
        pivot_longer(cols=-c(Category, Mental_var, Variable_clean), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>% 
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
        select(Mental_var, rank_coef_females, rank_median, color_females) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Mental_var)
    
    df_sec_half = df_rank %>%
        select(Mental_var, rank_median, rank_coef_males, color_males) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Mental_var)

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Mental_var), color="#e0e2e2", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Mental_var), color="#e0e2e2", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#e0e2e2"), aes(x=Rank_type, y=Rank, group=Mental_var, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#e0e2e2"), aes(x=Rank_type, y=Rank, group=Mental_var, color=color_males), linewidth=1.5) +
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
        select(Mental_var, rank_coef_females, rank_median, color_females, fdrsig_inter) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Mental_var) %>%
        mutate(color_females = ifelse(fdrsig_inter == 1, color_females, "#FFFFFF"))
    
    df_sec_half = df_rank %>%
        select(Mental_var, rank_median, rank_coef_males, color_males, fdrsig_inter) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Mental_var) %>%
        mutate(color_males = ifelse(fdrsig_inter == 1, color_males, "#FFFFFF"))

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Mental_var), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Mental_var), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Mental_var, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Mental_var, color=color_males), linewidth=1.5) +
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
        select(Mental_var, coef_females, coef_males, rank_coef_females, rank_median, color_females, fdrsig_inter) %>%
        mutate(Change_dir = ifelse(sign(coef_females) != sign(coef_males), "YES", "NO")) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Mental_var) %>%
        mutate(color_females = ifelse(Change_dir == "YES", color_females, "#FFFFFF"))
    
    df_sec_half = df_rank %>%
        select(Mental_var, coef_females, coef_males, rank_coef_males, rank_median, color_males, fdrsig_inter) %>%
        mutate(Change_dir = ifelse(sign(coef_females) != sign(coef_males), "YES", "NO")) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Mental_var) %>%
        mutate(color_males = ifelse(Change_dir == "YES", color_males, "#FFFFFF"))

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Mental_var), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Mental_var), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Mental_var, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Mental_var, color=color_males), linewidth=1.5) +
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

# All
dir.create("./visualization/clean_mental", showWarnings = FALSE)

df_clean = df_results %>% 
    filter(Category != "Life") %>%
    filter(!Mental_var %in% c(
        "Seen_GP_for_nerves_anxiety_tension_or_depression_2090_0",
        "Seen_a_psychiatrist_for_nerves_anxiety_tension_or_depression_2100_0",
        "Ever_depressed_for_a_whole_week_4598_0",
        "Longest_period_of_depression_4609_0",
        "Number_of_depression_episodes_4620_0",
        "Ever_unenthusiastic_disinterested_for_a_whole_week_4631_0",
        "Ever_manic_hyper_for_2_days_4642_0",
        "Ever_highly_irritable_argumentative_for_2_days_4653_0",
        "Longest_period_of_unenthusiasm_disinterest_5375_0",
        "Number_of_unenthusiastic_disinterested_episodes_5386_0",
        "Length_of_longest_manic_irritable_episode_5663_0",
        "Severity_of_manic_irritable_episodes_5674_0",
        "Manic_6156_all_symptoms",
        "Manic_6156_more_active",
        "Manic_6156_none",
        "Manic_6156_more_talkative",
        "Manic_6156_less_sleep",
        "Manic_6156_more_creative",
        "bipolar_depression_20126_none",
        "bipolar_depression_20126_depression_moderate",
        "bipolar_depression_20126_depression_severe",
        "bipolar_depression_20126_bipolar_1",
        "bipolar_depression_20126_single_depressive_episode",
        "bipolar_depression_20126_bipolar_2"
    ))

fwrite(as.data.frame(unique(df_clean$Mental_var)), "results/markers_paper.txt", col.names=FALSE)

plot_assoc(
    df_clean,
    c(4.5,4),
    c(-0.03, 0, 0.03, 0.06),
    "./visualization/clean_mental/clean_mental"
)

plot_assoc_rank(
    df_clean,
    c(1,4),
    c(0.21, 0.23),
    "./visualization/clean_mental/clean_mental_rank"
)

combine_bar_rank(
    "./visualization/clean_mental/clean_mental",
    wdt_lines_bar = c(700, 520),
    hgt = 3000,
    off_lines_bar_males = c(90,790),
    off_lines_bar_females = c(560,40)
)

#endregion
