
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

df_results = as.data.frame(fread("./results/results_alltab.tsv"))

df_results = df_results %>%
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    rename(n_inter = "n_all") %>%
    mutate(
        categ_num = factor(categ_num),
        categ_name = factor(categ_name),
        Organ = factor(Organ)
    ) %>% 
    glimpse()

#endregion

#region Functions

# Function to plot associations (NO RANK)

# df_func = df_results %>% filter(categ_name %in% c("Body composition by DXA"))
# width_height = c(5,9)
# list_breaks = seq(-1,1,by=0.025)
# out="test"

plot_organ_assoc = function(df_func, width_height, list_breaks, out) {

    df_func = df_func %>%
        mutate(
            pvalfdr_males = p.adjust(pval_males, method="fdr"),
            pvalfdr_females = p.adjust(pval_females, method="fdr"),
            pvalfdr_inter = p.adjust(pval_inter, method="fdr")
        ) %>%
        # Remove effects when n<5000 (some fields of Cardiac and aortic function 2)
        filter(n_inter > 10000) %>%
        pivot_longer(cols=-c(categ_num, categ_name, Organ), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>% 
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
            Organ = reorder(Organ, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, scale_gender_dis[2], scale_gender_dis[1]))
        )

    label_colors_males <- df_males$color
    names(label_colors_males) <- df_males$Organ

    # Prepare dataset for females
    df_females = df_func %>% 
        filter(Type %in% c("females")) %>%
        arrange(coef) %>%
        mutate(
            Organ = reorder(Organ, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, scale_gender_dis[2], scale_gender_dis[1]))
        )

    label_colors_females <- df_females$color
    names(label_colors_females) <- df_females$Organ

    # Prepare dataset for interaction
    df_inter = df_func %>% 
        filter(Type %in% c("inter")) %>%
        arrange(coef) %>%
        mutate(
            Organ = reorder(Organ, coef),
            color = ifelse(fdrsig == 0, "#e0e2e2", ifelse(coef > 0, "#1006f9", "#f90609"))
        )

    label_colors_inter <- df_inter$color
    names(label_colors_inter) <- df_inter$Organ

    # Plot for males
    plt_males = ggplot(df_males, aes(x = Organ, y = coef, fill = color)) +
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
    plt_females = ggplot(df_females, aes(x = Organ, y = coef, fill = color)) +
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
    plt_inter = ggplot(df_inter, aes(x = Organ, y = coef, fill = color)) +
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

# df_func = df_results %>% filter(categ_name %in% c("Body composition by DXA"))
# width_height = c(2,9)
# list_exp = c(0.068, 0.072)
# out="test"

plot_organ_assoc_rank = function(df_func, width_height, list_exp, out) {

    df_func = df_func %>%
        mutate(
            pvalfdr_males = p.adjust(pval_males, method="fdr"),
            pvalfdr_females = p.adjust(pval_females, method="fdr"),
            pvalfdr_inter = p.adjust(pval_inter, method="fdr")
        ) %>%
        # Remove effects when n<5000 (some fields of Cardiac and aortic function 2)
        filter(n_inter > 10000) %>%
        pivot_longer(cols=-c(categ_num, categ_name, Organ), names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>% 
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
        select(Organ, rank_coef_females, rank_median, color_females) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Organ)
    
    df_sec_half = df_rank %>%
        select(Organ, rank_median, rank_coef_males, color_males) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Organ)

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Organ), color="#e0e2e2", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Organ), color="#e0e2e2", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#e0e2e2"), aes(x=Rank_type, y=Rank, group=Organ, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#e0e2e2"), aes(x=Rank_type, y=Rank, group=Organ, color=color_males), linewidth=1.5) +
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
        select(Organ, rank_coef_females, rank_median, color_females, fdrsig_inter) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Organ) %>%
        mutate(color_females = ifelse(fdrsig_inter == 1, color_females, "#FFFFFF"))
    
    df_sec_half = df_rank %>%
        select(Organ, rank_median, rank_coef_males, color_males, fdrsig_inter) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Organ) %>%
        mutate(color_males = ifelse(fdrsig_inter == 1, color_males, "#FFFFFF"))

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Organ), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Organ), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Organ, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Organ, color=color_males), linewidth=1.5) +
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
        select(Organ, coef_females, coef_males, rank_coef_females, rank_median, color_females, fdrsig_inter) %>%
        mutate(Change_dir = ifelse(sign(coef_females) != sign(coef_males), "YES", "NO")) %>%
        pivot_longer(cols=c(rank_coef_females, rank_median), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_coef_females", "rank_median"))) %>%
        arrange(Organ) %>%
        mutate(color_females = ifelse(Change_dir == "YES", color_females, "#FFFFFF"))
    
    df_sec_half = df_rank %>%
        select(Organ, coef_females, coef_males, rank_coef_males, rank_median, color_males, fdrsig_inter) %>%
        mutate(Change_dir = ifelse(sign(coef_females) != sign(coef_males), "YES", "NO")) %>%
        pivot_longer(cols=c(rank_median, rank_coef_males), names_to="Rank_type", values_to="Rank") %>%
        mutate(Rank_type = factor(Rank_type, levels=c("rank_median", "rank_coef_males"))) %>%
        arrange(Organ) %>%
        mutate(color_males = ifelse(Change_dir == "YES", color_males, "#FFFFFF"))

    # Plot
    ggplot() +
        geom_bump(data=df_first_half, aes(x=Rank_type, y=Rank, group=Organ), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=Rank_type, y=Rank, group=Organ), color="#FFFFFF", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color_females != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Organ, color=color_females), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color_males != "#FFFFFF"), aes(x=Rank_type, y=Rank, group=Organ, color=color_males), linewidth=1.5) +
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

# Body composition by DXA
dir.create("./visualization/body_comp_dxa", showWarnings = FALSE)

plot_organ_assoc(
    df_results %>% filter(categ_name %in% c("Body composition by DXA")),
    c(5,9),
    seq(-1,1,by=0.025),
    "./visualization/body_comp_dxa/body_comp_dxa"
)

fwrite(as.data.frame(unique(df_results %>% filter(categ_name %in% c("Body composition by DXA")) %>% select(Organ))), "results/markers_dxa.csv", col.names=TRUE)

plot_organ_assoc_rank(
    df_results %>% 
        filter(categ_name %in% c("Body composition by DXA")),
    c(2,9),
    c(0.073, 0.08),
    "./visualization/body_comp_dxa/body_comp_dxa_rank"
)

combine_bar_rank(
    "./visualization/body_comp_dxa/body_comp_dxa",
    wdt_lines_bar = c(620, 755),
    hgt = 3000,
    off_lines_bar_males = c(90,710),
    off_lines_bar_females = c(785,35)
)

# Kidney + liver + other organs
dir.create("./visualization/kidney_liver_organ", showWarnings = FALSE)

plot_organ_assoc(
    df_results %>% 
        filter(categ_name %in% c("Kidney", "Liver", "Abdominal organ composition")) %>%
        mutate(Organ = if_else(Organ == "Proton density fat fraction (PDFF)", "Liver PDFF (Perspectum)", Organ)),
    c(4.5,5),
    seq(-0.02,1,by=0.02),
    "./visualization/kidney_liver_organ/kidney_liver_organ"
)

fwrite(as.data.frame(unique(df_results %>% 
        filter(categ_name %in% c("Kidney", "Liver", "Abdominal organ composition")) %>%
        mutate(Organ = if_else(Organ == "Proton density fat fraction (PDFF)", "Liver PDFF (Perspectum)", Organ)) %>% select(Organ))), "results/markers_organ.csv", col.names=TRUE)

plot_organ_assoc_rank(
    df_results %>% 
        filter(categ_name %in% c("Kidney", "Liver", "Abdominal organ composition")) %>%
        mutate(Organ = if_else(Organ == "Proton density fat fraction (PDFF)", "Liver PDFF (Perspectum)", Organ)),
    c(1.5,5),
    c(0.165, 0.178),
    "./visualization/kidney_liver_organ/kidney_liver_organ_rank"
)

combine_bar_rank(
    "./visualization/kidney_liver_organ/kidney_liver_organ",
    wdt_lines_bar = c(555, 660),
    hgt = 3000,
    off_lines_bar_males = c(95,653),
    off_lines_bar_females = c(700,38)
)

# Cardiac and aortic function 1
dir.create("./visualization/cardiac_1", showWarnings = FALSE)

plot_organ_assoc(
    df_results %>% filter(categ_name %in% c("Cardiac and aortic function 1")),
    c(5,12),
    seq(0,1,by=0.02),
    "./visualization/cardiac_1/cardiac_1"
)

fwrite(as.data.frame(unique(df_results %>% filter(categ_name %in% c("Cardiac and aortic function 1")) %>% select(Organ))), "results/markers_cardiac.csv", col.names=TRUE)

plot_organ_assoc_rank(
    df_results %>% filter(categ_name %in% c("Cardiac and aortic function 1")),
    c(2,12),
    c(0.055, 0.059),
    "./visualization/cardiac_1/cardiac_1_rank"
)

combine_bar_rank(
    "./visualization/cardiac_1/cardiac_1",
    wdt_lines_bar = c(720, 640),
    hgt = 4000,
    off_lines_bar_males = c(95,820),
    off_lines_bar_females = c(685,40)
)

#endregion

