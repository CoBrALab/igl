
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

blood_mapping = as.data.frame(fread("ukb_blood_mapping.csv"))

df_results = as.data.frame(fread("./results/lm_results.tsv"))

df_results = df_results %>%
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    rename(n_inter = "n_all") %>%
    mutate(
        Blood_marker = str_replace_all(Blood_marker, "_", " "),
        Blood_marker = factor(Blood_marker),
        pvalfdr_males = p.adjust(pval_males, method="fdr"),
        pvalfdr_females = p.adjust(pval_females, method="fdr"),
        pvalfdr_inter = p.adjust(pval_inter, method="fdr")
    ) %>% 
    mutate(
        pvalfdrsig_males = ifelse(pvalfdr_males < 0.05, pvalfdr_males, NA),
        pvalfdrsig_females = ifelse(pvalfdr_females < 0.05, pvalfdr_females, NA),
        pvalfdrsig_inter = ifelse(pvalfdr_inter < 0.05, pvalfdr_inter, NA),
        fdrsig_males = ifelse(pvalfdr_males < 0.05, 1, 0),
        fdrsig_females = ifelse(pvalfdr_females < 0.05, 1, 0),
        fdrsig_inter = ifelse(pvalfdr_inter < 0.05, 1, 0)
    ) %>% 
    pivot_longer(cols=-Blood_marker, names_sep="_", names_to = c("Stat", "Type"), values_to="Value") %>% 
    mutate(Stat = factor(Stat), Type = factor(Type)) %>%
    pivot_wider(names_from=Stat, values_from=Value) %>%
    left_join(blood_mapping %>% rename(Blood_marker = "name_clean") %>% select(Blood_marker, categ), by="Blood_marker") %>%
    mutate(categ = factor(categ)) %>%
    glimpse()

fwrite(df_results, "./results/lm_results_clean.tsv", row.names=FALSE, col.names=TRUE, quote=FALSE, sep='\t')

# Reload results
blood_mapping = as.data.frame(fread("ukb_blood_mapping.csv"))

df_results_wide = as.data.frame(fread("./results/lm_results.tsv"))

df_results_wide = df_results_wide %>%
    select(-c(coef_sexdiff, pval_sexdiff, coef_igl_female, coef_igl_male)) %>%
    rename(n_inter = "n_all") %>%
    mutate(Blood_marker = str_replace_all(Blood_marker, "_", " ")) %>%
    left_join(blood_mapping %>% rename(Blood_marker = "name_clean") %>% select(Blood_marker, categ, categ_group), by="Blood_marker") %>%
    mutate(
        Blood_marker = factor(Blood_marker),
        categ = factor(categ),
        categ_group = factor(categ_group)
    ) %>%
    glimpse()

#endregion

#region Plot functions

facet_spacing = unit(0.6, "lines")

prepare_blood_facet = function(df_func) {

    df_func %>%
        mutate(
            pvalfdr_males = p.adjust(pval_males, method="fdr"),
            pvalfdr_females = p.adjust(pval_females, method="fdr"),
            pvalfdr_inter = p.adjust(pval_inter, method="fdr")
        ) %>%
        mutate(
            fdrsig_males = ifelse(pvalfdr_males < 0.05, 1, 0),
            fdrsig_females = ifelse(pvalfdr_females < 0.05, 1, 0),
            fdrsig_inter = ifelse(pvalfdr_inter < 0.05, 1, 0),
            color_males = ifelse(fdrsig_males == 0, "#e0e2e2", ifelse(coef_males > 0, scale_gender_dis[2], scale_gender_dis[1])),
            color_females = ifelse(fdrsig_females == 0, "#e0e2e2", ifelse(coef_females > 0, scale_gender_dis[2], scale_gender_dis[1])),
            color_inter = ifelse(fdrsig_inter == 0, "#e0e2e2", ifelse(coef_inter > 0, "#1006f9", "#f90609"))
        ) %>%
        group_by(categ_group) %>%
        mutate(
            rank_males = rank(coef_males, ties.method="first", na.last="keep"),
            rank_females = rank(coef_females, ties.method="first", na.last="keep"),
            rank_inter = rank(coef_inter, ties.method="first", na.last="keep")
        ) %>%
        ungroup() %>%
        mutate(rank_median = (rank_males + rank_females) / 2)
}

facet_rank_rows = function(df, strip="right") {

    df_counts = df %>% count(categ_group)
    df_bounds = bind_rows(df_counts %>% mutate(y = 0), df_counts %>% mutate(y = n + 1))

    list(
        geom_blank(data=df_bounds, aes(y=y), inherit.aes=FALSE),
        scale_y_continuous(expand=c(0, 0), breaks=function(lim) seq(ceiling(lim[1] + 0.5), floor(lim[2] - 0.5)), minor_breaks=NULL),
        facet_grid(categ_group ~ ., scales="free_y", space="free_y", switch=if (strip == "left") "y" else NULL)
    )
}

plot_facet_bars = function(df, type, limits, list_breaks, title, strip="none") {

    col_coef = paste0("coef_", type)
    col_rank = paste0("rank_", type)
    col_color = paste0("color_", type)

    plt = ggplot(df, aes(x=.data[[col_coef]], y=.data[[col_rank]], fill=.data[[col_color]])) +
        geom_col(orientation="y", width=0.9) +
        scale_fill_identity() +
        scale_x_continuous(name="Standardized Beta", limits=limits, breaks=list_breaks) +
        facet_rank_rows(df, strip) +
        ggtitle(title) +
        theme_light() +
        theme(axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.ticks = element_blank(),
            plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
            strip.placement = "outside",
            strip.text.y = element_text(size = 11, face = "bold", color = "black"),
            strip.background.y = element_rect(fill = NA, color = NA),
            panel.spacing.y = facet_spacing,
            plot.margin = margin(t = 20, r = 5, b = 10, l = 5)
        )

    if (strip == "none") {
        plt = plt + theme(strip.text.y = element_blank(), strip.background.y = element_blank())
    }
    plt
}

plot_facet_labels = function(df, type, align="right") {

    col_rank = paste0("rank_", type)
    col_color = paste0("color_", type)

    df = df %>% mutate(x_label = ifelse(align == "right", 1, 0))

    ggplot(df, aes(x=x_label, y=.data[[col_rank]], label=Blood_marker, color=.data[[col_color]])) +
        geom_text(hjust=ifelse(align == "right", 1, 0), size=9/.pt) +
        scale_color_identity() +
        scale_x_continuous(limits=c(0, 1), expand=c(0, 0)) +
        facet_rank_rows(df) +
        coord_cartesian(clip="off") +
        theme_void() +
        theme(strip.text = element_blank(),
            strip.background = element_blank(),
            panel.spacing.y = facet_spacing,
            plot.margin = margin(t = 0, r = 5, b = 0, l = 5)
        )
}

plot_facet_rank = function(df, keep) {

    df_keep = df[keep, ]

    df_first_half = bind_rows(
        df_keep %>% transmute(categ_group, Blood_marker, color = color_females, x = 3, Rank = rank_females),
        df_keep %>% transmute(categ_group, Blood_marker, color = color_females, x = 2, Rank = rank_median)
    )

    df_sec_half = bind_rows(
        df_keep %>% transmute(categ_group, Blood_marker, color = color_males, x = 2, Rank = rank_median),
        df_keep %>% transmute(categ_group, Blood_marker, color = color_males, x = 1, Rank = rank_males)
    )

    ggplot() +
        geom_bump(data=df_first_half, aes(x=x, y=Rank, group=Blood_marker), color="#e0e2e2", linewidth=1.5) +
        geom_bump(data=df_sec_half, aes(x=x, y=Rank, group=Blood_marker), color="#e0e2e2", linewidth=1.5) +
        geom_bump(data=df_first_half %>% filter(color != "#e0e2e2"), aes(x=x, y=Rank, group=Blood_marker, color=color), linewidth=1.5) +
        geom_bump(data=df_sec_half %>% filter(color != "#e0e2e2"), aes(x=x, y=Rank, group=Blood_marker, color=color), linewidth=1.5) +
        scale_color_identity() +
        scale_x_continuous(limits=c(1, 3), expand=c(0, 0)) +
        facet_rank_rows(df) +
        theme_void() +
        theme(legend.position = "none",
            strip.text = element_blank(),
            strip.background = element_blank(),
            panel.spacing.y = facet_spacing,
            plot.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
        )
}

# df_func = df_results_wide
# width_height = c(14,11)
# list_breaks = seq(-1,1,by=0.02)
# out = "test"
# rel_widths = c(3, 2.4, 1.2, 2.4, 3) (male bars, male labels, rank, female labels, female bars)

plot_blood_assoc_facet = function(df_func, width_height, list_breaks, out, rel_widths=c(3, 2.4, 1.2, 2.4, 3), limits_plot=NULL, limits_inter=NULL) {

    df_facet = prepare_blood_facet(df_func)

    fwrite(df_facet, paste0(out, ".tsv"), row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")

    if (is.null(limits_plot)) {
        limits_plot = range(c(0, df_facet$coef_males, df_facet$coef_females), na.rm=TRUE)
    }
    if (is.null(limits_inter)) {
        limits_inter = range(c(0, df_facet$coef_inter), na.rm=TRUE)
    }

    plt_males_bars = plot_facet_bars(df_facet, "males", limits_plot, list_breaks, "Males", strip="left")
    plt_males_labels = plot_facet_labels(df_facet, "males", align="right")
    plt_females_labels = plot_facet_labels(df_facet, "females", align="left")
    plt_females_bars = plot_facet_bars(df_facet, "females", limits_plot, list_breaks, "Females", strip="right")

    list_keep = list(
        all = rep(TRUE, nrow(df_facet)),
        sig_inter = df_facet$fdrsig_inter == 1,
        change_dir = sign(df_facet$coef_females) != sign(df_facet$coef_males)
    )

    for (name_keep in names(list_keep)) {

        plt_rank = plot_facet_rank(df_facet, list_keep[[name_keep]] %in% TRUE)

        plt = plt_males_bars + plt_males_labels + plt_rank + plt_females_labels + plt_females_bars +
            plot_layout(nrow=1, widths=rel_widths)
        ggsave(paste0(out, "_", name_keep, ".png"), plt, width=width_height[1], height=width_height[2])
        print(paste0(out, "_", name_keep, ".png"))
    }

    plt_inter_labels = plot_facet_labels(df_facet, "inter", align="right")
    plt_inter_bars = plot_facet_bars(df_facet, "inter", limits_inter, waiver(), "Interaction", strip="right")

    widths_inter = c(rel_widths[2] + 0.5, rel_widths[1])
    plt = plt_inter_labels + plt_inter_bars + plot_layout(nrow=1, widths=widths_inter)
    ggsave(paste0(out, "_inter.png"), plt, width=width_height[1] * sum(widths_inter) / sum(rel_widths), height=width_height[2])
    print(paste0(out, "_inter.png"))
}

#endregion

#region Run (facets)

dir.create("./visualization/facet", showWarnings = FALSE)

plot_blood_assoc_facet(
    df_results_wide %>%
        mutate(categ_group = factor(categ_group, levels=c("Red cells", "Immune & platelets", "Cardiometabolic", "Organ & endocrine"))),
    c(13.5, 10),
    seq(-1,1,by=0.02),
    "./visualization/facet/blood_facet"
)

#endregion


