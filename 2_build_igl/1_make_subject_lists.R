
library(data.table)
library(tidyverse)
library(dplyr)
library(MatchIt)
library(viridis)
library(ggbreak)
library(scales)
library(patchwork)

# Color scales
scale_female_male_dis = c(Female="#4d0084", Male="#097800")
scale_gender_cont = colorRampPalette(c("#A754FF","#FFFFFF","#4DAD70"))(100)
scale_gender_dis = c(Female="#A754FF", Male="#4DAD70")
gender_neutral_cont = colorRampPalette(c("#C7C7C7", "#333333"))
gender_neutral_single = "#333333"
gender_neutral_single_light="#d6e7d6"
gender_neutral_timepoints = c(tp0="#000000", tp1="#494949", tp2="#797979", tp3="#AAAAAA")

#region Load data

df_raw = list()
for (i in 0:3) {
    df_raw[[i+1]] = as.data.frame(fread(paste0("../impute_input/results/df_imputed_tp_",i,"_combined.tsv")))
}
df_raw = do.call(rbind, df_raw)

df_ethnicity = as.data.frame(fread("../../../UKB/tabular/df_ethnicity/UKBB_ethnicity_wide.tsv"))
df_ethnicity = df_ethnicity %>%
    rename(ID = "SubjectID", Ethnic_background_21000 = "Ethnic background_21000") %>%
    filter(InstanceID == 0) %>%
    mutate(eth_categ = factor(case_when(
        Ethnic_background_21000 %in% c("Black or Black British", "Caribbean", "African","Any other Black background")
            ~ "Black",
        Ethnic_background_21000 %in% c("Asian or Asian British", "Indian", "Pakistani", "Bangladeshi", "Chinese", "Any other Asian background") 
            ~ "Asian",
        Ethnic_background_21000 %in% c("White", "British", "Irish", "Any other white background")
            ~ "White",
        Ethnic_background_21000 %in% c("White and Black Caribbean", "White and Black African", "White and Asian", "Any other mixed background")
            ~ "Mixed",
        Ethnic_background_21000 %in% c("Other ethnic group")
            ~ "Other"
    ))) %>%
    glimpse()

df_birth = as.data.frame(fread("../../../UKB/tabular/df_firstocc_all_ses0/UKBB_firstocc_all_ses0_wide.tsv"))
df_birth = df_birth %>%
    rename(ID = "SubjectID", year_birth = "Year of birth_34") %>%
    mutate(generation = factor(case_when(
        between(year_birth, 1928, 1945) ~ "Silent",
        between(year_birth, 1946, 1954) ~ "Boom 1",
        between(year_birth, 1955, 1964) ~ "Boom 2",
        between(year_birth, 1965, 1980) ~ "Gen X"
    ), levels=c("Silent", "Boom 1", "Boom 2", "Gen X"))) %>%
    filter(InstanceID == 0) %>%
    glimpse()

df_income = as.data.frame(fread("../../../UKB/tabular/df_lifestyle/UKBB_lifestyle_wider.tsv"))
df_income = df_income %>%
    rename(ID = "SubjectID", household_income = "Average_total_household_income_before_tax_738_0", instanceID = "InstanceID") %>%
    select(ID, instanceID, household_income) %>%
    mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
    mutate(household_income = factor(household_income, levels = c("Less than 18,000", "18,000 to 30,999", "31,000 to 51,999", "52,000 to 100,000", "Greater than 100,000"))) %>%
    filter(complete.cases(.)) %>%
    glimpse()

df_demo = df_raw %>%
    select(ID, instanceID, age_21003, sex_31, genetic_sex_22001, sex_aneuploidy) %>%
    left_join(df_ethnicity %>% select(ID, eth_categ, Ethnic_background_21000), by="ID") %>%
    left_join(df_income, by=c("ID", "instanceID")) %>%
    left_join(df_birth %>% select(ID, year_birth, generation), by=c("ID")) %>%
    mutate(genetic_sex_22001 = factor(genetic_sex_22001, levels=c("Female", "Male"))) %>%
    glimpse()

#endregion

#region Describe demographics of groups

# list_ids = id_list
# timepoint = 0
# name = "all_tp0"

describe_demo = function(list_ids, timepoint, name) {

    # Filter IDs and timepoint
    df_tmp = df_demo %>%
        filter(ID %in% list_ids$ID, instanceID == timepoint)

    # Summarize demographics in table
    df_summary <- bind_rows(
        df_tmp %>%
            summarise(
            variable = "Age",
            level = "Mean ± SD [range]",
            value = sprintf(
                "%.1f (%.1f) [%.1f–%.1f]",
                mean(age_21003, na.rm = TRUE),
                sd(age_21003, na.rm = TRUE),
                min(age_21003, na.rm = TRUE),
                max(age_21003, na.rm = TRUE)
            )
            ),
        df_tmp %>%
            count(variable = "Ethnicity", level = eth_categ) %>%
            mutate(value = sprintf("%d (%.1f%%)", n, n / sum(n) * 100)) %>%
            select(-n),
        df_tmp %>%
            count(variable = "Household income", level = household_income) %>%
            mutate(value = sprintf("%d (%.1f%%)", n, n / sum(n) * 100)) %>%
            select(-n)
    )

    fwrite(df_summary, paste0("./results/",name,"/demo_table.tsv"), row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
    print(paste0("./results/",name,"/demo_table.tsv"))

    # Plot demo across sex
    common_theme <- theme_light() +
    theme(
        text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
        strip.background = element_rect(fill = "#d4d4d2"),
        strip.text = element_text(color = "black"),
        axis.text.x = element_text(hjust = 1, angle = 45),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)  # same margin
    )

    plt_age = ggplot(df_tmp, aes(x=age_21003, color = genetic_sex_22001)) +
        geom_density(linewidth=2) + 
        scale_y_continuous(name="Density") + 
        scale_x_continuous(name=paste0("Age\n",nrow(df_tmp))) +
        scale_color_manual(values=scale_female_male_dis) +
        ggtitle("Age") +
        common_theme + 
        theme(axis.text.x = element_text(hjust = 0.5, angle = 0))
    ggsave(paste0("./visualization/",name, "/demo_age.png"), height=7, width=7)

    scale_eth_categ = viridis(option="mako", n=5, begin=0.2, end=1)
    color_eth_categ = c(Asian = scale_eth_categ[1], Black = scale_eth_categ[2], Mixed = scale_eth_categ[3], Other = scale_eth_categ[4], White = scale_eth_categ[5])
    plt_eth_categ = ggplot(df_tmp, aes(x = genetic_sex_22001, fill = eth_categ)) +
        geom_bar(position = "fill") +
        scale_y_break(c(0.05, 0.9), scales=3) +
        scale_y_continuous(name = "", labels = scales::percent_format(), breaks = c(0, 0.05, 0.9, 0.95, 1), sec.axis = dup_axis(labels = NULL)) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_manual(values=color_eth_categ) +
        ggtitle("Ethnicity") +
        common_theme + 
        coord_cartesian(clip = "off")
    ggsave(paste0("./visualization/",name, "/demo_eth_categ.png"), height=7, width=7)

    list_eth = c(
        "British",
        "Indian", "Pakistani", "Chinese",
        "African", "Caribbean"
    )
    scale_eth = viridis(option="turbo", n=length(list_eth)+1, begin=0.1, end=0.9)
    color_eth = c(
        "British" = scale_eth[1],
        "Indian" = scale_eth[2], "Pakistani" = scale_eth[3], "Chinese" = scale_eth[4],
        "African" = scale_eth[5], "Caribbean" = scale_eth[6], 
        "Other" = scale_eth[7]
    )
    plt_eth = ggplot(df_tmp, aes(x = genetic_sex_22001, fill = Ethnic_background_21000)) +
        geom_bar(position = "fill") +
        scale_y_continuous(name = "", labels = scales::percent_format()) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_manual(values=color_eth) +
        ggtitle("Ethnicity") +
        common_theme + 
        coord_cartesian(clip = "off")
    ggsave(paste0("./visualization/",name, "/demo_eth.png"), height=7, width=7)

    plt_income = ggplot(df_tmp, aes(x = genetic_sex_22001, fill = household_income)) +
        geom_bar(position = "fill") +
        scale_y_continuous(name = "", labels = scales::percent_format()) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_viridis_d(option="magma", begin = 0, end=1, na.value = "grey70") + 
        ggtitle("Income") +
        common_theme
    ggsave(paste0("./visualization/",name, "/demo_income.png"), height=7, width=7)

    command=paste0("convert -gravity Center ./visualization/",name,"/demo_age.png ./visualization/",name,"/demo_eth_categ.png ./visualization/",name, "/demo_income.png +append ./visualization/",name, "/demo_all.png")
    system(command)

    print(paste0("./visualization/",name, "/demo_all.png"))

}

# list_list_ids = id_lists
# list_tp = c(0,0,0)
# list_names = c("white_matched", "black_matched", "asian_matched")
# list_names_clean = c("White", "Black", "Asian")
# name="test"

compare_demo = function(list_list_ids, list_tp, list_names, list_names_clean, name, age_sm_adj = 1) {

    list_eth = c(
        "British",
        "Indian", "Pakistani", "Chinese",
        "African", "Caribbean")

    df_tmp = list()
    for (d in 1:length(list_tp)) {
        df_tmp[[d]] = df_demo %>%
            filter(ID %in% list_list_ids[[d]]$ID, instanceID == list_tp[d]) %>%
            mutate(Run = list_names[d])
    }
    df_tmp = do.call(rbind, df_tmp)
    df_tmp = df_tmp %>% 
        mutate(
            Run = factor(Run, levels = list_names, labels = list_names_clean),
            eth_categ = factor(eth_categ, levels = c(NA, "Asian", "Black", "Mixed", "Other", "White")),
            # For ethnic background, map all categories with n>1% to "Other"
            Ethnic_background_21000 = fct_other(Ethnic_background_21000, keep = list_eth, other_level = "Other")
        ) %>%
        mutate(
            Ethnic_background_21000 = factor(Ethnic_background_21000, levels=c(
                "British", 
                "Indian", "Pakistani", "Chinese",
                "African", "Caribbean",
                "Other"
            ))
        )

    common_theme <- theme_light() +
    theme(
        text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
        strip.background = element_rect(fill = "#d4d4d2"),
        strip.text = element_text(color = "black"),
        axis.text.x = element_text(hjust = 1, angle = 45),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)  # same margin
    )

    plt_age = ggplot(df_tmp, aes(x=age_21003, color = Run)) +
        geom_density(linewidth=2, adjust=age_sm_adj) + 
        scale_y_continuous(name="Density") + 
        scale_x_continuous(name="Age") +
        scale_color_viridis_d(option="viridis", begin = 0, end=0.9) + 
        ggtitle("Age") +
        common_theme + 
        theme(axis.text.x = element_text(hjust = 0.5, angle = 0))
    ggsave(paste0("./visualization/",name, "/demo_age.png"), height=7, width=7)

    scale_eth_categ = viridis(option="mako", n=5, begin=0.2, end=1)
    color_eth_categ = c(Asian = scale_eth_categ[1], Black = scale_eth_categ[2], Mixed = scale_eth_categ[3], Other = scale_eth_categ[4], White = scale_eth_categ[5])
    plt_eth_categ = ggplot(df_tmp, aes(x = Run, fill = eth_categ)) +
        geom_bar(position = "fill") +
        scale_y_break(c(0.05, 0.9), scales=3) +
        scale_y_continuous(name = "", labels = scales::percent_format(), breaks = c(0, 0.05, 0.9, 0.95, 1), sec.axis = dup_axis(labels = NULL)) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_manual(values=color_eth_categ) +
        ggtitle("Ethnicity") +
        common_theme + 
        coord_cartesian(clip = "off")
    ggsave(paste0("./visualization/",name, "/demo_eth_categ.png"), height=7, width=7)

    scale_eth = viridis(option="turbo", n=length(list_eth)+1, begin=0.1, end=0.9)
    color_eth = c(
        "British" = scale_eth[1],
        "Indian" = scale_eth[2], "Pakistani" = scale_eth[3], "Chinese" = scale_eth[4],
        "African" = scale_eth[5], "Caribbean" = scale_eth[6], 
        "Other" = scale_eth[7]
    )
    plt_eth = ggplot(df_tmp, aes(x = Run, fill = Ethnic_background_21000)) +
        geom_bar(position = "fill") +
        scale_y_continuous(name = "", labels = scales::percent_format()) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_manual(values=color_eth) +
        ggtitle("Ethnicity") +
        common_theme + 
        coord_cartesian(clip = "off")
    ggsave(paste0("./visualization/",name, "/demo_eth.png"), height=7, width=7)

    plt_income = ggplot(df_tmp, aes(x = Run, fill = household_income)) +
        geom_bar(position = "fill") +
        scale_y_continuous(name = "", labels = scales::percent_format()) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_viridis_d(option="magma", begin = 0, end=1, na.value = "grey70") + 
        ggtitle("Income") +
        common_theme
    ggsave(paste0("./visualization/",name, "/demo_income.png"), height=7, width=7)

    command=paste0("convert -gravity Center ./visualization/",name,"/demo_age.png ./visualization/",name,"/demo_eth_categ.png ./visualization/",name, "/demo_income.png +append ./visualization/",name, "/demo_all.png")
    system(command)

    print(paste0("./visualization/",name, "/demo_all.png"))
}

# list_list_ids = id_lists
# list_tp = c(0,0,0)
# list_names = c("white_matched", "black_matched", "asian_matched")
# list_names_clean = c("White", "Black", "Asian")
# name="test"

compare_demo_sex = function(list_list_ids, list_tp, list_names, list_names_clean, name, age_sm_adj = 1) {

    list_eth = c(
        "British",
        "Indian", "Pakistani", "Chinese",
        "African", "Caribbean")

    df_tmp = list()
    for (d in 1:length(list_tp)) {
        df_tmp[[d]] = df_demo %>%
            filter(ID %in% list_list_ids[[d]]$ID, instanceID == list_tp[d]) %>%
            mutate(Run = list_names[d])
    }
    df_tmp = do.call(rbind, df_tmp)
    df_tmp = df_tmp %>% 
        mutate(
            Run = factor(Run, levels = list_names, labels = list_names_clean),
            eth_categ = factor(eth_categ, levels = c(NA, "Asian", "Black", "Mixed", "Other", "White")),
            # For ethnic background, map all categories with n>1% to "Other"
            Ethnic_background_21000 = fct_other(Ethnic_background_21000, keep = list_eth, other_level = "Other")
        ) %>%
        mutate(
            Ethnic_background_21000 = factor(Ethnic_background_21000, levels=c(
                "British", 
                "Indian", "Pakistani", "Chinese",
                "African", "Caribbean",
                "Other"
            ))
        ) %>%
        mutate(
            Run_sex = factor(paste0(Run, " - ", genetic_sex_22001))
        )

    common_theme <- theme_light() +
    theme(
        text = element_text(size = 25),
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
        strip.background = element_rect(fill = "#d4d4d2"),
        strip.text = element_text(color = "black"),
        axis.text.x = element_text(hjust = 1, angle = 45),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10)  # same margin
    )

    plt_age = ggplot(df_tmp, aes(x=age_21003, color = Run, linetype=genetic_sex_22001)) +
        geom_density(linewidth=2, adjust=age_sm_adj) + 
        scale_y_continuous(name="Density") + 
        scale_x_continuous(name="Age") +
        scale_color_viridis_d(option="viridis", begin = 0, end=0.9) + 
        ggtitle("Age") +
        common_theme + 
        theme(axis.text.x = element_text(hjust = 0.5, angle = 0))
    ggsave(paste0("./visualization/",name, "/demo_age_sex.png"), height=7, width=7)

    scale_eth_categ = viridis(option="mako", n=5, begin=0.2, end=1)
    color_eth_categ = c(Asian = scale_eth_categ[1], Black = scale_eth_categ[2], Mixed = scale_eth_categ[3], Other = scale_eth_categ[4], White = scale_eth_categ[5])
    plt_eth_categ = ggplot(df_tmp, aes(x = Run_sex, fill = eth_categ)) +
        geom_bar(position = "fill") +
        scale_y_break(c(0.05, 0.9), scales=3) +
        scale_y_continuous(name = "", labels = scales::percent_format(), breaks = c(0, 0.05, 0.9, 0.95, 1), sec.axis = dup_axis(labels = NULL)) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_manual(values=color_eth_categ) +
        ggtitle("Ethnicity") +
        common_theme + 
        coord_cartesian(clip = "off")
    ggsave(paste0("./visualization/",name, "/demo_eth_categ_sex.png"), height=7, width=7)

    scale_eth = viridis(option="turbo", n=length(list_eth)+1, begin=0.1, end=0.9)
    color_eth = c(
        "British" = scale_eth[1],
        "Indian" = scale_eth[2], "Pakistani" = scale_eth[3], "Chinese" = scale_eth[4],
        "African" = scale_eth[5], "Caribbean" = scale_eth[6], 
        "Other" = scale_eth[7]
    )
    plt_eth = ggplot(df_tmp, aes(x = Run_sex, fill = Ethnic_background_21000)) +
        geom_bar(position = "fill") +
        scale_y_continuous(name = "", labels = scales::percent_format()) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_manual(values=color_eth) +
        ggtitle("Ethnicity") +
        common_theme + 
        coord_cartesian(clip = "off")
    ggsave(paste0("./visualization/",name, "/demo_eth_sex.png"), height=7, width=7)

    plt_income = ggplot(df_tmp, aes(x = Run_sex, fill = household_income)) +
        geom_bar(position = "fill") +
        scale_y_continuous(name = "", labels = scales::percent_format()) +
        scale_x_discrete(name="") +
        labs(x=NULL) +
        scale_fill_viridis_d(option="magma", begin = 0, end=1, na.value = "grey70") + 
        ggtitle("Income") +
        common_theme
    ggsave(paste0("./visualization/",name, "/demo_income_sex.png"), height=7, width=7)

    command=paste0("convert -gravity Center ./visualization/",name,"/demo_age_sex.png ./visualization/",name,"/demo_eth_categ_sex.png ./visualization/",name, "/demo_income_sex.png +append ./visualization/",name, "/demo_all_sex.png")
    system(command)

    print(paste0("./visualization/",name, "/demo_all_sex.png"))
}

# df = df_combine
# group_var = "instanceID"
# name = "all_compare"

describe_demo_across_groups = function(df, group_var, name) {

    # Filter IDs and timepoint
    df_tmp = df %>%
        left_join(df_raw %>% select(ID, instanceID, Date_of_attending_assessment_centre_53_0) %>% mutate(instanceID = factor(instanceID)), by=c("ID", "instanceID")) %>%
        mutate(year_attending = year(Date_of_attending_assessment_centre_53_0)) %>%
        select(-Date_of_attending_assessment_centre_53_0) %>%
        mutate(group = .data[[group_var]]) %>%
        group_by(group)

    # Summarize demographics
    df_summary <- bind_rows(
        # N
        df_tmp %>%
            count(group) %>%
            mutate(
                variable = "N",
                level    = "Count",
                value    = as.character(n)
            ) %>%
            select(group, variable, level, value),
        df_tmp %>%
            summarise(
            variable = "Year acquired",
            level    = "Range (min–max)",
            value    = sprintf(
                "%d–%d",
                min(year_attending, na.rm = TRUE),
                max(year_attending, na.rm = TRUE)
            )
            ),
        # Age
        df_tmp %>%
            summarise(
            variable = "Age",
            level = "Mean ± SD",
            value = sprintf(
                    "%.1f (%.1f)",
                    mean(age_21003, na.rm = TRUE),
                    sd(age_21003, na.rm = TRUE)
                )
            ),
        # Ethnicity
        df_tmp %>%
            count(variable = "Ethnicity", level = eth_categ) %>%
            mutate(value = sprintf("%d (%.1f%%)", n, n / sum(n) * 100)) %>%
            select(-n),
        # Household income
        df_tmp %>%
            count(variable = "Household income", level = household_income) %>%
            mutate(value = sprintf("%d (%.1f%%)", n, n / sum(n) * 100)) %>%
            select(-n)
    ) %>%
    pivot_wider(names_from = group, values_from = value)

    fwrite(df_summary, paste0("./visualization/",name,"/demo_table.tsv"), row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
    print(paste0("./visualization/",name,"/demo_table.tsv"))
}

#endregion

#region IDs: by timepoints

print("By timepoints")

name_ana = c("all_tp0", "all_tp1", "all_tp2", "all_tp3")
id_lists = list()

for (tp in 1:4) {

    dir.create(paste0("./results/",name_ana[tp]), showWarnings=FALSE)
    dir.create(paste0("./visualization/",name_ana[tp]), showWarnings=FALSE)

    # Remove subjects with sex aneuploidy or incongruent genetic and self-report sex from training
    df_tomatch = df_demo %>%
        filter(complete.cases(.)) %>%
        filter(instanceID == tp-1) %>%
        filter(sex_aneuploidy == 0) %>%
        filter(genetic_sex_22001 == sex_31)

    # Match males and females
    df_match = match.data(matchit(
        genetic_sex_22001 ~ age_21003 + eth_categ,
        data = df_tomatch,
        method = "nearest",
        exact=c("age_21003", "eth_categ"),
        distance = "glm",
        m.order = "random",
        ratio = 1,
        replace = FALSE,
        verbose = FALSE
    )) %>%
    select(-c(distance, weights, subclass))

    id_list = data.frame("ID" = df_match %>% pull(ID))
    fwrite(id_list, paste0("./results/",name_ana[tp],"/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

    id_list_all = data.frame("ID" = df_demo %>% filter(instanceID == tp-1) %>% pull(ID))
    fwrite(id_list_all, paste0("./results/",name_ana[tp],"/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")

    describe_demo(id_list, tp-1, name_ana[tp])

    id_lists[[tp]] = id_list
}

dir.create(paste0("./results/all_compare"), showWarnings=FALSE)
dir.create(paste0("./visualization/all_compare"), showWarnings=FALSE)
compare_demo(
    id_lists,
    c(0,1,2,3),
    c("all_tp0", "all_tp1", "all_tp2", "all_tp3"),
    c("TP0", "TP1", "TP2", "TP3"),
    "all_compare"
)
compare_demo_sex(
    id_lists,
    c(0,1,2,3),
    c("all_tp0", "all_tp1", "all_tp2", "all_tp3"),
    c("TP0", "TP1", "TP2", "TP3"),
    "all_compare"
)

# Table of demographics
df_combine = df_demo %>%
    filter(ID %in% do.call(rbind, id_lists)$ID) %>%
    mutate(instanceID = factor(instanceID, levels=c(0,1,2,3)))

describe_demo_across_groups(df_combine, "instanceID", "all_compare")

#endregion

#region IDs: by timepoints (matched age, ethnicity)

print("By timepoints (matched)")

df_tomatch = df_demo %>%
    filter(complete.cases(.)) %>%
    filter(sex_aneuploidy == 0) %>%
    filter(genetic_sex_22001 == sex_31) %>%
    mutate(instanceID = factor(instanceID, levels = c(0, 1, 2, 3)))

# Helper: match target to ref groups
match_to_ref_tp <- function(target_pool, ref_pool,
                             exact_vars = NULL) {
    combined <- bind_rows(
        ref_pool    %>% mutate(is_ref = 1L),
        target_pool %>% mutate(is_ref = 0L)
    )
    fit <- matchit(
        is_ref ~ age_21003 + eth_categ,
        data     = combined,
        exact    = exact_vars,
        method   = "nearest",
        distance = "glm",
        ratio    = 1,
        replace  = FALSE,
        verbose  = FALSE
    )
    match.data(fit, drop.unmatched = TRUE) %>%
        select(-c(distance, weights, subclass, is_ref))
}

# 1. Balance males and females within TP3
fit_tp3 <- matchit(
    genetic_sex_22001 == "Male" ~ age_21003 + eth_categ,
    data     = df_tomatch %>% filter(instanceID == 3),
    exact    = c("age_21003", "eth_categ"),
    method   = "nearest",
    distance = "glm",
    m.order  = "random",
    ratio    = 1,
    replace  = FALSE,
    verbose  = FALSE
)
df_tp3 <- match.data(fit_tp3, drop.unmatched = TRUE) %>%
    select(-c(distance, weights, subclass))
tp3_m <- df_tp3 %>% filter(genetic_sex_22001 == "Male")
tp3_f <- df_tp3 %>% filter(genetic_sex_22001 == "Female")

# 2. Match each timepoint × sex group to TP3 reference
df_tp0_m <- match_to_ref_tp(df_tomatch %>% filter(instanceID == 0, genetic_sex_22001 == "Male"),   tp3_m, exact_vars = c("age_21003"))
df_tp0_f <- match_to_ref_tp(df_tomatch %>% filter(instanceID == 0, genetic_sex_22001 == "Female"), tp3_f, exact_vars = c("age_21003"))
df_tp1_m <- match_to_ref_tp(df_tomatch %>% filter(instanceID == 1, genetic_sex_22001 == "Male"),   df_tp0_m %>% filter(instanceID == 3))
df_tp1_f <- match_to_ref_tp(df_tomatch %>% filter(instanceID == 1, genetic_sex_22001 == "Female"), df_tp0_f %>% filter(instanceID == 3))
df_tp2_m <- match_to_ref_tp(df_tomatch %>% filter(instanceID == 2, genetic_sex_22001 == "Male"),   df_tp1_m %>% filter(instanceID == 3))
df_tp2_f <- match_to_ref_tp(df_tomatch %>% filter(instanceID == 2, genetic_sex_22001 == "Female"), df_tp1_f %>% filter(instanceID == 3))

# Combine and verify
df_match <- bind_rows(
    df_tp0_m %>% filter(instanceID == 0),
    df_tp0_f %>% filter(instanceID == 0),
    df_tp1_m %>% filter(instanceID == 1),
    df_tp1_f %>% filter(instanceID == 1),
    df_tp2_m %>% filter(instanceID == 2),
    df_tp2_f %>% filter(instanceID == 2),
    df_tp0_m %>% filter(instanceID == 3),
    df_tp0_f %>% filter(instanceID == 3)
)

# Graphs of demographics
id_lists = list(
    df_match %>% filter(instanceID == 0) %>% select(ID),
    df_match %>% filter(instanceID == 1) %>% select(ID),
    df_match %>% filter(instanceID == 2) %>% select(ID),
    df_match %>% filter(instanceID == 3) %>% select(ID)
)
dir.create(paste0("./results/all_compare_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/all_compare_match"), showWarnings=FALSE)
compare_demo(
    id_lists,
    c(0,1,2,3),
    c("tp0_matched", "tp1_matched", "tp2_matched", "tp3_matched"),
    c("TP0", "TP1", "TP2", "TP3"),
    "all_compare_match"
)
compare_demo_sex(
    id_lists,
    c(0,1,2,3),
    c("tp0_matched", "tp1_matched", "tp2_matched", "tp3_matched"),
    c("TP0", "TP1", "TP2", "TP3"),
    "all_compare_match"
)

# Table of demographics
describe_demo_across_groups(df_match, "instanceID", "all_compare_match")

# Write ID lists

# TP0 matched
dir.create(paste0("./results/all_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/all_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(instanceID == 0) %>% select(ID), paste0("./results/all_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_demo %>% filter(instanceID == 0) %>% select(ID), paste0("./results/all_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(instanceID == 0) %>% select(ID), 0, "all_tp0_match")

# TP1 matched
dir.create(paste0("./results/all_tp1_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/all_tp1_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(instanceID == 1) %>% select(ID), paste0("./results/all_tp1_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_demo %>% filter(instanceID == 1) %>% select(ID), paste0("./results/all_tp1_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(instanceID == 1) %>% select(ID), 1, "all_tp1_match")

# TP2 matched
dir.create(paste0("./results/all_tp2_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/all_tp2_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(instanceID == 2) %>% select(ID), paste0("./results/all_tp2_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_demo %>% filter(instanceID == 2) %>% select(ID), paste0("./results/all_tp2_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(instanceID == 2) %>% select(ID), 2, "all_tp2_match")

# TP3 matched
dir.create(paste0("./results/all_tp3_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/all_tp3_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(instanceID == 3) %>% select(ID), paste0("./results/all_tp3_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_demo %>% filter(instanceID == 3) %>% select(ID), paste0("./results/all_tp3_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(instanceID == 3) %>% select(ID), 3, "all_tp3_match")

#endregion

#region IDs: by ethnicity (matched age and income)

print("By ethnicity (matched)")

df_tomatch = df_demo %>%
    filter(eth_categ %in% c("White", "Asian", "Black")) %>%
    filter(complete.cases(.)) %>%
    filter(instanceID == 0) %>%
    filter(sex_aneuploidy == 0) %>%
    filter(genetic_sex_22001 == sex_31) %>%
    mutate(
        eth_categ        = factor(eth_categ, levels = c("White", "Asian", "Black")),
        household_income = factor(household_income, ordered = TRUE)
    )

# Helper: match target to ref groups
match_to_ref <- function(target_pool, ref_pool, exact_vars = NULL) {
    combined <- bind_rows(
        ref_pool    %>% mutate(is_ref = 1L),
        target_pool %>% mutate(is_ref = 0L)
    )
    fit <- matchit(
        is_ref ~ age_21003 + household_income,
        data     = combined,
        exact    = exact_vars,
        method   = "nearest",
        distance = "glm",
        m.order  = "random",
        ratio    = 1,
        replace  = FALSE,
        verbose  = FALSE
    )
    match.data(fit) %>%
        select(-c(distance, weights, subclass, is_ref))
}

# 2. Balance males and females within the Black group
fit_black <- matchit(
    genetic_sex_22001 == "Male" ~ age_21003,
    data     = df_tomatch %>% filter(eth_categ == "Black"),
    exact    = c("age_21003"),
    method   = "nearest",
    distance = "glm",
    m.order  = "random",
    ratio    = 1,
    replace  = FALSE,
    verbose  = FALSE
)
df_black <- match.data(fit_black) %>% select(-c(distance, weights, subclass))
black_m  <- df_black %>% filter(genetic_sex_22001 == "Male")
black_f  <- df_black %>% filter(genetic_sex_22001 == "Female")

# 2. Match each sex × ethnicity group to the Black reference
df_asian_m <- match_to_ref(
    df_tomatch %>% filter(eth_categ == "Asian", genetic_sex_22001 == "Male"),
    black_m,
    exact_vars = c("household_income")
)
df_asian_f <- match_to_ref(
    df_tomatch %>% filter(eth_categ == "Asian", genetic_sex_22001 == "Female"),
    black_f,
    exact_vars = c("household_income")
)
df_white_m <- match_to_ref(
    df_tomatch %>% filter(eth_categ == "White", genetic_sex_22001 == "Male"),
    df_asian_m %>% filter(eth_categ == "Black")
)
df_white_f <- match_to_ref(
    df_tomatch %>% filter(eth_categ == "White", genetic_sex_22001 == "Female"),
    df_asian_f %>% filter(eth_categ == "Black")
)

# Bind matched df
df_match = bind_rows(
    df_white_m %>% filter(eth_categ == "White"),
    df_white_f %>% filter(eth_categ == "White"),
    df_asian_m %>% filter(eth_categ == "Asian"),
    df_asian_f %>% filter(eth_categ == "Asian"),
    df_white_m %>% filter(eth_categ == "Black"),
    df_white_f %>% filter(eth_categ == "Black")
)

# Graphs of demographics
id_lists = list(
    df_match %>% filter(eth_categ == "White") %>% select(ID),
    df_match %>% filter(eth_categ == "Black") %>% select(ID),
    df_match %>% filter(eth_categ == "Asian") %>% select(ID)
)
dir.create(paste0("./results/eth_compare_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_compare_match"), showWarnings=FALSE)
compare_demo(
    id_lists,
    c(0,0,0),
    c("white_matched", "black_matched", "asian_matched"),
    c("White", "Black", "Asian"),
    "eth_compare_match"
)
compare_demo_sex(
    id_lists,
    c(0,0,0),
    c("white_matched", "black_matched", "asian_matched"),
    c("White", "Black", "Asian"),
    "eth_compare_match"
)

# Table of demographics
describe_demo_across_groups(df_match %>% mutate(instanceID = factor(instanceID, levels=c(0,1,2,3))), "eth_categ", "eth_compare_match")

# Write ID lists

# White matched
dir.create(paste0("./results/eth_white_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_white_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(eth_categ == "White") %>% select(ID), paste0("./results/eth_white_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_demo %>% filter(eth_categ == "White", instanceID == 0) %>% select(ID), paste0("./results/eth_white_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(eth_categ == "White") %>% select(ID), 0, "eth_white_tp0_match")

# Black matched
dir.create(paste0("./results/eth_black_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_black_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(eth_categ == "Black") %>% select(ID), paste0("./results/eth_black_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_demo %>% filter(eth_categ == "Black", instanceID == 0) %>% select(ID), paste0("./results/eth_black_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(eth_categ == "Black") %>% select(ID), 0, "eth_black_tp0_match")

# Asian matched
dir.create(paste0("./results/eth_asian_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_asian_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(eth_categ == "Asian") %>% select(ID), paste0("./results/eth_asian_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_demo %>% filter(eth_categ == "Asian", instanceID == 0) %>% select(ID), paste0("./results/eth_asian_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(eth_categ == "Asian") %>% select(ID), 0, "eth_asian_tp0_match")

#endregion

#region IDs: by Asian ethnicity (matched age and income)

print("By Asian ethnicity (matched)")

df_all = df_demo %>%
    filter(Ethnic_background_21000 %in% c("Chinese", "Indian", "Pakistani", "Bangladeshi")) %>%
    filter(instanceID == 0) %>%
    mutate(
        Ethnic_background_21000 = factor(Ethnic_background_21000),
        household_income = factor(household_income, ordered = TRUE),
        eth_group = factor(case_when(
            Ethnic_background_21000 == "Chinese" ~ "Chinese",
            Ethnic_background_21000 %in% c("Indian", "Pakistani", "Bangladeshi") ~ "South asian",
        ))
    )

df_tomatch = df_demo %>%
    filter(Ethnic_background_21000 %in% c("Chinese", "Indian", "Pakistani", "Bangladeshi")) %>%
    filter(complete.cases(.)) %>%
    filter(instanceID == 0) %>%
    filter(sex_aneuploidy == 0) %>%
    filter(genetic_sex_22001 == sex_31) %>%
    mutate(
        Ethnic_background_21000 = factor(Ethnic_background_21000),
        household_income = factor(household_income, ordered = TRUE),
        eth_group = factor(case_when(
            Ethnic_background_21000 == "Chinese" ~ "Chinese",
            Ethnic_background_21000 %in% c("Indian", "Pakistani", "Bangladeshi") ~ "South asian",
        ))
    )

# 1. Balance males and females within Chinese
fit_chinese <- matchit(
    genetic_sex_22001 == "Male" ~ age_21003,
    data     = df_tomatch %>% filter(eth_group == "Chinese"),
    exact    = c("age_21003"),
    method   = "nearest",
    distance = "glm",
    m.order  = "random",
    ratio    = 1,
    replace  = FALSE,
    verbose  = FALSE
)
df_chinese <- match.data(fit_chinese) %>% select(-c(distance, weights, subclass))
chinese_m  <- df_chinese %>% filter(genetic_sex_22001 == "Male")
chinese_f  <- df_chinese %>% filter(genetic_sex_22001 == "Female")

# Helper: match target to ref groups
match_to_ref <- function(target_pool, ref_pool, exact_vars = NULL) {
    combined <- bind_rows(
        ref_pool    %>% mutate(is_ref = 1L),
        target_pool %>% mutate(is_ref = 0L)
    )
    fit <- matchit(
        is_ref ~ age_21003 + household_income,
        data     = combined,
        exact    = exact_vars,
        method   = "nearest",
        distance = "glm",
        m.order  = "random",
        ratio    = 1,
        replace  = FALSE,
        verbose  = FALSE
    )
    match.data(fit) %>%
        select(-c(distance, weights, subclass, is_ref))
}

# 2. Match each sex × South asian group to the Chinese reference
df_southa_m <- match_to_ref(
    df_tomatch %>% filter(eth_group == "South asian", genetic_sex_22001 == "Male"),
    chinese_m
)
df_southa_f <- match_to_ref(
    df_tomatch %>% filter(eth_group == "South asian", genetic_sex_22001 == "Female"),
    chinese_f
)

# Bind matched df
df_match = bind_rows(
    df_southa_m %>% filter(eth_group == "Chinese"),
    df_southa_f %>% filter(eth_group == "Chinese"),
    df_southa_m %>% filter(eth_group == "South asian"),
    df_southa_f %>% filter(eth_group == "South asian")
)

# Graphs of demographics
id_lists = list(
    df_match %>% filter(eth_group == "Chinese") %>% select(ID),
    df_match %>% filter(eth_group == "South asian") %>% select(ID)
)
dir.create(paste0("./results/eth_compareAsian_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_compareAsian_match"), showWarnings=FALSE)
compare_demo(
    id_lists,
    c(0,0),
    c("chinese_matched", "southasian_matched"),
    c("Chinese", "South Asian"),
    "eth_compareAsian_match"
)
compare_demo_sex(
    id_lists,
    c(0,0),
    c("chinese_matched", "southasian_matched"),
    c("Chinese", "South Asian"),
    "eth_compareAsian_match"
)

# Table of demographics
describe_demo_across_groups(df_match %>% mutate(instanceID = factor(instanceID, levels=c(0,1,2,3))), "eth_group", "eth_compareAsian_match")

# Write ID lists
dir.create(paste0("./results/eth_chinese_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_chinese_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(eth_group == "Chinese") %>% select(ID), paste0("./results/eth_chinese_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(eth_group == "Chinese") %>% select(ID), paste0("./results/eth_chinese_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(eth_group == "Chinese") %>% select(ID), 0, "eth_chinese_tp0_match")

dir.create(paste0("./results/eth_southasian_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_southasian_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(eth_group == "South asian") %>% select(ID), paste0("./results/eth_southasian_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(eth_group == "South asian") %>% select(ID), paste0("./results/eth_southasian_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(eth_group == "South asian") %>% select(ID), 0, "eth_southasian_tp0_match")

#endregion

#region IDs: by Black ethnicity (matched age and income)

print("By Black ethnicity (matched)")

df_all = df_demo %>%
    filter(Ethnic_background_21000 %in% c("African", "Caribbean")) %>%
    filter(instanceID == 0) %>%
    mutate(
        Ethnic_background_21000 = factor(Ethnic_background_21000),
        household_income = factor(household_income, ordered = TRUE),
        eth_group = factor(case_when(
            Ethnic_background_21000 == "African" ~ "African",
            Ethnic_background_21000 == "Caribbean" ~ "Caribbean",
        ))
    )

df_tomatch = df_demo %>%
    filter(Ethnic_background_21000 %in% c("African", "Caribbean")) %>%
    filter(complete.cases(.)) %>%
    filter(instanceID == 0) %>%
    filter(sex_aneuploidy == 0) %>%
    filter(genetic_sex_22001 == sex_31) %>%
    mutate(
        Ethnic_background_21000 = factor(Ethnic_background_21000),
        household_income = factor(household_income, ordered = TRUE),
        eth_group = factor(case_when(
            Ethnic_background_21000 == "African" ~ "African",
            Ethnic_background_21000 == "Caribbean" ~ "Caribbean",
        ))
    )

# 1. Balance males and females within African
fit_african <- matchit(
    genetic_sex_22001 == "Male" ~ age_21003,
    data     = df_tomatch %>% filter(eth_group == "African"),
    exact    = c("age_21003"),
    method   = "nearest",
    distance = "glm",
    m.order  = "random",
    ratio    = 1,
    replace  = FALSE,
    verbose  = FALSE
)
df_african <- match.data(fit_african) %>% select(-c(distance, weights, subclass))
african_m  <- df_african %>% filter(genetic_sex_22001 == "Male")
african_f  <- df_african %>% filter(genetic_sex_22001 == "Female")

# Helper: match target to ref groups
match_to_ref <- function(target_pool, ref_pool, exact_vars = NULL) {
    combined <- bind_rows(
        ref_pool    %>% mutate(is_ref = 1L),
        target_pool %>% mutate(is_ref = 0L)
    )
    fit <- matchit(
        is_ref ~ age_21003 + household_income,
        data     = combined,
        exact    = exact_vars,
        method   = "nearest",
        distance = "glm",
        m.order  = "random",
        ratio    = 1,
        replace  = FALSE,
        verbose  = FALSE
    )
    match.data(fit) %>%
        select(-c(distance, weights, subclass, is_ref))
}

# 2. Match each sex × South asian group to the Chinese reference
df_caribbean_m <- match_to_ref(
    df_tomatch %>% filter(eth_group == "Caribbean", genetic_sex_22001 == "Male"),
    african_m,
    exact_vars = c("age_21003")
)
df_caribbean_f <- match_to_ref(
    df_tomatch %>% filter(eth_group == "Caribbean", genetic_sex_22001 == "Female"),
    african_f,
    exact_vars = c("age_21003")
)

# Bind matched df
df_match = bind_rows(
    df_caribbean_m %>% filter(eth_group == "Caribbean"),
    df_caribbean_f %>% filter(eth_group == "Caribbean"),
    df_caribbean_m %>% filter(eth_group == "African"),
    df_caribbean_f %>% filter(eth_group == "African")
)

# Graphs of demographics
id_lists = list(
    df_match %>% filter(eth_group == "Caribbean") %>% select(ID),
    df_match %>% filter(eth_group == "African") %>% select(ID)
)
dir.create(paste0("./results/eth_compareBlack_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_compareBlack_match"), showWarnings=FALSE)
compare_demo(
    id_lists,
    c(0,0),
    c("african_matched", "caribbean_matched"),
    c("African", "Caribbean"),
    "eth_compareBlack_match"
)
compare_demo_sex(
    id_lists,
    c(0,0),
    c("african_matched", "caribbean_matched"),
    c("African", "Caribbean"),
    "eth_compareBlack_match"
)

# Table of demographics
describe_demo_across_groups(df_match %>% mutate(instanceID = factor(instanceID, levels=c(0,1,2,3))), "eth_group", "eth_compareBlack_match")

# Write ID lists
dir.create(paste0("./results/eth_african_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_african_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(eth_group == "African") %>% select(ID), paste0("./results/eth_african_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(eth_group == "African") %>% select(ID), paste0("./results/eth_african_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(eth_group == "African") %>% select(ID), 0, "eth_african_tp0_match")

dir.create(paste0("./results/eth_caribbean_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/eth_caribbean_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(eth_group == "Caribbean") %>% select(ID), paste0("./results/eth_caribbean_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(eth_group == "Caribbean") %>% select(ID), paste0("./results/eth_caribbean_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(eth_group == "Caribbean") %>% select(ID), 0, "eth_caribbean_tp0_match")

#endregion

#region IDs: by income (matched age and ethnicity)

print("By income (matched)")

df_all = df_demo %>%
    filter(instanceID == 0) %>%
    mutate(
        household_income = factor(household_income, ordered = TRUE)
    )

df_tomatch = df_demo %>%
    filter(complete.cases(.)) %>%
    filter(sex_aneuploidy == 0) %>%
    filter(genetic_sex_22001 == sex_31) %>%
    filter(instanceID == 0) %>%
    mutate(
        household_income = factor(household_income, ordered = TRUE)
    )

# Helper: match target to ref groups
match_to_ref_tp <- function(target_pool, ref_pool,
                             exact_vars = NULL) {
    combined <- bind_rows(
        ref_pool    %>% mutate(is_ref = 1L),
        target_pool %>% mutate(is_ref = 0L)
    )
    fit <- matchit(
        is_ref ~ age_21003 + eth_categ,
        data     = combined,
        exact    = exact_vars,
        method   = "nearest",
        distance = "glm",
        ratio    = 1,
        replace  = FALSE,
        verbose  = FALSE
    )
    match.data(fit, drop.unmatched = TRUE) %>%
        select(-c(distance, weights, subclass, is_ref))
}

# 1. Balance males and females within >100k
fit_100k <- matchit(
    genetic_sex_22001 == "Male" ~ age_21003 + eth_categ,
    data     = df_tomatch %>% filter(household_income == "Greater than 100,000"),
    exact    = c("age_21003", "eth_categ"),
    method   = "nearest",
    distance = "glm",
    m.order  = "random",
    ratio    = 1,
    replace  = FALSE,
    verbose  = FALSE
)
df_100k <- match.data(fit_100k, drop.unmatched = TRUE) %>%
    select(-c(distance, weights, subclass))
df_100k_m <- df_100k %>% filter(genetic_sex_22001 == "Male")
df_100k_f <- df_100k %>% filter(genetic_sex_22001 == "Female")

# 2. Match to <18k and sex match again
df_18k_m <- match_to_ref_tp(df_tomatch %>% filter(household_income == "Less than 18,000", genetic_sex_22001 == "Male"),   df_100k_m, exact_vars = c("age_21003", "eth_categ"))
df_18k_f <- match_to_ref_tp(df_tomatch %>% filter(household_income == "Less than 18,000", genetic_sex_22001 == "Female"),   df_100k_f, exact_vars = c("age_21003", "eth_categ"))

fit_100k <- matchit(
    genetic_sex_22001 == "Male" ~ age_21003 + eth_categ,
    data     = bind_rows(df_18k_m, df_18k_f) %>% filter(household_income == "Greater than 100,000"),
    exact    = c("age_21003", "eth_categ"),
    method   = "nearest",
    distance = "glm",
    m.order  = "random",
    ratio    = 1,
    replace  = FALSE,
    verbose  = FALSE
)
df_100k <- match.data(fit_100k, drop.unmatched = TRUE) %>%
    select(-c(distance, weights, subclass))
df_100k_m <- df_100k %>% filter(genetic_sex_22001 == "Male")
df_100k_f <- df_100k %>% filter(genetic_sex_22001 == "Female")

# 3. Match each timepoint × sex group to >100k reference
df_18k_m <- match_to_ref_tp(df_tomatch %>% filter(household_income == "Less than 18,000", genetic_sex_22001 == "Male"),   df_100k_m, exact_vars = c("age_21003", "eth_categ"))
df_18k_f <- match_to_ref_tp(df_tomatch %>% filter(household_income == "Less than 18,000", genetic_sex_22001 == "Female"),   df_100k_f, exact_vars = c("age_21003", "eth_categ"))
df_31k_m <- match_to_ref_tp(df_tomatch %>% filter(household_income == "18,000 to 30,999", genetic_sex_22001 == "Male"),   df_18k_m %>% filter(household_income == "Greater than 100,000"))
df_31k_f <- match_to_ref_tp(df_tomatch %>% filter(household_income == "18,000 to 30,999", genetic_sex_22001 == "Female"),   df_18k_f %>% filter(household_income == "Greater than 100,000"))
df_52k_m <- match_to_ref_tp(df_tomatch %>% filter(household_income == "31,000 to 51,999", genetic_sex_22001 == "Male"),   df_18k_m %>% filter(household_income == "Greater than 100,000"))
df_52k_f <- match_to_ref_tp(df_tomatch %>% filter(household_income == "31,000 to 51,999", genetic_sex_22001 == "Female"),   df_18k_f %>% filter(household_income == "Greater than 100,000"))
df_70k_m <- match_to_ref_tp(df_tomatch %>% filter(household_income == "52,000 to 100,000", genetic_sex_22001 == "Male"),   df_18k_m %>% filter(household_income == "Greater than 100,000"))
df_70k_f <- match_to_ref_tp(df_tomatch %>% filter(household_income == "52,000 to 100,000", genetic_sex_22001 == "Female"),   df_18k_f %>% filter(household_income == "Greater than 100,000"))

# Combine and verify
df_match <- bind_rows(
    df_18k_m %>% filter(household_income == "Less than 18,000"),
    df_18k_f %>% filter(household_income == "Less than 18,000"),
    df_31k_m %>% filter(household_income == "18,000 to 30,999"),
    df_31k_f %>% filter(household_income == "18,000 to 30,999"),
    df_52k_m %>% filter(household_income == "31,000 to 51,999"),
    df_52k_f %>% filter(household_income == "31,000 to 51,999"),
    df_70k_m %>% filter(household_income == "52,000 to 100,000"),
    df_70k_f %>% filter(household_income == "52,000 to 100,000"),
    df_18k_m %>% filter(household_income == "Greater than 100,000"),
    df_18k_f %>% filter(household_income == "Greater than 100,000")
)

# Graphs of demographics
id_lists = list(
    df_match %>% filter(household_income == "Less than 18,000") %>% select(ID),
    df_match %>% filter(household_income == "18,000 to 30,999") %>% select(ID),
    df_match %>% filter(household_income == "31,000 to 51,999") %>% select(ID),
    df_match %>% filter(household_income == "52,000 to 100,000") %>% select(ID),
    df_match %>% filter(household_income == "Greater than 100,000") %>% select(ID)
)
dir.create(paste0("./results/income_compare_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/income_compare_match"), showWarnings=FALSE)
compare_demo(
    id_lists,
    c(0,0,0,0,0),
    c("income_1_match", "income_2_match", "income_3_match", "income_4_match", "income_5_match"),
    c("<18k", "18k to 31k", "31k to 52k", "52k to 100k", ">100k"),
    "income_compare_match"
)
compare_demo_sex(
    id_lists,
    c(0,0,0,0,0),
    c("income_1_match", "income_2_match", "income_3_match", "income_4_match", "income_5_match"),
    c("<18k", "18k to 31k", "31k to 52k", "52k to 100k", ">100k"),
    "income_compare_match"
)

# Table of demographics
describe_demo_across_groups(df_match %>% mutate(instanceID = factor(instanceID, levels=c(0,1,2,3))), "household_income", "income_compare_match")

# Write ID lists

# <18k matched
dir.create(paste0("./results/income_1_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/income_1_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(household_income == "Less than 18,000") %>% select(ID), paste0("./results/income_1_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(household_income == "Less than 18,000") %>% select(ID), paste0("./results/income_1_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(household_income == "Less than 18,000") %>% select(ID), 0, "income_1_tp0_match")

# 18k to 31k matched
dir.create(paste0("./results/income_2_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/income_2_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(household_income == "18,000 to 30,999") %>% select(ID), paste0("./results/income_2_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(household_income == "18,000 to 30,999") %>% select(ID), paste0("./results/income_2_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(household_income == "18,000 to 30,999") %>% select(ID), 0, "income_2_tp0_match")

# 31k to 52k matched
dir.create(paste0("./results/income_3_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/income_3_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(household_income == "31,000 to 51,999") %>% select(ID), paste0("./results/income_3_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(household_income == "31,000 to 51,999") %>% select(ID), paste0("./results/income_3_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(household_income == "31,000 to 51,999") %>% select(ID), 0, "income_3_tp0_match")

# 52k to 100k matched
dir.create(paste0("./results/income_4_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/income_4_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(household_income == "52,000 to 100,000") %>% select(ID), paste0("./results/income_4_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(household_income == "52,000 to 100,000") %>% select(ID), paste0("./results/income_4_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(household_income == "52,000 to 100,000") %>% select(ID), 0, "income_4_tp0_match")

# <100k matched
dir.create(paste0("./results/income_5_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/income_5_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(household_income == "Greater than 100,000") %>% select(ID), paste0("./results/income_5_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(household_income == "Greater than 100,000") %>% select(ID), paste0("./results/income_5_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(household_income == "Greater than 100,000") %>% select(ID), 0, "income_5_tp0_match")

#endregion

#region IDs: by generation (matched ethnicity and income)

print("By generation (matched)")

df_all = df_demo %>%
    filter(generation %in% c("Silent", "Boom 1", "Boom 2", "Gen X")) %>%
    filter(instanceID == 0) %>%
    mutate(
        household_income = factor(household_income, ordered = TRUE)
    )

df_tomatch = df_demo %>%
    filter(generation %in% c("Silent", "Boom 1", "Boom 2", "Gen X")) %>%
    filter(complete.cases(.)) %>%
    filter(sex_aneuploidy == 0) %>%
    filter(genetic_sex_22001 == sex_31) %>%
    filter(instanceID == 0) %>%
    mutate(
        household_income = factor(household_income, ordered = TRUE)
    )

# Helper: match target to ref groups
match_to_ref_tp <- function(target_pool, ref_pool,
                             exact_vars = NULL) {
    combined <- bind_rows(
        ref_pool    %>% mutate(is_ref = 1L),
        target_pool %>% mutate(is_ref = 0L)
    )
    fit <- matchit(
        is_ref ~ eth_categ + household_income,
        data     = combined,
        exact    = exact_vars,
        method   = "nearest",
        distance = "glm",
        ratio    = 1,
        replace  = FALSE,
        verbose  = FALSE
    )
    match.data(fit, drop.unmatched = TRUE) %>%
        select(-c(distance, weights, subclass, is_ref))
}

# 1. Balance males and females within Gen X
fit_genx <- matchit(
    genetic_sex_22001 == "Male" ~ age_21003 + eth_categ,
    data     = df_tomatch %>% filter(generation == "Gen X"),
    exact    = c("age_21003", "eth_categ"),
    method   = "nearest",
    distance = "glm",
    ratio    = 1,
    replace  = FALSE,
    verbose  = FALSE
)
df_genx <- match.data(fit_genx, drop.unmatched = TRUE) %>%
    select(-c(distance, weights, subclass))
df_genx_m <- df_genx %>% filter(genetic_sex_22001 == "Male")
df_genx_f <- df_genx %>% filter(genetic_sex_22001 == "Female")

# 2. Match to Gen X and sex match again
df_silent_m <- match_to_ref_tp(df_tomatch %>% filter(generation == "Silent", genetic_sex_22001 == "Male"), df_genx_m, exact_vars = c("eth_categ", "household_income"))
df_silent_f <- match_to_ref_tp(df_tomatch %>% filter(generation == "Silent", genetic_sex_22001 == "Female"), df_genx_f, exact_vars = c("eth_categ", "household_income"))

fit_genx <- matchit(
    genetic_sex_22001 == "Male" ~ age_21003 + eth_categ,
    data     = bind_rows(df_silent_m, df_silent_f) %>% filter(generation == "Gen X"),
    exact    = c("age_21003", "eth_categ"),
    method   = "nearest",
    distance = "glm",
    ratio    = 1,
    replace  = FALSE,
    verbose  = FALSE
)
df_genx <- match.data(fit_genx, drop.unmatched = TRUE) %>%
    select(-c(distance, weights, subclass))
df_genx_m <- df_genx %>% filter(genetic_sex_22001 == "Male")
df_genx_f <- df_genx %>% filter(genetic_sex_22001 == "Female")

# 3. Match each timepoint × sex group to TP3 reference
df_silent_m <- match_to_ref_tp(df_tomatch %>% filter(generation == "Silent", genetic_sex_22001 == "Male"), df_genx_m, exact_vars = c("eth_categ", "household_income"))
df_silent_f <- match_to_ref_tp(df_tomatch %>% filter(generation == "Silent", genetic_sex_22001 == "Female"), df_genx_f, exact_vars = c("eth_categ", "household_income"))
df_boom1_m <- match_to_ref_tp(df_tomatch %>% filter(generation == "Boom 1", genetic_sex_22001 == "Male"), df_silent_m %>% filter(generation == "Gen X"))
df_boom1_f <- match_to_ref_tp(df_tomatch %>% filter(generation == "Boom 1", genetic_sex_22001 == "Female"), df_silent_f %>% filter(generation == "Gen X"))
df_boom2_m <- match_to_ref_tp(df_tomatch %>% filter(generation == "Boom 2", genetic_sex_22001 == "Male"), df_silent_m %>% filter(generation == "Gen X"))
df_boom2_f <- match_to_ref_tp(df_tomatch %>% filter(generation == "Boom 2", genetic_sex_22001 == "Female"), df_silent_f %>% filter(generation == "Gen X"))

# Combine and verify
df_match = bind_rows(
    df_silent_m %>% filter(generation == "Silent"),
    df_silent_f %>% filter(generation == "Silent"),
    df_boom1_m %>% filter(generation == "Boom 1"),
    df_boom1_f %>% filter(generation == "Boom 1"),
    df_boom2_m %>% filter(generation == "Boom 2"),
    df_boom2_f %>% filter(generation == "Boom 2"),
    df_silent_m %>% filter(generation == "Gen X"),
    df_silent_f %>% filter(generation == "Gen X")
)

# Graphs of demographics
id_lists = list(
    df_match %>% filter(generation == "Silent") %>% select(ID),
    df_match %>% filter(generation == "Boom 1") %>% select(ID),
    df_match %>% filter(generation == "Boom 2") %>% select(ID),
    df_match %>% filter(generation == "Gen X") %>% select(ID)
)
dir.create(paste0("./results/gen_compare_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/gen_compare_match"), showWarnings=FALSE)
compare_demo(
    id_lists,
    c(0,0,0,0),
    c("silent_matched", "boom1_matched", "boom2_matched", "genx_matched"),
    c("Silent", "Boom 1", "Boom 2", "Gen X"),
    "gen_compare_match", age_sm_adj=3
)
compare_demo_sex(
    id_lists,
    c(0,0,0,0),
    c("silent_matched", "boom1_matched", "boom2_matched", "genx_matched"),
    c("Silent", "Boom 1", "Boom 2", "Gen X"),
    "gen_compare_match", age_sm_adj=3
)

# Table of demographics
describe_demo_across_groups(df_match %>% mutate(instanceID = factor(instanceID, levels=c(0,1,2,3))), "generation", "gen_compare_match")

# Write ID lists

# Silent matched
dir.create(paste0("./results/gen_silent_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/gen_silent_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(generation == "Silent") %>% select(ID), paste0("./results/gen_silent_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(generation == "Silent") %>% select(ID), paste0("./results/gen_silent_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(generation == "Silent") %>% select(ID), 0, "gen_silent_tp0_match")

# Boom 1 matched
dir.create(paste0("./results/gen_boom1_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/gen_boom1_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(generation == "Boom 1") %>% select(ID), paste0("./results/gen_boom1_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(generation == "Boom 1") %>% select(ID), paste0("./results/gen_boom1_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(generation == "Boom 1") %>% select(ID), 0, "gen_boom1_tp0_match")

# Boom 2 matched
dir.create(paste0("./results/gen_boom2_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/gen_boom2_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(generation == "Boom 2") %>% select(ID), paste0("./results/gen_boom2_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(generation == "Boom 2") %>% select(ID), paste0("./results/gen_boom2_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(generation == "Boom 2") %>% select(ID), 0, "gen_boom2_tp0_match")

# Gen X matched
dir.create(paste0("./results/gen_genx_tp0_match"), showWarnings=FALSE)
dir.create(paste0("./visualization/gen_genx_tp0_match"), showWarnings=FALSE)
fwrite(df_match %>% filter(generation == "Gen X") %>% select(ID), paste0("./results/gen_genx_tp0_match/id_list.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
fwrite(df_all %>% filter(generation == "Gen X") %>% select(ID), paste0("./results/gen_genx_tp0_match/id_list_all.txt"), col.names=TRUE, row.names=FALSE, quote=FALSE, sep="\t")
describe_demo(df_match %>% filter(generation == "Gen X") %>% select(ID), 0, "gen_genx_tp0_match")

#endregion
