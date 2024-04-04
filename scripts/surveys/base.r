suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(xtable)
    library(ggplot2)
    library(cowplot)
    library(extrafont)
    library(extrafontdb)
    library(stringr)
    library(purrr)
    library(tidyr)
    library(reshape2)
    library(ggpubr)
})
sessionInfo()
loadfonts(quiet = TRUE)
options(vsc.plot = FALSE)

is.yes <- function(x) {
    return(x == "Yes")
}

ACM_PAGE_WIDTH <- 5.497527778
ACM_COL_WIDTH <- ACM_PAGE_WIDTH / 2
PLOT_FONT <- "Linux Libertine Display"

positivity_neutral <- c("Neither positively nor negatively", "Unsure")
positivity_choices <- c(c("Extremely negatively", "Somewhat negatively"), positivity_neutral, c("Somewhat positively", "Extremely positively"))

frequency_neutral <- c("About half the time")
frequency_choices <- c("Never", "Sometimes", frequency_neutral, "Most of the time", "Always")

frequency_unsure_neutral <- c("Unsure", frequency_neutral)
frequency_choices_unsure <- c("Never", "Sometimes", "Unsure", frequency_neutral, "Most of the time", "Always")

agreement_choices <- c(
    "Strongly disagree",
    "Somewhat disagree",
    "Neither agree nor disagree",
    "Somewhat agree",
    "Strongly agree"
)

age_choices <- c(
    "18-29",
    "30-39",
    "40-49",
    "50-59",
    "Prefer not to answer"
)
gender_choices <- c(
    "Non-binary",
    "Woman",
    "Man",
    "Prefer to self-describe",
    "Prefer not to disclose"
)

affiliation_choices <- c(
    "Academia",
    "Industry",
    "Open-source contributor",
    "Rust project member",
    "Other"
)

education_choices <- c(
    "Some high school",
    "High school diploma/GED",
    "Some college",
    "Bachelor's degree",
    "Master's degree",
    "PhD",
    "Prefer not to answer"
)

yes_no_binary_choices <- c(
    "Yes",
    "No"
)

difficulty_choices <- c(
    "Extremely difficult",
    "Somewhat difficult",
    "Neither easy nor difficult",
    "Somewhat easy",
    "Extremely easy"
)

yes_no_choices <- c(
    "Definitely not",
    "Probably not",
    "Might or might not",
    "Probably yes",
    "Definitely yes"
)

date_frequency_choices <- c(
    "Daily",
    "Weekly",
    "Monthly",
    "Yearly",
    "Less than once a year"
)

likely_unlikely_choices <- c(
    "Extremely unlikely",
    "Somewhat unlikely",
    "Neither likely nor unlikely",
    "Somewhat likely",
    "Extremely likely"
)

options(dplyr.summarise.inform = FALSE)

survey_raw <- read_csv(file.path("./data/community_survey/data.csv"), show_col_types = FALSE)

# add a respondent ID column
survey_raw <- survey_raw %>%
    mutate(response_id = row_number())
screening_survey_path <- file.path("./data/screening_survey/data.csv")
screening_raw <- read_csv(
    screening_survey_path,
    show_col_types = FALSE
)
sections <- read_csv(file.path("./data/community_survey/sections.csv"), show_col_types = FALSE)
screening_eligible <- screening_raw %>%
    filter(
        is.yes(CQ1),
        is.yes(CQ2),
        is.yes(CQ3),
        is.yes(EQ1),
        is.yes(EQ2),
        Finished == TRUE,
        Progress == 100
    ) %>%
    mutate(EndDate = as.Date(EndDate, format = "%m/%d/%Y")) %>%
    filter(EndDate <= as.Date("2018-05-25"))

screening <- screening_eligible %>%
    filter(!is.na(ID)) %>%
    filter(!is.na(PID))

questions <- read_csv(file.path("./data/community_survey/questions.csv"), show_col_types = FALSE)

survey_complete_and_elligible <- survey_raw %>%
    filter(Finished == TRUE) %>%
    filter(is.yes(CQ1)) %>%
    filter(is.yes(CQ2)) %>%
    filter(is.yes(CQ3)) %>%
    filter(!is.na(ELQ2)) %>%
    filter(is.yes(ELQ2)) %>%
    filter(!is.na(ELQ1)) %>%
    filter(trimws(ELQ1) != "") %>%
    mutate(ELQ1 = as.numeric(ELQ1)) %>%
    filter(ELQ1 >= 1) %>%
    filter(DistributionChannel == "anonymous") 
    
survey <- survey_complete_and_elligible %>%
    filter(Q_RecaptchaScore >= 0.5) %>%
    filter(is.na(Q_RelevantIDDuplicate)) %>%
    filter(Q_RelevantIDDuplicateScore < 75) %>%
    filter(Q_RelevantIDFraudScore < 30)

years <- survey %>%
    select(response_id, ELQ1, BQ2, BQ3) %>%
    pivot_longer(cols = c("ELQ1", "BQ2", "BQ3"), names_to = "language", values_to = "value")

years <- years %>%
    group_by(language) %>%
    summarise(
        mean = mean(value),
        min = min(value),
        max = max(value),
        stdev = sd(value)
    ) %>%
    ungroup()
years$language <- ifelse(years$language == "BQ2", "C", years$language)
years$language <- ifelse(years$language == "BQ3", "C++", years$language)
years$language <- ifelse(years$language == "ELQ1", "Rust", years$language)

# column names to title
colnames(years) <- c("Language", "Mean", "Min", "Max", "Stdev")
survey_pivot <- survey
survey <- survey %>%
    pivot_longer(
        !response_id,
        names_to = "question_id",
        values_to = "value",
        values_transform = as.character
    )

survey <- survey %>%
    separate_rows(value, sep = ",") %>%
    mutate(value = trimws(value)) %>%
    filter(value != "") %>%
    filter(!is.na(question_id)) %>%
    mutate(value = ifelse(is.na(value), "N/A", value))

# - HELPER FUNCTIONS -

compute_num_responses <- function(population) {
    result <- survey %>%
        inner_join(population, by = c("response_id")) %>%
        group_by(question_id) %>%
        summarise(total_num_responses = n()) %>%
        ungroup()
    return(result)
}
all <- survey %>%
    select(response_id) %>%
    unique()
num_responses_per_question <- compute_num_responses(all)

compute_frequency <- function(sample) {
    sample_size <- nrow(sample)
    num_responses_per_question_in_sample <- survey %>% 
        filter(response_id %in% sample$response_id) %>%
        group_by(question_id) %>%
        summarise(total_num_responses = n()) %>%
        ungroup()
    result <- survey %>%
        inner_join(sample, by = c("response_id")) %>%
        group_by(question_id, value) %>%
        summarise(num_responses = n()) %>%
        inner_join(num_responses_per_question_in_sample, by = c("question_id")) %>%
        mutate(percentage = round(num_responses / total_num_responses * 100, 1)) %>%
        select(question_id, value, percentage, num_responses, total_num_responses) %>%
        ungroup()
    return(result)
}
FREQ_ALL <- compute_frequency(all)


# — HELPER FUNCTIONS —

ensure_choices <- function(df, choices) {
    response_col <- match("value", names(df))

    cols_to_zero <- (response_col + 1):ncol(df)

    missing_choices <- setdiff(choices, df$value)

    # If there are missing choices, set the corresponding columns to zero
    if (length(missing_choices) > 0) {
        for (choice in missing_choices) {
            df <- df %>%
                add_row(value = choice) %>%
                mutate_at(cols_to_zero, ~ ifelse(is.na(.), 0, .))
        }
    }
    return(df)
}

compute_likert_frequency <- function(frequency, id, choices) {
    computation <- questions %>%
        filter(question_id == id) %>%
        inner_join(frequency, by = c("question_id")) %>%
        mutate(question = paste0(question_id, " - ", question)) %>%
        select(-question_id) %>%
        group_by(question) %>%
        group_modify(~ ensure_choices(., choices)) %>%
        ungroup() %>%
        pivot_wider(id_cols = question, names_from = value, values_from = percentage) %>%
        select(question, all_of(choices))
    return(computation)
}


compute_category_frequency <- function(selected_question) {
    computation <- questions %>%
        filter(question_id %in% c(selected_question)) %>%
        inner_join(FREQ_ALL, by = c("question_id")) %>%
        select(-question, -question_id, -num_responses, -total_num_responses) %>%
        rename(
            All = percentage
        ) %>%
        melt(id.vars = "value")

    # for each group, order the 'category' column by choices
    colnames(computation) <- c("category", "group", "value")
    computation
}

plot_likert_bar <- function(df, total_num_responses, options, path, center = c(), palette = "RdYlBu", extract_legend = NA, add_unsure = FALSE) {
    width <- 0.35*ACM_COL_WIDTH
    height <- 0.3
    # if unsure is not a column, add it with the value 0
    if (add_unsure) {
        if (!("Unsure" %in% colnames(df))) {
            df$Unsure <- 0
        }
    }
    if (length(center) == 0) {
        if (length(options) %% 2 == 1) {
            center <- c(options[[ceiling(length(options) / 2)]])
        } 
    }
    if (length(center) == 0) {
        index_left <- length(options) / 2
        index_right <- index_left + 1
    }else {
        index_left <- which(colnames(df) == center[1])
        index_right <- which(colnames(df) == center[length(center)]) + 1
    }

    excluded_middle <- colnames(df)[(index_left):(index_right - 1)]

    sum_right <- df %>%
        select(-question) %>%
        select(all_of(0:(index_left - 2))) %>%
        rowSums() %>%
        round(0)

    sum_left <- df %>%
        select(-question) %>%
        select(all_of((index_right-1):ncol(.))) %>%
        rowSums() %>%
        round(0)

    stopifnot(sum_left + sum_right <= 100)

    df <- df %>%
        melt(id.vars = "question") %>%
        mutate(
            variable = factor(variable, levels = options, ordered = TRUE),
            value = value
        )
    df$value <- round(df$value, 0)
    if(PLOT_FONT %in% fonts()) {
        default_family <- PLOT_FONT
        default_text <- element_text(size = 8, family = PLOT_FONT, color = "#000000")
    } else {
        default_family <- ""
        default_text <- element_text(size = 8, color = "#000000")
    }

    likert_plot <- ggplot(data = df, aes(question, value, fill = variable)) +
        geom_col(
            position = position_stack(),
            width = 1
        ) +
        labs(
            x = element_blank(),
            y = element_blank(),
            fill = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
        ) + 
        coord_flip(clip = "off") +
        theme(text = default_text) +
        scale_y_continuous(labels = abs, expand = c(0, 0)) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_fill_brewer(
            palette = palette,
            drop = FALSE,
        ) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin=(margin(l=0, r=0, t=0, b=0)),
            text = default_text,
            legend.margin = margin(0, 0, 0, 0),
            legend.key.spacing.x = unit(3, "mm"),
            legend.key.width = unit(3.5, "mm"),
            legend.key.height = unit(3.5, "mm"),
            legend.key.spacing.y = unit(0, "mm"),
            legend.position = "bottom",
            legend.direction = "horizontal",  
            legend.text = element_text(size = 8, family = default_family),
            axis.ticks.length = unit(0, "pt")
        ) + guides(
        fill = guide_legend(
            nrow = 1, 
            byrow = TRUE,
            reverse = TRUE,
            override.aes = list(
                linetype = 1
            )
        )
    )

    if (!is.na(extract_legend)) {
        legend_plot <- as_ggplot(get_legend(likert_plot))
        ggsave(
            plot = legend_plot,
            file = extract_legend,
            width = ACM_PAGE_WIDTH,
            height = 0.3,
            units = c("in"),
            dpi = 300
        )
    }
    likert_plot <- likert_plot + 
            theme(legend.position = "none")
    ggsave(
        plot = likert_plot,
        file = path,
        width = width,
        height = 0.15,
        units = c("in"),
        dpi = 300
    )
    return(c(sum_left, sum_right, total_num_responses))
}