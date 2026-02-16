suppressPackageStartupMessages({
    library(dplyr)
    library(readr)
    library(xtable)
    library(stringr)
    library(purrr)
    library(tidyr)
    library(reshape2)
})

is.yes <- function(x) {
    return(x == "Yes")
}

positivity_neutral <- c(
    "Neither positively nor negatively",
    "Unsure"
)
positivity_choices <- c(
    c("Extremely negatively", "Somewhat negatively"),
    positivity_neutral, 
    c("Somewhat positively", "Extremely positively")
)

frequency_neutral <- c("About half the time")
frequency_choices <- c(
    "Never", 
    "Sometimes", 
    frequency_neutral, 
    "Most of the time", 
    "Always"
)

frequency_unsure_neutral <- c("Unsure", frequency_neutral)
frequency_choices_unsure <- c(
    "Never", 
    "Sometimes", 
    "Unsure", 
    frequency_neutral, 
    "Most of the time", 
    "Always"
)

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

survey_raw <- read_csv(
        file.path("./data/community_survey/data.csv"), 
        show_col_types = FALSE
    ) %>%
    mutate(response_id = row_number())

screening_raw <- read_csv(
    file.path("./data/screening_survey/data.csv"),
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
    filter(EndDate <= as.Date("2023-05-25"))

screening <- screening_eligible %>%
    filter(!is.na(ID)) %>%
    filter(!is.na(PID))

questions <- read_csv(
    file.path("./data/community_survey/questions.csv"), 
    show_col_types = FALSE
)

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
    select(response_id, ELQ1, BQ1, BQ2, BQ3) %>%
    pivot_longer(
        cols = c("ELQ1", "BQ1", "BQ2", "BQ3"),
        names_to = "language", 
        values_to = "value"
    )
    
years$language <- ifelse(years$language == "BQ1", "SE", years$language)
years$language <- ifelse(years$language == "BQ2", "C", years$language)
years$language <- ifelse(years$language == "BQ3", "C++", years$language)
years$language <- ifelse(years$language == "ELQ1", "Rust", years$language)

years_summarized <- years %>%
    group_by(language) %>%
    summarise(
        mean = mean(value),
        min = min(value),
        max = max(value),
        stdev = sd(value)
    ) %>%
    ungroup()
colnames(years_summarized) <- c("Language", "Mean", "Min", "Max", "Stdev")

survey_pivot <- survey
survey <- survey %>%
    pivot_longer(
        !response_id,
        names_to = "question_id",
        values_to = "value",
        values_transform = as.character
    ) %>%
    separate_rows(value, sep = ",") %>%
    mutate(value = trimws(value)) %>%
    filter(value != "") %>%
    filter(!is.na(question_id)) %>%
    mutate(value = ifelse(is.na(value), "N/A", value))


all <- survey %>%
    select(response_id) %>%
    unique()

compute_num_responses <- function(population) {
    result <- survey %>%
        inner_join(population, by = c("response_id")) %>%
        group_by(question_id) %>%
        summarise(total_num_responses = n()) %>%
        ungroup()
    return(result)
}

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
        summarise(num_responses = n(), .groups = "keep") %>%
        ungroup() %>%
        inner_join(num_responses_per_question_in_sample, by = c("question_id")) %>%
        mutate(percentage = round(num_responses / total_num_responses * 100, 1)) %>%
        select(question_id, value, percentage, num_responses, total_num_responses) %>%
        ungroup()
    return(result)
}

FREQ_ALL <- compute_frequency(all)

# Certain likert scale questions may not have had all
# choices selected. This function adds a value of "0"
# for any choices missing from `choices`.
ensure_choices <- function(df, choices) {
    response_col <- match("value", names(df))
    cols_to_zero <- (response_col + 1):ncol(df)
    missing_choices <- setdiff(choices, df$value)
    if (length(missing_choices) > 0) {
        for (choice in missing_choices) {
            df <- df %>%
                add_row(value = choice) %>%
                mutate_at(cols_to_zero, ~ ifelse(is.na(.), 0, .))
        }
    }
    return(df)
}

# Computes the percentage of each likert choice, ensuring that
# choices which were never selected are recorded as 0%.
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