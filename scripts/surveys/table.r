source("scripts/surveys/base.r")
table_dir <- "./build/tables"
if (!dir.exists(table_dir)) {
    dir.create(table_dir)
}
coding_decisions <- read_csv(file.path("data/interviews/coding/decisions.csv"), show_col_types = FALSE) 
coding_decisions_split <- coding_decisions %>%
    mutate(Codes = str_split(Codes, ",")) %>%
    unnest(Codes) %>%
    mutate(Codes = trimws(Codes))
    
## Table of Codes and Themes
codes <- read_csv(file.path("data/interviews/coding/codebook.csv"), show_col_types = FALSE) %>%
    mutate(Groups = str_split(Groups, ", ")) %>%
    unnest(Groups) %>%
    mutate(Groups = trimws(Groups)) %>%
    select(Code, Groups, Comment) %>%
    filter(Groups == "Miri")

## Table of Interview Participant Demographic Information 

interview_duration <- read_csv(file.path("data/interviews/duration.csv"), show_col_types = FALSE) %>%
    mutate(duration_in_minutes = round(as.numeric(duration_in_minutes), 1)) %>%
    rename(`Duration (min.)` = duration_in_minutes)

coding <- coding_decisions %>%
    select(`Number of Codes`, `Document`) %>%
    group_by(Document) %>% 
    summarize(`# Codes` = sum(`Number of Codes`)) %>%
    mutate(Document = as.numeric(str_replace(Document, "Interview P", ""))) %>%
    rename(PID = Document)

screening_survey_demo <- screening %>%
    select(PID, PQ3, BQ1_CLEAN) %>%
    rename(
        `# Years Rust` = BQ1_CLEAN,
        `Affiliation` = PQ3
    ) %>%
    mutate(PID = as.numeric(str_replace(PID, "P", ""))) %>%
    mutate(`Affiliation` = str_replace_all(`Affiliation`, regex("Industry", ignore_case = TRUE), "I")) %>%
    mutate(`Affiliation` = str_replace_all(`Affiliation`, regex("Academia", ignore_case = TRUE), "A")) %>%
    mutate(`Affiliation` = str_replace_all(`Affiliation`, regex("Open Source", ignore_case = TRUE), "O")) %>%
    mutate(`Affiliation` = str_replace_all(`Affiliation`, regex("Rust team", ignore_case = TRUE), "R"))

survey_stats_table <- screening_survey_demo %>% 
    full_join(interview_duration, by=c("PID")) %>%
    full_join(coding, by=c("PID")) %>%
    arrange(PID) %>%
    write_csv(file.path(table_dir, "interview_participant_demographics.csv"))
    
## Table of reasons why developers chose to use unsafe (Community Survey Question WUQ1)
options <- survey %>%
    filter(question_id == "WUQ1") %>%
    select(value) %>%
    unique()

options <- options[options != "Other"]

response_ids <- options %>%
    map(~ survey %>%
        filter(question_id == "WUQ1" & value == .x) %>%
        select(response_id) %>%
        unique())

options <- options %>% map(~ ifelse(.x == "I am not aware of a safe alternative at any level of ease-of-use or performance.", "No choice", .x))
options <- options %>% map(~ ifelse(.x == "I could use a safe pattern but unsafe is faster or more space-efficient.", "Performance", .x))
options <- options %>% map(~ ifelse(.x == "I could use a safe pattern but unsafe is easier to implement or more ergonomic.", "Ergonomics", .x))

table <- matrix(0, nrow = length(options), ncol = length(options))
rownames(table) <- options
colnames(table) <- options

num_users <- survey %>%
    filter(question_id == "WUQ1") %>%
    select(response_id) %>%
    unique() %>%
    nrow()
for (i in 1:nrow(table)) {
    for (j in 1:ncol(table)) {
        num_in_category <- union(response_ids[[i]], response_ids[[j]]) %>%
            unique() %>%
            nrow()
        table[i, j] <- paste0(round(num_in_category / num_users * 100, 0), "%")
    }
}
table[lower.tri(table)] <- ""
write.csv(table, file = "./build/tables/why_unsafe.csv")

found <- survey %>%
    filter(question_id == "DQ5") %>%
    select(-question_id)
# pivot so that column names are from "value", and the values are 0 or 1 based on whether a participant selected a given value
found <- found %>% pivot_wider(names_from = value, values_from = value, values_fn = length, values_fill = 0)
found <- found %>% mutate(All = 1)
years <- survey_pivot %>% select(response_id, BQ1, BQ2, BQ3, ELQ1)
colnames(years) <- c("response_id", "SE", "C", "C++", "Rust")

years_stats <- data.frame(
    group = character(),
    group_count = numeric(),
    language = character(),
    min = numeric(),
    max = numeric(),
    mean = numeric(),
    stdev = numeric(),
    stringsAsFactors = FALSE
)
for (group in colnames(found)[-1]) {
    response_ids <- found %>%
        filter(.data[[group]] == 1) %>%
        select(response_id)
    group_count <- response_ids %>% nrow()
    for (language in colnames(years)[-1]) {
        years_for_group <- years %>%
            inner_join(response_ids, by = "response_id") %>%
            select(all_of(language))
        min <- years_for_group %>%
            pull(language) %>%
            min() %>%
            round(1)
        max <- years_for_group %>%
            pull(language) %>%
            max() %>%
            round(1)
        mean <- years_for_group %>%
            pull(language) %>%
            mean() %>%
            round(1)
        stdev <- years_for_group %>%
            pull(language) %>%
            sd() %>%
            round(1)
        years_stats <- years_stats %>% add_row(group = group, group_count = group_count, language = language, min = min, max = max, mean = mean, stdev = stdev)
    }
}

years_stats <- years_stats %>% mutate(group = str_replace_all(group, "The Rust Programming Language Community Discord", "Rust Discord"))
years_stats <- years_stats %>% mutate(group = str_replace_all(group, "The Rust Programming Language Forums", "Rust Forums"))

years_stats$summary <- paste0(years_stats$mean, " ± ", years_stats$stdev)
years_stats <- years_stats %>% select(-min, -max, -mean, -stdev)
years_stats <- years_stats %>% pivot_wider(names_from = language, values_from = summary, names_sep = "_")

years_stats_format <- years_stats %>%
    mutate_if(is.numeric, as.character) %>%
    mutate_all(~ ifelse(is.na(.x), "-", .x))

write.csv(years_stats_format, file = "./build/tables/years.csv")


unsafe_features <- survey %>% filter(question_id %in% c("WUEQ1", "WUPQ1", "WUNQ1"))
compute_ratio <- function(l, r) {
    # two comma-separated lists of values
    if (is.na(l) | is.na(r)) {
        return(NA)
    } else {
        return(paste0(l, r))
    }
}
# for each respondent, find the list of unsafe features they use by each motivation\

unsafe_features <- survey %>%
    filter(question_id %in% c("WUEQ1", "WUPQ1", "WUNQ1")) %>%
    group_by(response_id, question_id) %>%
    summarise(value = paste(value, collapse = ", ")) %>%
    spread(question_id, value)
colnames(unsafe_features) <- c("response_id", "Ergonomics", "Performance", "No Choice")
# create additional columns with the value of compute_ratio between each unique column
unsafe_features <- unsafe_features %>%
    mutate(
        ergonomics_performance = compute_ratio(Ergonomics, Performance),
        ergonomics_safety = compute_ratio(Ergonomics, `No Choice`),
        performance_safety = compute_ratio(Performance, `No Choice`)
    ) %>%
    select(-Ergonomics, -Performance, -`No Choice`)

unsafe_features %>%
    filter(!is.na(ergonomics_performance)) %>%
    ungroup() %>%
    select(ergonomics_performance) %>%
    map(~ str_split(.x, ", ")) %>%
    reduce(c) %>%
    reduce(c) %>%
    table() %>%
    as.data.frame() %>%
    arrange(desc(Freq))

