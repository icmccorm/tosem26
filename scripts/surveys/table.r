source("scripts/surveys/base.r")
table_dir <- "./build/tables"
if (!dir.exists(table_dir)) {
    dir.create(table_dir)
}

## Figure 2.a
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
    mutate(`Affiliation` = str_replace_all(`Affiliation`, regex("Rust team", ignore_case = TRUE), "R")) %>%
    arrange(PID)
    
screening_survey_demo %>%
    write_csv(file.path(table_dir, "interview_participant_demographics.csv"))


## Figure 2.b

# 1 if the participant was recruited by the given method, 0 otherwise
found <- survey %>%
    filter(question_id == "DQ5") %>%
    select(-question_id) %>%
    pivot_wider(
        names_from = value,
        values_from = value,
        values_fn = length,
        values_fill = 0
    ) %>%
    mutate(All = 1)

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

years_pivot <- years %>% 
    pivot_wider(names_from = language, values_from = value)

for (group in colnames(found)[-1]) {
    response_ids <- found %>%
        filter(.data[[group]] == 1) %>%
        select(response_id)
    group_count <- response_ids %>% nrow()
    for (language in colnames(years_pivot)[-1]) {
        years_for_group <- years_pivot %>%
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
        years_stats <- years_stats %>% 
            add_row(
                group = group, 
                group_count = group_count,
                language = language,
                min = min, 
                max = max, 
                mean = mean, 
                stdev = stdev
            )
    }
}

years_stats <- years_stats %>% 
    mutate(group = str_replace_all(group, "The Rust Programming Language Community Discord", "Rust Discord"))
years_stats <- years_stats %>% 
    mutate(group = str_replace_all(group, "The Rust Programming Language Forums", "Rust Forums"))

years_stats$summary <- paste0(years_stats$mean, " ± ", years_stats$stdev)
years_stats <- years_stats %>% select(-min, -max, -mean, -stdev)
years_stats <- years_stats %>% pivot_wider(names_from = language, values_from = summary, names_sep = "_")

years_stats_format <- years_stats %>%
    mutate_if(is.numeric, as.character) %>%
    mutate_all(~ ifelse(is.na(.x), "-", .x))

write.csv(years_stats_format, file = "./build/tables/years.csv")


## Table 5
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