source("./scripts/surveys/base.r")
if (file.exists("./build/stats.csv")) {
    file.remove("./build/stats.csv")
}

dir.create("./build", showWarnings = FALSE)

stats <- data.frame(
    key = character(),
    value = numeric(),
    stringsAsFactors = FALSE
)

to_key_value <- function(label, question, df, .., optional = FALSE, num_responses_override = NA) {
    df <- df %>% filter(question_id == question)
    if (is.na(num_responses_override)) {
        num_responses <- df %>%
            select(response_id) %>%
            unique() %>%
            nrow()
    } else {
        num_responses <- num_responses_override
    }
    transformed <- df %>%
        group_by(value) %>%
        summarise(count = round(n() / num_responses * 100, 1)) %>%
        mutate(key = paste(value)) %>%
        mutate(value = count) %>%
        select(key, value)
    df <- if (optional) {
        df %>%
            group_by(value) %>%
            summarize(count = n()) %>%
            mutate(key = value) %>%
            mutate(value = count) %>%
            select(key, value) %>%
            bind_rows(transformed)
    } else {
        data.frame(key = "# responses", value = num_responses) %>% 
            bind_rows(transformed)
    }
    colnames(df)[colnames(df) == "key"] <- label
    return(df)
}

# The number of eligible canidates invited for interviews
num_interview_candidates <- screening_eligible %>%
    nrow()

# The number of interview participants
num_interview_participants <- screening %>%
    filter(!is.na(ID)) %>%
    nrow()

# The number of community survey responses.
total_community_survey_responses <- survey_raw %>%
    select(response_id) %>%
    unique() %>%
    nrow()

# The number of complete community survey responses.
total_complete_community_survey_responses <- survey_complete_and_elligible %>%
    nrow()

# The number of complete, eligible survey responses.
total_valid_before_fraud_detection <- survey_complete_and_elligible %>%
    filter(DistributionChannel != "preview") %>%
    select(response_id) %>%
    unique() %>%
    nrow()

total_valid <- survey %>%
    select(response_id) %>%
    unique() %>%
    nrow()


# ————DEMOGRAPHICS————

# Interview participants' affiliation
interview_participant_affiliation <- screening %>%
    select(ID, PQ3) %>%
    separate_rows(PQ3, sep = ",") %>%
    mutate(PQ3 = str_to_lower(PQ3)) %>%
    group_by(PQ3) %>%
    summarize(count = n()) %>%
    rename(
        key = PQ3,
        value = count
    )

# Survey participants' affiliation
stats <- survey %>%
    to_key_value("demo.affiliation", "DQ4", .)

# Survey participants' age
survey_age <- survey %>%
    to_key_value("demo.age", "DQ1", .)

# Survey participants' education
survey_education <- survey %>%
    to_key_value("demo.education", "DQ2", .)

# Survey participants' gender
survey_gender <- survey %>%
    to_key_value("demo.gender", "DQ3", .)


# ————RQ1 Interoperation————

# IDs for survey respondents who used foreign functions
used_ffi <- survey %>%
    filter(question_id == "UFQ2") %>%
    filter(value == "Calling foreign functions") %>%
    select(response_id) %>%
    unique()

# Number of survey respondents who used foreign functions
num_used_ffi <- used_ffi %>%
    nrow()

# The frequency of languages used by survey respondents who used foreign functions
ffi_languages <- FREQ_ALL %>% 
    filter(question_id == "FFIBQ1") %>%
    mutate(percentage = round(num_responses / num_used_ffi * 100, 1)) %>%
    arrange(desc(percentage))

# The number of participants who ported applications to Rust
num_ported <- survey %>% 
    filter(question_id == "DMQ3") %>%
    group_by(value) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(percentage = round(count / sum(count) * 100, 1))

# The number of participants who ported applications to rust without using foreign functions
num_ported_without_ffi <- survey %>% 
    filter(question_id == "DMQ3") %>%
    filter(value == "Yes") %>%
    select(response_id) %>%
    unique() %>% 
    anti_join(used_ffi, by=c("response_id")) %>%
    nrow()

percent_ported_without_ffi <- num_ported_without_ffi / num_ported %>% filter(value == "Yes") %>% pull(count) * 100

used_containers_ffi <- survey %>%
    filter(question_id == "FFIMMQ1") %>%
    select(response_id) %>%
    unique() %>%
    nrow()

num_used_containers_ffi <- data.frame(
    key = c(
        "rq2.used.containers.count", 
        "rq2.used.containers"
    ), 
    value = c(
        used_containers_ffi,
        used_containers_ffi / num_used_ffi * 100
    ))
    
container_types <- survey %>%
    to_key_value("rq2.container.types", "FFIMMQ1", optional = TRUE, .)

stats <- survey %>%
    to_key_value("rq1.box.move", "BXQ1", .) %>%
    bind_rows(stats)

# ————RQ2 Tooling————

# Dynamic analysis tools
tools <- survey %>%
    to_key_value("rq1.tool", "VQ2", num_responses_override = total_valid, .) %>%
    arrange(desc(value))

num_used_fuzzers <- survey %>%
    filter(question_id == "VQ2") %>%
    filter(value %in% c("cargo fuzz", "libFuzzer", "Loom", "Shuttle")) %>%
    select(response_id) %>%
    unique() %>%
    nrow()

percent_used_fuzzers <- num_used_fuzzers / total_valid * 100

respondents_who_used_miri <- survey %>%
    filter(question_id == "VQ2" & value == "Miri") %>%
    select(response_id)

num_used_miri <- respondents_who_used_miri %>%
    unique() %>%
    nrow()

deterred_by_miri_limitation <- survey %>%
    to_key_value("rq1.miri.deter", "VMQ1", num_responses_override = num_used_miri, .)

num_with_miri_issue <- survey %>% 
    filter(question_id == "VMQ1") %>%
    filter(value %in% c(
        "Slow performance", 
        "Lack of support for foreign function calls", 
        "Lack of support for inline assembly"
    )) %>%
    select(response_id) %>%
    unique() %>%
    nrow()

percent_with_miri_issue <- num_with_miri_issue / num_used_miri * 100

binding_method_counts <- survey %>%
    to_key_value("rq1.binding.method", "FFIBQ2", .)

binding_method_used <- survey %>%
    filter(question_id == "FFIBQ2") %>%
    filter(value != "I do not write or generate bindings") %>%
    to_key_value("rq1.binding.method.subset", "FFIBQ2", .)
    
incorrect_bindings <- survey %>%
    to_key_value("rq1.binding.incorrect", "FFIBQ5", .)

num_used_formal <- survey %>%
    filter(question_id == "VQ1") %>%
    select(response_id) %>%
    unique() %>%
    nrow()

num_used_debugger <- survey %>%
    to_key_value("rq1.debugging", "VQ4", .)

num_used_auditing_tools <- survey %>%
    to_key_value("rq1.auditing.tools", "VQ8", .)

# ————RQ3 Motivations————

used_performance <- survey %>%
    filter(question_id == "WUQ1") %>%
    filter(value == "I could use a safe pattern but unsafe is faster or more space-efficient.") %>%
    select(response_id) %>%
    unique() %>%
    nrow()

performance_scale <- survey %>%
    to_key_value("rq3.perf.scale", "WUPQ4", .)

# ————RQ4 Encapsulation————
random_block_difficulty <- survey %>%
    to_key_value("rq4.understand", "LMUQ1", .) 

exposed_safe <- survey %>%
    to_key_value("rq4.api.safe", "ENQ2", .)

exposed_unsafe <- survey %>%
    to_key_value("rq4.api.unsafe", "ENQ1", .)

exposed_unsafe_motivations <- survey %>%
    to_key_value("rq4.unsafe.api.motivation", "EUAQ1", .)

requirements_beyond_type_system <- survey %>%
    to_key_value("rq4.unsafe.preconditions", "EUAQ2", .)
    
# ————Misc————
unsafe_motivation_other <- read_csv(file.path("./data/community_survey/coding/unsafe.csv"), show_col_types = FALSE)
stats <- unsafe_motivation_other %>%
    separate_rows(Code, sep = ",") %>%
    mutate(Code = str_replace_all(str_to_lower(trimws(Code)), " ", ".")) %>%
    filter(Code != "") %>%
    group_by(Code) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    rename(key = Code, value = count) %>%
    mutate(key = paste0("rq3.motivation.other.", key)) %>%
    bind_rows(stats)
num_unsafe_motivation_other <- unsafe_motivation_other %>% nrow()
stats <- data.frame(
    key = c("rq3.motivation.other.count"), 
    value = c(num_unsafe_motivation_other)
) %>%
    bind_rows(stats)

unsafe_api_motivation_other <- read_csv(
    file.path("./data/community_survey/coding/unsafe_api.csv"),
    show_col_types = FALSE
)
stats <- unsafe_api_motivation_other %>%
    separate_rows(Code, sep = ",") %>%
    mutate(Code = str_replace_all(str_to_lower(trimws(Code)), " ", ".")) %>%
    filter(Code != "") %>%
    group_by(Code) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    rename(key = Code, value = count) %>%
    mutate(key = paste0("rq4.motivation.api.other.", key)) %>%
    bind_rows(stats)

num_unsafe_api_motivation_other <- unsafe_api_motivation_other %>%
    nrow()
    
stats <- data.frame(key = c("rq4.motivation.api.other.count"), value = c(num_unsafe_api_motivation_other)) %>%
    bind_rows(stats)
