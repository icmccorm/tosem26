source("./scripts/surveys/base.r")

if (file.exists("./build/stats.csv")) {
    file.remove("./build/stats.csv")
}






dir.create("./build", showWarnings = FALSE)


stats <- data.frame(key = character(), value = numeric(), stringsAsFactors = FALSE)

category_frequency_to_key_value <- function(label, df) {
    df %>%
        mutate(group_lower = tolower(str_replace_all(group, " ", "."))) %>%
        mutate(category = tolower(str_replace_all(category, " ", "."))) %>%
        mutate(key = paste(label, group_lower, category, sep = ".")) %>%
        select(key, value)
}

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
        mutate(key = paste(label, tolower(str_replace_all(value, " ", ".")), sep = ".")) %>%
        mutate(value = count) %>%
        select(key, value)
    if (optional) {
        df %>%
            group_by(value) %>%
            summarize(count = n()) %>%
            mutate(key = paste(label, "count", tolower(str_replace_all(value, " ", ".")), sep = ".")) %>%
            mutate(value = count) %>%
            select(key, value) %>%
            bind_rows(transformed)
    } else {
        data.frame(key = paste(label, "count", sep = "."), value = num_responses) %>% bind_rows(transformed)
    }
}


FREQ_ALL %>% inner_join(questions, by=c("question_id")) %>%
    write_csv(file.path("./build/freq.csv"))


# ————COUNTS————

total_valid <- survey %>%
    select(response_id) %>%
    unique() %>%
    nrow()

stats <- screening_eligible %>%
    nrow() %>%
    data.frame(key = "responses.screening.raw", value = .) %>%
    bind_rows(stats)

stats <- screening %>%
    filter(!is.na(ID)) %>%
    nrow() %>%
    data.frame(key = "responses.screening.valid", value = .) %>%
    bind_rows(stats)

stats <- survey_raw %>%
    select(response_id) %>%
    unique() %>%
    nrow() %>%
    data.frame(key = "responses.survey.raw", value = .) %>%
    bind_rows(stats)

stats <- survey_raw %>%
    filter(Finished == TRUE) %>%
    nrow() %>%
    data.frame(key = "responses.survey.finished", value = .) %>%
    bind_rows(stats)

total_valid_before_fraud_detection <- survey_complete_and_elligible %>%
    select(response_id) %>%
    unique() %>%
    nrow()

stats <- stats %>% add_row(key = "responses.survey.without.fraud.detection", value = total_valid_before_fraud_detection)

stats <- survey %>%
    select(response_id) %>%
    unique() %>%
    nrow() %>%
    data.frame(key = "responses.survey.valid", value = .) %>%
    bind_rows(stats)

# ————DEMOGRAPHICS————

stats <- screening %>%
    select(ID, PQ3) %>%
    separate_rows(PQ3, sep = ",") %>%
    mutate(PQ3 = str_to_lower(str_replace_all(PQ3, " ", "."))) %>%
    group_by(PQ3) %>%
    summarize(count = n()) %>%
    mutate(PQ3 = paste0("screening.affiliation.", PQ3)) %>%
    rename(
        key = PQ3,
        value = count
    ) %>%
    bind_rows(stats)


stats <- survey %>%
    to_key_value("demo.age", "DQ1", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("demo.education", "DQ2", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("demo.gender", "DQ3", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("demo.affiliation", "DQ4", .) %>%
    bind_rows(stats)

# ————RQ1————
stats <- survey %>%
    to_key_value("rq1.box.move", "BXQ1", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("box.leak", "BXQ2", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.difficulty", "DMQ6", .) %>%
    bind_rows(stats)

# ----RQ1----
stats <- survey %>%
    to_key_value("rq1.binding.method", "FFIBQ2", .) %>%
    bind_rows(stats)

stats <- survey %>%
    filter(question_id == "FFIBQ2") %>%
    filter(value != "I do not write or generate bindings") %>%
    to_key_value("rq1.binding.method.subset", "FFIBQ2", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.binding.incorrect", "FFIBQ5", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.binding.incorrect.method", "FFIBQ6", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.binding.handwritten.trust", "FFIBQ3", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.binding.generated.trust", "FFIBQ4", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.tests", "VQ6", num_responses_override = total_valid, .) %>%
    arrange(desc(value)) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.tool", "VQ2", num_responses_override = total_valid, .) %>%
    arrange(desc(value)) %>%
    bind_rows(stats)

num_used_formal <- survey %>%
    filter(question_id == "VQ1") %>%
    select(response_id) %>%
    unique() %>%
    nrow()
stats <- data.frame(key = c("rq1.formal.count", "rq1.formal"), value = c(num_used_formal, num_used_formal / total_valid * 100)) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.formal", "VQ1", optional = TRUE, .) %>%
    arrange(desc(value)) %>%
    bind_rows(stats)

num_used_fuzzers <- survey %>%
    filter(question_id == "VQ2") %>%
    filter(value %in% c("cargo fuzz", "libFuzzer", "Loom", "Shuttle")) %>%
    select(response_id) %>%
    unique() %>%
    nrow()
stats <- data.frame(key = c("rq1.fuzzed.count", "rq1.fuzzed"), value = c(num_used_fuzzers, num_used_fuzzers / total_valid * 100)) %>%
    bind_rows(stats)

respondents_who_used_miri <- survey %>%
    filter(question_id == "VQ2" & value == "Miri") %>%
    select(response_id)

num_used_miri <- respondents_who_used_miri %>%
    unique() %>%
    nrow()

stats <- survey %>%
    to_key_value("rq1.miri.deter", "VMQ1", num_responses_override = num_used_miri, .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.miri.test", "VMQ2", num_responses_override = num_used_miri, .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.auditing.frequency", "VQ7", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.auditing.tools", "VQ8", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq1.debugging", "VQ4", .) %>%
    bind_rows(stats)

# ————RQ2————
stats <- survey %>%
    to_key_value("rq2.language", "FFIBQ1", ., optional = TRUE) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq2.adts", "FFIMMQ4", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq2.raw.asref", "FFIMMQ5", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq2.adts.avoided", "FFIMMQ6", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq2.raw.asref.avoided", "FFIMMQ7", .) %>%
    bind_rows(stats)

used_ffi <- survey %>%
    filter(question_id == "UFQ2") %>%
    filter(value == "Calling foreign functions") %>%
    select(response_id) %>%
    unique() %>%
    nrow()

FREQ_ALL %>% 
    filter(question_id == "FFIBQ1") %>%
    mutate(percentage = round(num_responses / used_ffi * 100, 1)) %>%
    arrange(desc(percentage)) %>%
    mutate(value = ifelse(value == "JavaScript", "JS/TS", value)) %>%
    mutate(value = ifelse(value == "TypeScript", "JS/TS", value))


used_containers_ffi <- survey %>%
    filter(question_id == "FFIMMQ1") %>%
    select(response_id) %>%
    unique() %>%
    nrow()
stats <- data.frame(key = c("rq2.used.containers.count", "rq2.used.containers"), value = c(used_containers_ffi, used_containers_ffi / used_ffi * 100)) %>%
    bind_rows(stats)
stats <- survey %>%
    to_key_value("rq2.container.types", "FFIMMQ1", optional = TRUE, .) %>%
    bind_rows(stats)

# ————RQ3————

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
stats <- data.frame(key = c("rq3.motivation.other.count"), value = c(num_unsafe_motivation_other)) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("*.types", "UFQ1", ., optional = TRUE) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("*.feature", "UFQ2", ., optional = TRUE) %>%
    bind_rows(stats)

stats <- performance_arbitrary <- survey %>%
    to_key_value("rq3.perf.vs.safe.any", "PVSQ1", .) %>%
    bind_rows(stats)

stats <- performance_crate <- survey %>%
    to_key_value("rq3.perf.vs.safe.crate", "PVSQ2", .) %>%
    bind_rows(stats)

used_wrapper_for_performance <- survey %>%
    filter(question_id == "WUPQ2") %>%
    filter(value %in% c("UnsafeCell", "MaybeUninit", "ManuallyDrop")) %>%
    select(response_id) %>%
    unique() %>%
    nrow()
used_performance <- survey %>%
    filter(question_id == "WUQ1") %>%
    filter(value == "I could use a safe pattern but unsafe is faster or more space-efficient.") %>%
    select(response_id) %>%
    unique() %>%
    nrow()
stats <- data.frame(key = c("rq3.perf.used.wrapper.count", "rq3.perf.used.wrapper"), value = c(used_wrapper_for_performance, used_wrapper_for_performance / used_performance * 100)) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq3.perf.scale", "WUPQ4", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq3.profiling", "WUPQ3", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq3.impossibility", "WUNQ2", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq3.motivation", "WUQ1", num_responses_override = total_valid, .) %>%
    bind_rows(stats)

# ————RQ4————
stats <- survey %>%
    to_key_value("rq4.api.unsafe", "ENQ1", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.api.safe", "ENQ2", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.understand", "LMUQ1", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.unsafe.api.motivation", "EUAQ1", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.documented.unsafe", "EUAIQ1", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.unsafe.preconditions", "EUAQ2", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.safe.preconditions", "ESAQ2", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.refactored", "LMUQ3", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.avoided", "LMUQ4", .) %>%
    bind_rows(stats)

unsafe_api_motivation_other <- read_csv(file.path("./data/community_survey/coding/unsafe_api.csv"), show_col_types = FALSE)
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
num_unsafe_api_motivation_other <- unsafe_api_motivation_other %>% nrow()
stats <- data.frame(key = c("rq4.motivation.api.other.count"), value = c(num_unsafe_api_motivation_other)) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.unsafecell", "USC1", .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.perception.yours", "CCQ3", num_responses_override = total_valid, .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.perception.general", "CCQ2", num_responses_override = total_valid, .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.adequate", "CCQ4", num_responses_override = total_valid, .) %>%
    bind_rows(stats)

stats <- survey %>%
    to_key_value("rq4.forum", "CCQ1", .) %>%
    bind_rows(stats)


stats %>%
    pivot_wider(names_from = key) %>%
    write.table(file = file.path("./build/", "stats.csv"), quote = TRUE, sep = ",", row.names = FALSE)
