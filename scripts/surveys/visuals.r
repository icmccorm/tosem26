source("./scripts/surveys/base.r")
result_dir <- file.path("./build/figures/")
figure_tables <- file.path(result_dir, "tables")

unlink(result_dir)
survey %>% select(response_id) %>% unique() %>% nrow()
if (!dir.exists(result_dir)) {
  dir.create(result_dir, recursive = TRUE)
}

if (!dir.exists(figure_tables)) {
  dir.create(figure_tables, recursive = TRUE)
}

to_table <- function (df) {
    df <- df %>% select(type, rq, question, figure)
    vtype <- unique(df$type)[[1]]
    rq <- unique(df$rq)[[1]]
    out_path <- file.path(figure_tables, paste0(vtype,"_",rq,".tex"))
    xt <- df %>%
        ungroup() %>%
        select(question, figure) %>%
        xtable()
    print(xt,
      file = out_path,
      sanitize.text.function = identity,
      include.rownames = FALSE
    )
}

PLOT_CONFIG <- list(
    list(
        type = "freq",
        choices = frequency_choices,
        center = frequency_neutral
    ),
    list(
        type = "freq_unsure",
        choices = frequency_choices_unsure,
        center = frequency_unsure_neutral
    ),
    list(
        type = "yes_no_grad",
        choices = yes_no_choices,
        center = c()
    ),
    list(
        type = "freq_time",
        choices = date_frequency_choices,
        center = c()
    )
)

plots <- PLOT_CONFIG %>%
    lapply(function(config) {
        questions %>%
            filter(type == config$type) %>%
            pmap(function(rq, type, optional, gated_by, question_id, question) {
                plot_id <- paste0(rq, "_", question_id)
                qid <- question_id
                total_num_responses <- FREQ_ALL %>% filter(question_id == qid) %>% pull(total_num_responses)
                df <- compute_likert_frequency(FREQ_ALL, question_id, config$choices)
                figure <- if (length(colnames(df)) == 7) {
                    paste0("\\tzplotbarsunsure{",
                        df[[2]][1],
                        "}{", df[[3]][1],
                        "}{", df[[4]][1],
                        "}{", df[[5]][1],
                        "}{", df[[6]][1],
                        "}{", df[[7]][1],
                        "}{", total_num_responses,
                    "}")
                }else{
                    paste0("\\tzplotbars{",
                        df[[2]][1],
                        "}{", df[[3]][1],
                        "}{", df[[4]][1],
                        "}{", df[[5]][1],
                        "}{", df[[6]][1],
                        "}{", total_num_responses,
                    "}")
                }
                data.frame(
                    rq = rq,
                    type = type,
                    question = question,
                    figure = figure
                )
           }) %>%
        bind_rows()
    }) %>%
    bind_rows() %>%
    unique() %>%
    group_by(type, rq) %>%
    group_split() %>%
    lapply(to_table)

quickplot <- function(subset, name, question, choices, center = c()) {
    compute_likert_frequency(compute_frequency(subset), question, choices)
}

respondents_who_used_miri <- survey %>%
    filter(question_id == "VQ2" & value == "Miri") %>%
    select(response_id) %>%
    unique() %>%
    quickplot("2_miri", "VQ6", frequency_choices, frequency_neutral)

respondents_who_used_auditing_tools <- survey %>%
    filter(question_id == "VQ8", !is.na(value)) %>%
    select(response_id) %>%
    unique() %>%
    quickplot("2_auditing_tools", "VQ7", frequency_choices, frequency_neutral)

respondents_who_used_unsafe_apis <- survey %>%
    filter(question_id == "UFQ2", value %in% c("Calling unsafe functions written in Rust", "Calling foreign functions")) %>%
    select(response_id) %>%
    unique() %>%
    quickplot("4_unsafe_apis", "LMUQ4", frequency_choices, frequency_neutral)

respondents_who_regularly_wrote <- survey %>%
    filter(question_id == "BQ6" & value == "Yes") %>%
    select(response_id) %>%
    unique() %>%
    quickplot("4_regularly_wrote", "LMUQ3", frequency_choices, frequency_neutral)
