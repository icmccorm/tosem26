source("./scripts/surveys/base.r")
result_dir <- file.path("./build/figures/")
legend_dir <- file.path(result_dir, "legends")
figure_tables <- file.path(result_dir, "tables")


unlink(result_dir)
survey %>% select(response_id) %>% unique() %>% nrow()
if (!dir.exists(result_dir)) {
  dir.create(result_dir, recursive = TRUE)
  dir.create(legend_dir, recursive = TRUE)
}

if (!dir.exists(legend_dir)) {
  dir.create(legend_dir, recursive = TRUE)
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


survey %>% filter(question_id == "WUQ1") %>% group_by(response_id) %>% summarize(n=n()) %>%
    filter(n > 1) %>% nrow()


plots <- PLOT_CONFIG %>% 
    lapply(function(config) {
        questions %>% 
            filter(type == config$type) %>% 
            pmap(function(rq, type, optional, gated_by, question_id, question) {
                plot_id <- paste0(rq, "_", question_id)
                qid <- question_id
                total_num_responses <- FREQ_ALL %>% filter(question_id == qid) %>% pull(total_num_responses)
                df <- compute_likert_frequency(FREQ_ALL, question_id, config$choices)
                labels <- plot_likert_bar(
                    df = df,
                    total_num_responses = total_num_responses,
                    options = config$choices,
                    path = file.path(result_dir, paste0(plot_id, ".pdf")),
                    center = config$center,
                    extract_legend = file.path(legend_dir, paste0("legend_", type, ".pdf"))
                )
                data.frame(
                    rq = rq,
                    type = type,
                    question = question,
                    figure = paste0("\\plotbar{figures/",
                        plot_id, 
                        "}{", labels[[1]],
                        "}{", labels[[2]],
                        "}{", labels[[3]],
                        "}"
                    )
                )
           }) %>%
        bind_rows()
    }) %>%
    bind_rows() %>%
    group_by(type, rq) %>%
    group_split() %>%
    lapply(to_table)

quickplot <- function(subset, name, question, choices, center = c()) {
    total_num_responses <- subset %>% nrow()
    frequency <- compute_frequency(subset)
    likert_frequency <- compute_likert_frequency(frequency, question, choices)
    labels <- plot_likert_bar(
        df = likert_frequency,
        total_num_responses = total_num_responses,
        options = choices,
        path = file.path(result_dir, paste0(name, "_", question, ".pdf")),
        center = center
    )
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