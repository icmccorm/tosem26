source("scripts/surveys/base.r")
output_file <- file.path("build", "appendix.tex")
if (file.exists(output_file)) {
    file.remove(output_file)
}
dir.create("./build", showWarnings = FALSE)

write_tex <- function(text) {
    cat(text, file = output_file, append = TRUE)
}

counts <- survey %>%
    inner_join(questions, by = "question_id") %>%
    group_by(question_id, question, value) %>%
    summarize(n = n(), .groups = "keep") %>%
    ungroup()

sections <- sections %>% mutate(order = row_number())
counts$section_id <- gsub("_[0-9]+(_[A-Z]+)?", "", counts$question_id)
counts$question_order <- gsub("[A-Z]+", "", counts$section_id)
counts$section_id <- gsub("[0-9]+(_[A-Z]+)?", "", counts$section_id)

counts <- counts %>% inner_join(sections, by = c("section_id"))

counts <- counts %>% filter(!grepl("- Other - Text", question))
counts$question <- gsub(" - Selected Choice", "", counts$question)
counts <- counts %>% arrange(order)

for (curr_section_id in unique(counts$section_id)) {
    section_text <- sections %>%
        filter(section_id == curr_section_id) %>%
        pull(section)
    write_tex(paste0("\\subsubsection{", section_text, "}\n"))
    write_tex(paste0("\\label{survey:sec:", tolower(curr_section_id), "}\n\n"))
    write_tex(paste0("\\begin{protocol}\n"))

    counts_in_section <- counts %>% filter(section_id == curr_section_id)
    for (curr_question_id in unique(counts_in_section$question_id)) {
        curr_question_text <- counts_in_section %>%
            filter(question_id == curr_question_id) %>%
            select(question, question_id) %>%
            unique() %>%
            pull(question)
        write_tex(paste0("\\item \\label{survey:", str_to_lower(curr_question_id), "} ", curr_question_text, "\n"))

        counts_in_question <- counts_in_section %>%
            filter(question_id == curr_question_id) %>%
            select(value, n) %>%
            arrange(desc(n))

        predefined_lists <- list(
            positivity_choices = positivity_choices,
            frequency_choices = frequency_choices,
            frequency_choices_unsure = frequency_choices_unsure,
            agreement_choices = agreement_choices,
            age_choices = age_choices,
            gender_choices = gender_choices,
            affiliation_choices = affiliation_choices,
            education_choices = education_choices,
            difficulty_choices = difficulty_choices,
            yes_no_choices = yes_no_choices,
            yes_no_binary_choices = yes_no_binary_choices,
            date_frequency_choices = date_frequency_choices,
            likely_unlikely_choices = likely_unlikely_choices
        )

        matching_list_name <- NULL
        for (list_name in names(predefined_lists)) {
            n_items <- length(predefined_lists[[list_name]])
            n_matching_items <- unique(counts_in_question$value) %>% length()
            if (n_items == n_matching_items) {
                if (all(sort(unique(counts_in_question$value)) == sort(predefined_lists[[list_name]]))) {
                    matching_list_name <- list_name
                    break
                }
            }
        }

        if (!is.null(matching_list_name)) {
            matching_list <- predefined_lists[[matching_list_name]]
            counts_in_question <- counts_in_question[order(match(counts_in_question$value, matching_list)), ]
        }
        
        counts_in_question <- counts_in_question %>%
            select(value, n) %>%
            unique() %>%
            filter(!is.na(value))

        apply_item_format <- function(df) {
            max_digits <- max(nchar(df$n))
            df %>%
                mutate(n_formatted = paste0("\\texttt{(", str_pad(paste0(n), max_digits, "left"), ")}")) %>%
                mutate(n_formatted = gsub(" ", "\\\\space ", n_formatted)) %>%
                mutate(value_formatted = paste0("\\item ", n_formatted, " - \\textit{", value, "}\n")) %>%
                pull(value_formatted)
        }

        if (nrow(counts_in_question) > 6) {
            # split into two columns
            first_column <- counts_in_question[1:ceiling(nrow(counts_in_question) / 2) - 1, ]
            second_column <- counts_in_question[(ceiling(nrow(counts_in_question) / 2)):nrow(counts_in_question), ]
            items <- c(apply_item_format(first_column), apply_item_format(second_column))
        } else {
            items <- apply_item_format(counts_in_question)
        }

        if (length(items) > 6) {
            write_tex("\\begin{multicols}{2}\n")
        }
        write_tex("\\begin{itemize}\n")
        for (item in items) {
            write_tex(item)
        }
        write_tex("\\end{itemize}\n")
        if (length(items) > 6) {
            write_tex("\\end{multicols}\n")
        }
    }
    write_tex(paste("\\end{protocol}\n"))
    write_tex("\n")
}
