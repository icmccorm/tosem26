source("./scripts/surveys/base.r")

SHORT_ANSWER="short_answer"
CONSENT="consent"
MC="mc"
YN="yn"
YNU="ynu"

survey_mc <- survey %>% 
    inner_join(questions) %>%
    filter(type == "mc") %>%
    mutate(question_id = paste0(question_id, "-", value)) %>%
    
population <- survey %>% 
    inner_join(questions, by=c("question_id")) %>%
    mutate(wrote = ifelse(response_id %in% wrote_participants$response_id, 1, 0)) %>%
    group_by(question_id) %>%
    group_split() %>%
    lapply(function(df) {
        is_mc <- df$type == "mc"
        print()
        print(df)
        question_id <- unique(df$question_id)
        df <- df %>%
            mutate(value = paste0(question_id, "-", value)) %>%
            mutate(value = as.numeric(as.factor(value))) %>%
            select(wrote, value)
        l <- df %>% filter(wrote == 0)
        r <- df %>% filter(wrote == 1)
        n_wrote <- nrow(r)
        n_not_wrote <- nrow(l)
        if (length(unique(l$value)) == 1 | length(unique(r$value)) == 1 | length(unique(df$wrote)) == 1) {
            pvalue <- NA
        }else{
            pvalue <- t.test(value ~ wrote, data = df)$p.value
        }
        data.frame(
            question_id = question_id,
            pvalue = pvalue,
            n_wrote = n_wrote,
            n_not_wrote = n_not_wrote
        )
    }) %>% 
    bind_rows()

population %>% inner_join(questions, by=c("question_id")) %>%
    filter(pvalue <= 0.06) %>% 
    write_csv(file.path("./p.csv"))
