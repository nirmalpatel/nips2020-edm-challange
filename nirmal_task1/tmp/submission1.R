library(tidyverse)
library(recosystem)

# fixing errors in the first submission

train_rdf <- read_csv("../data/train_data/train_task_1_2.csv")
ansmd_rdf <- read_csv("../data/metadata/answer_metadata_task_1_2.csv")
qmd_rdf <- read_csv("../data/metadata/question_metadata_task_1_2.csv")
stumd_rdf <- read_csv("../data/metadata/student_metadata_task_1_2.csv")
subjmd_rdf <- read_csv("../data/metadata/subject_metadata.csv")

submit_rdf <- read_csv("../data/starter_kit/submission_templates/submission_task_1_2.csv")

qmd_df <- map2(qmd_rdf$QuestionId, qmd_rdf$SubjectId, function(x, y) {
  
  y_clean <- y %>%
    str_remove("\\[") %>%
    str_remove("\\]")
  
  subjects <- as.numeric(strsplit(y_clean, ", ")[[1]])
  
  data.frame(QuestionId = rep(x, length(subjects)),
             SubjectId = subjects)
  
}) %>%
  bind_rows() %>%
  as_tibble()

# 

qdiff_rdf <- train_rdf %>%
  group_by(QuestionId) %>%
  summarise(q_diff = mean(IsCorrect),
            n_resp = n()) %>%
  ungroup()

qdiff_rdf %>%
  ggplot(aes(q_diff)) +
  geom_histogram()

qdiff_rdf %>%
  ggplot(aes(log(n_resp))) +
  geom_histogram()

qdiff_rdf %>%
  inner_join(qmd_df)

subj_scores_rdf <- train_rdf %>%
  select(UserId, QuestionId, IsCorrect) %>%
  inner_join(qmd_df)

subj_scores_df <- subj_scores_rdf %>%
  group_by(UserId, SubjectId) %>%
  summarise(subj_score = mean(IsCorrect),
            subj_n_resp = n()) %>%
  ungroup()

# we build a response model that predicts student outcome
# from student's subject score, difficulty, and # responses to the item

response_mod_rdf <- subj_scores_rdf %>%
  inner_join(subj_scores_df)

response_mod_df <- response_mod_rdf %>%
  group_by(UserId, QuestionId, IsCorrect) %>%
  summarise(avg_subj_score = mean(subj_score)) %>%
  inner_join(qdiff_rdf) %>%
  ungroup()

response_mod_train_df <- response_mod_df %>%
  sample_n(30000)

response_mod <- lm(IsCorrect ~ avg_subj_score + q_diff, data = response_mod_train_df)
summary(response_mod)

# matrices for recosystem

subj_scores_wide_df <- subj_scores_df %>%
  select(UserId, SubjectId, subj_score) %>%
  spread(SubjectId, subj_score)

subj_scores_ord_df <- subj_scores_df %>%
  arrange(UserId, SubjectId)

subj_ords <- setNames(1:length(unique(subj_scores_ord_df$SubjectId)), sort(unique(subj_scores_ord_df$SubjectId)))
user_ords <- setNames(1:length(unique(subj_scores_ord_df$UserId)), sort(unique(subj_scores_ord_df$UserId)))

submit_df <- submit_rdf %>%
  select(-1) %>%
  inner_join(qmd_df)

# recosystem package

reco <- Reco()

reco$train(data_memory(user_ords[as.character(subj_scores_ord_df$UserId)], subj_ords[as.character(subj_scores_ord_df$SubjectId)], subj_scores_ord_df$subj_score),
        opts = list(dim = 20, costp_l2 = 0.01, costq_l2 = 0.01, nthread = 7)
)

pred <- reco$predict(data_memory(user_ords[as.character(submit_df$UserId)], subj_ords[as.character(submit_df$SubjectId)]), out_memory())

submit_df_upload <- submit_df %>%
  mutate(predval = pred) %>%
  group_by(UserId, QuestionId) %>%
  summarise(IsCorrect = if_else(mean(predval, na.rm = TRUE) >= .50, 1, 0)) %>%
  ungroup()

submit_df_export <- submit_rdf %>%
  select(-1) %>%
  inner_join(submit_df_upload)

sum(is.na(submit_df_export))

write_csv(submit_df_export, "submission_task_1.csv")
