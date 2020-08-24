library(tidyverse)
library(recosystem)

source("../load_data.R")

train_rdf %>%
  inner_join(qmd_max_level_df) %>%
  group_by(QuestionId) %>%
  mutate(zscore = as.numeric(scale(IsCorrect))) %>%
  ungroup() %>%
  group_by(UserId, SubjectId) %>%
  summarise(subj_score = mean(zscore, na.rm = TRUE)) -> subj_scores_df

subj_scores_df %>%
  filter(SubjectId <= 57) %>%
  ggplot(aes(subj_score)) +
  geom_histogram() +
  facet_wrap(~ SubjectId, scales = "free_y")

subj_scores_wide_df <- subj_scores_df %>%
  select(UserId, SubjectId, subj_score) %>%
  spread(SubjectId, subj_score)

subj_scores_ord_df <- subj_scores_df %>%
  arrange(UserId, SubjectId)

user_ords <- setNames(1:length(unique(subj_scores_ord_df$UserId)), sort(unique(subj_scores_ord_df$UserId)))
subj_ords <- setNames(1:length(unique(subj_scores_ord_df$SubjectId)), sort(unique(subj_scores_ord_df$SubjectId)))

user_subj_combs_df <- expand.grid(UserId = sort(unique(subj_scores_ord_df$UserId)),
                                  SubjectId = sort(unique(subj_scores_ord_df$SubjectId))) %>%
  as_tibble()

# recosystem package

reco <- Reco()

reco$train(data_memory(user_ords[as.character(subj_scores_ord_df$UserId)], subj_ords[as.character(subj_scores_ord_df$SubjectId)], subj_scores_ord_df$subj_score),
           opts = list(dim = 40, costp_l2 = 0.01, costq_l2 = 0.01, nthread = 7, niter = 50)
)

train_pred <- reco$predict(data_memory(user_ords[as.character(user_subj_combs_df$UserId)], subj_ords[as.character(user_subj_combs_df$SubjectId)]), out_memory())

user_subj_combs_df %>%
  mutate(pred_subj_score = train_pred) -> user_subj_scores_full_df

user_subj_scores_full_df %>%
  spread(SubjectId, pred_subj_score) -> user_subj_scores_full_wide_df

train_rdf %>%
  select(UserId, QuestionId, AnswerValue) %>%
  inner_join(user_subj_scores_full_wide_df) -> train_skillprofile_rdf
