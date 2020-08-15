# in this submission we try to look at most recent scores for each skill associated with the item
# and use those scores to predict the question
# we take last 3 responses of the user, if 2 or more were correct, we go for correct, otherwise we go for incorrect

# adding more features can help too

# trying to build a predictive models with these features
# qdiff
# n_responses

# ------------ this file ----------------

# simple idea
# make a model that takes
# agg_skill_score
# qdiff
# and predicts the outcome, use that in the final stage

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

set.seed(100)

response_mod_train_df <- response_mod_df

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
           opts = list(dim = 40, costp_l2 = 0.01, costq_l2 = 0.01, nthread = 7, niter = 50)
)

# train_pred <- reco$predict(data_memory(user_ords[as.character(subj_scores_ord_df$UserId)], subj_ords[as.character(subj_scores_ord_df$SubjectId)]), out_memory())
val_pred <- reco$predict(data_memory(user_ords[as.character(submit_df$UserId)], subj_ords[as.character(submit_df$SubjectId)]), out_memory())
  
train_rdf %>%
  select(UserId, QuestionId, IsCorrect) %>%
  inner_join(qmd_df)

submit_predictors_df <- submit_df %>%
  mutate(predval = val_pred) %>%
  group_by(UserId, QuestionId) %>%
  summarise(avg_subj_score = mean(predval, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(qdiff_rdf)

submit_predictors_df

iscorrect_pred <- predict(response_mod, newdata = submit_predictors_df)

pred_train_subj_recsys_respmod_v1 <- response_mod_df %>%
  select(UserId, QuestionId, IsCorrect) %>%
  mutate(pred_subj_recsys_respmod_v1 = predict(response_mod))
  
pred_test_subj_recsys_respmod_v1 <- submit_predictors_df %>%
  mutate(IsCorrectProb = iscorrect_pred) %>%
  select(UserId, QuestionId, IsCorrectProb) %>%
  mutate(analysis = "subj_recsys_respmod_v1")

save(pred_train_subj_recsys_respmod_v1, pred_test_subj_recsys_respmod_v1, file = "pred_subj_recsys_respmod_v1.Rdata")

# submit_df_results <- submit_predictors_df %>%
#   mutate(IsCorrectProb = iscorrect_pred,
#          IsCorrect = if_else(IsCorrectProb > .5, 1, 0)) %>%
#   select(UserId, QuestionId, IsCorrectProb)
# 
# write_csv(submit_df_results, "submission2_res.csv")
# 
# submit_df_export <- submit_rdf %>%
#   select(-1) %>%
#   inner_join(submit_df_results)
# 
# submit_df_last <- read_csv("submission_task_1_last.csv")
# 
# table(submit_df_export$IsCorrect, submit_df_last$IsCorrect)
# 
# write_csv(submit_df_export, "submission_task_1.csv")
