library(tidyverse)
library(caret)
library(doMC)
registerDoMC(cores = 7) 

submit12_rdf <- read_csv("../data/starter_kit/submission_templates/submission_task_1_2.csv") %>%
  select(-1)

# 3 aug -------------------------------------------------------------------

# ensemble of 3 models
# 0.7420590366

# load("pred_clusterquiz_recsys_v1.Rdata")
# load("pred_irt_v1.Rdata")
# load("pred_subj_recsys_respmod_v1.Rdata")
# 
# all_preds <- bind_rows(
#   pred_clusterquiz_recsys_v1,
#   pred_irt_v1,
#   pred_subj_recsys_respmod_v1
# )
# 
# all_preds %>%
#   ggplot(aes(IsCorrectProb)) +
#   geom_histogram() +
#   facet_wrap(~ analysis, ncol = 1)
# 
# ens_preds_df <- all_preds %>%
#   mutate(IsCorrectProb = case_when(
#     IsCorrectProb > 1.0 ~ 1.0,
#     IsCorrectProb < 0.0 ~ 0.0,
#     TRUE ~ IsCorrectProb
#   )) %>%
#   group_by(UserId, QuestionId) %>%
#   summarise(IsCorrectProb = mean(IsCorrectProb)) %>%
#   ungroup() %>%
#   mutate(IsCorrect = if_else(IsCorrectProb > .5, 1, 0)) %>%
#   select(UserId, QuestionId, IsCorrect)
# 
# submit_df <- submit12_rdf %>%
#   left_join(ens_preds_df)
# 
# any(is.na(submit_df))
# nrow(ens_preds_df) == nrow(submit12_rdf)
# 
# write_csv(submit_df, "submission_task_1.csv")

# 9 aug -------------------------------------------------------------------

# first attempt at a bayesglm ensemble

load("pred_subj_recsys_respmod_v1.Rdata")
load("pred_clusterquiz_recsys_v1.Rdata")
load("pred_irt_v1.Rdata")

ens_train_rdf <- pred_train_subj_recsys_respmod_v1 %>%
  inner_join(pred_train_clusterquiz_recsys_v1) %>%
  inner_join(pred_train_irt_v1)

set.seed(100)
ens_train_df <- ens_train_rdf %>%
  sample_n(50000) %>%
  mutate(IsCorrect = factor(if_else(IsCorrect == 1, "Correct", "Incorrect"), levels = c("Correct", "Incorrect")))

# ens_mod <- bayesglm(IsCorrect ~ pred_subj_recsys_respmod_v1 + clusterquiz_recsys_v1 + pred_irt_v1, data = ens_train_df,
#                     family = binomial)
# summary(ens_mod)

set.seed(100)
ens_mod <- train(IsCorrect ~ pred_subj_recsys_respmod_v1 + clusterquiz_recsys_v1 + pred_irt_v1,
                 data = ens_train_df,
                 method = "knn",
                 trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
                 tuneGrid = expand.grid(k = 120))
ens_mod
summary(ens_mod$finalModel)

ens_test_rdf <- bind_rows(
  pred_test_clusterquiz_recsys_v1,
  pred_test_irt_v1,
  pred_test_subj_recsys_respmod_v1
) %>%
  spread(analysis, IsCorrectProb)

mean_impute <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}

ens_test_df <- ens_test_rdf %>%
  mutate_if(~ any(is.na(.)), mean_impute) %>%
  rename(pred_subj_recsys_respmod_v1 = subj_recsys_respmod_v1,
         pred_irt_v1 = irt_v1)

any(is.na(ens_test_df))

nrow(submit12_rdf) == nrow(ens_test_df)

test_preds <- predict(ens_mod, newdata = ens_test_df, type = "prob")

hist(test_preds$Correct)

prop.table(table(ens_train_rdf$IsCorrect))
prop.table(table(test_preds$Correct > .5))

ens_preds_rdf <- ens_test_df %>%
  mutate(IsCorrectProb = test_preds$Correct,
         IsCorrect = if_else(IsCorrectProb > .5, 1, 0)) %>%
  dplyr::select(UserId, QuestionId, IsCorrect)

ens_preds_df <- submit12_rdf %>%
  left_join(ens_preds_rdf)

any(is.na(ens_preds_df))

write_csv(ens_preds_df, "submission_task_1.csv")
