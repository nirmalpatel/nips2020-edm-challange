library(tidyverse)
library(lubridate)
library(recosystem)

load("../data/quiz_clusters.Rdata")

source("../load_data.R")
source("../helpers.R")

train_rdf
ansmd_rdf
qmd_df
stumd_rdf
subjmd_rdf

responses_df <- ansmd_rdf %>%
  inner_join(train_rdf)

quiz_clusters

responses_clust_df <- responses_df %>%
  inner_join(quiz_clusters)

cluster_stats_df <- responses_clust_df %>%
  group_by(ClusterId, QuestionId) %>%
  summarise(n_correctlevs = n_distinct(IsCorrect),
            n_resplevs = n_distinct(AnswerValue),
            n_stu = n_distinct(UserId)) %>%
  ungroup()

# looping over 3000 cluster quizzes
lapply(1:3000, function(x) {
  
  # x <- 1
  
  if (x %% 100 == 0) cat(x, "\n")
  
  # responses_clust_df %>%
  #   filter(ClusterId == 3000) %>%
  #   select(UserId, QuestionId, IsCorrect) %>%
  #   spread(QuestionId, IsCorrect) %>%
  #   View()
  
  cluster_data <- responses_clust_df %>%
    filter(ClusterId == x) %>%
    arrange(UserId, QuestionId)
  
  cluster_test_data <- submit12_rdf %>%
    inner_join(quiz_clusters, by = "QuestionId") %>%
    filter(ClusterId == x, UserId %in% cluster_data$UserId)
  
  question_ords <- setNames(1:length(unique(cluster_data$QuestionId)), sort(unique(cluster_data$QuestionId)))
  user_ords <- setNames(1:length(unique(cluster_data$UserId)), sort(unique(cluster_data$UserId)))
  
  reco <- Reco()
  
  reco$train(data_memory(user_ords[as.character(cluster_data$UserId)], question_ords[as.character(cluster_data$QuestionId)], cluster_data$IsCorrect),
             opts = list(dim = 2, costp_l2 = 0.01, costq_l2 = 0.01, nthread = 7, verbose = FALSE)
  )
  
  train_pred <- reco$predict(data_memory(user_ords[as.character(cluster_data$UserId)], question_ords[as.character(cluster_data$QuestionId)]), out_memory())
  test_pred <- reco$predict(data_memory(user_ords[as.character(cluster_test_data$UserId)], question_ords[as.character(cluster_test_data$QuestionId)]), out_memory())
  
  train_pred_df <- cluster_data %>%
    select(UserId, QuestionId, IsCorrect) %>%
    mutate(clusterquiz_recsys_v1 = train_pred)
  
  test_pred_df <- cluster_test_data %>%
    mutate(pred = test_pred)
  
  list(train_pred_df, test_pred_df)
  
}) -> res

pred_train_clusterquiz_recsys_v1 <- lapply(res, function(x) x[[1]]) %>%
  bind_rows()

pred_test_clusterquiz_recsys_v1 <- lapply(res, function(x) x[[2]]) %>%
  bind_rows() %>%
  select(-ClusterId) %>%
  rename(IsCorrectProb = pred) %>%
  mutate(analysis = "clusterquiz_recsys_v1")

save(pred_train_clusterquiz_recsys_v1, pred_test_clusterquiz_recsys_v1, file = "pred_clusterquiz_recsys_v1.Rdata")

# summary(responses_df$IsCorrect)
# 
# submit12_s2_df <- read_csv("submission2_res.csv")
# 
# summary(submit12_s2_df$IsCorrectProb)
# 
# summary(res_df$pred)
# 
# predprob_avg_df <- bind_rows(
#   submit12_s2_df,
#   res_df
# ) %>%
#   group_by(UserId, QuestionId) %>%
#   summarise(IsCorrectProb = mean(IsCorrectProb)) %>%
#   ungroup()
# 
# summary(predprob_avg_df$IsCorrectProb)
# 
# quantile(predprob_avg_df$IsCorrectProb, .357)
# 
# res_submit_df <- predprob_avg_df %>%
#   mutate(IsCorrect = if_else(IsCorrectProb > .45, 1, 0)) %>%
#   select(UserId, QuestionId, IsCorrect)
# 
# prop.table(table(res_submit_df$IsCorrect))
# 
# # write_csv(res_submit_df, "submission_task_1.csv")
# 
# # submit12_s2_df
# # 
# # mixed_submit_df <- res_submit_df %>%
# #   bind_rows(anti_join(submit12_s2_df, select(res_submit_df, 1, 2)))
# #
# 
# task1_s3_submit_df <- submit12_rdf %>%
#   left_join(res_submit_df)
#  
# # any(is.na(task1_s3_submit_df))
# # 
# write_csv(task1_s3_submit_df, "submission_task_1.csv")
# # 
# # table(new = task1_s3_submit_df$IsCorrect, old = submit12_s2_df$IsCorrect)
