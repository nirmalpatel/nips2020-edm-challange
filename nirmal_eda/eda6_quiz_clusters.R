library(tidyverse)
library(mirt)
library(lubridate)
library(recosystem)

load("quiz_clusters.Rdata")

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

responses_clust_df %>%
  filter(ClusterId == 3000) %>%
  select(UserId, QuestionId, IsCorrect) %>%
  spread(QuestionId, IsCorrect) %>%
  View()

cluster_data <- responses_clust_df %>%
  filter(ClusterId == 3000) %>%
  arrange(UserId, QuestionId)

cluster_test_data <- submit12_rdf %>%
  inner_join(quiz_clusters) %>%
  filter(ClusterId == 3000, UserId %in% cluster_data$UserId)

question_ords <- setNames(1:length(unique(cluster_data$QuestionId)), sort(unique(cluster_data$QuestionId)))
user_ords <- setNames(1:length(unique(cluster_data$UserId)), sort(unique(cluster_data$UserId)))

reco <- Reco()

reco$train(data_memory(user_ords[as.character(cluster_data$UserId)], question_ords[as.character(cluster_data$QuestionId)], cluster_data$IsCorrect),
           opts = list(dim = 20, costp_l2 = 0.01, costq_l2 = 0.01, nthread = 7)
)

pred <- reco$predict(data_memory(user_ords[as.character(cluster_test_data$UserId)], question_ords[as.character(cluster_test_data$QuestionId)]), out_memory())

cluster_test_data %>%
  mutate(pred = pred)
