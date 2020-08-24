library(tidyverse)
library(lubridate)
library(catboost)

source("../load_data.R")
source("../helpers.R")

# features <- data.frame(feature1 = c(1, 2, 3, 2), feature2 = c('A', 'B', 'C', 'A'))
# labels <- as.integer(as.factor(c("X", "Y", "Z", "X")))
# train_pool <- catboost.load_pool(data = features, label = labels)
# 
# model <- catboost.train(train_pool,  NULL,
#                         params = list(loss_function = 'MultiClass',
#                                       iterations = 100, metric_period=10))
# 
# real_data <- data.frame(feature1 = c(2, 1, 3, 4), feature2 = c('D', 'B', 'C', 'B'))
# real_pool <- catboost.load_pool(real_data)
# 
# prediction <- catboost.predict(model, real_pool, "RawFormulaVal")
# print(prediction)

itemresp_train_rdf <- train_rdf %>%
  inner_join(ansmd_rdf)

avg_correct_df <- itemresp_train_rdf %>%
  group_by(UserId) %>%
  summarise(avg_correct = mean(IsCorrect))

itemresp_train_df <- itemresp_train_rdf %>%
  inner_join(avg_correct_df) %>%
  select(QuestionId, GroupId, QuizId, avg_correct) %>%
  mutate(QuestionId = as.factor(QuestionId),
         GroupId = as.factor(GroupId),
         QuizId = as.factor(QuizId))

itemresp_test_rdf <- submit12_rdf %>%
  inner_join(ansmd_rdf)

itemresp_test_df <- itemresp_test_rdf %>%
  inner_join(avg_correct_df) %>%
  select(QuestionId, GroupId, QuizId, avg_correct) %>%
  mutate(QuestionId = as.factor(QuestionId),
         GroupId = as.factor(GroupId),
         QuizId = as.factor(QuizId))

train_pool <- catboost.load_pool(data = itemresp_train_df, label = as.integer(itemresp_train_rdf$AnswerValue))
test_pool <- catboost.load_pool(data = itemresp_test_df)

fit_params <- list(iterations = 1000,
                   loss_function = 'MultiClassOneVsAll',
                   task_type = 'GPU')

model <- catboost.train(train_pool, NULL, params = fit_params)

prediction <- catboost.predict(model, test_pool)

prediction_answervals <- apply(prediction, 1, which.max)
table(prediction_answervals) 

submit12_rdf %>%
  mutate(AnswerValue = prediction_answervals) %>%
  select(QuestionId, UserId, AnswerValue) %>%
  write_csv("submission_task_2.csv")
