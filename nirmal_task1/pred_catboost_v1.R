library(tidyverse)
library(lubridate)
library(catboost)

source("../load_data.R")
source("../helpers.R")

train_question_split <- split(train_rdf, train_rdf$QuestionId)

lapply(train_question_split, function(x) {
  
  set.seed(680)
  
  x %>%
    sample_n(min(nrow(x), 200))
  
}) %>%
  bind_rows() -> train_sample



lapply(train_question_split, function(x) {
  
})

train_rdf %>%
  count(QuestionId) %>%
  ggplot(aes(n)) +
  geom_histogram()


features <- data.frame(feature1 = c(1, 2, 3), feature2 = as.factor(c('A', 'B', 'C')))
labels <- c(0, 0, 1)
train_pool <- catboost.load_pool(data = features, label = labels)

model <- catboost.train(train_pool,  NULL,
                        params = list(loss_function = 'Logloss',
                                      iterations = 100, metric_period=10))

real_data <- data.frame(feature1 = c(2, 1, 3), feature2 = as.factor(c('D', 'B', 'C')))
real_pool <- catboost.load_pool(real_data)

prediction <- catboost.predict(model, real_pool)
print(prediction)
