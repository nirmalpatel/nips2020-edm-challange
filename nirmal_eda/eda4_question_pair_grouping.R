library(tidyverse)
library(mirt)
library(lubridate)
library(parallel)

source("../load_data.R")
source("../helpers.R")

train_rdf
ansmd_rdf
qmd_df
stumd_rdf
subjmd_rdf

responses_df <- ansmd_rdf %>%
  inner_join(train_rdf)

# user_responses_split <- split(responses_df, responses_df$UserId)

# save(responses_df, user_responses_split, file = "user_responses.Rdata")

# user_questions <- lapply(user_responses_split, function(x) { unique(x$QuestionId) })

user_total_df <- responses_df %>%
  count(UserId) %>%
  arrange(n) %>%
  rename(total = n)

user_questions_df <-responses_df %>%
  count(UserId, QuestionId)

user_questions_sorted_df <- user_questions_df %>%
  inner_join(user_total_df) %>%
  arrange(total, UserId) %>%
  mutate(UserId = factor(UserId, unique(UserId)))

user_questions <- lapply(split(user_questions_sorted_df, as.character(user_questions_sorted_df$UserId)), function(x) { unique(x$QuestionId )})

head(user_questions)

reduce(seq(1, 1000, by = 100), function(x, y) {
  
  idx <- c(y, min(y + 100 - 1, length(user_questions)))
  
  cl <- makeCluster(7)
  clusterEvalQ(cl, library(tidyverse))
  
  user_qpairs <- parLapply(cl, user_questions[idx[1]:idx[2]], function(z) {
    expand.grid(Question1 = z, Question2 = z) %>%
      mutate(n_common_stu = 1)
  })
  
  stopCluster(cl)
  
  ydf <- do.call(rbind, user_qpairs) %>%
    group_by(Question1, Question2) %>%
    summarise(n_common_stu = sum(n_common_stu)) %>%
    ungroup()
  
  rbind(x, ydf) %>%
    group_by(Question1, Question2) %>%
    summarise(n_common_stu = sum(n_common_stu)) %>%
    ungroup()
  
}, .init = data.frame()) %>%
  as_tibble() -> qpair_common_stu


# cl <- makeCluster(7)
# 
# clusterEvalQ(cl, library(tidyverse))
# 
# user_qpairs <- parLapply(cl, user_questions[1:2500], function(x) {
#   
#   expand.grid(Question1 = x, Question2 = x) %>%
#     mutate(n_common_stu = 1)
#   
# })
# 
# lapply(seq(1, length(user_questions), by = 2500), function(x) {
#   
#   c(x, min(x + 2500 - 1, length(user_questions)))
#   
# })
# 
# 
# do.call(rbind, user_qpairs[1:100]) %>%
#   group_by(Question1, Question2) %>%
#   summarise(n_common_stu = sum(n_common_stu)) %>%
#   ungroup() -> all_users_qpair_counts
# 
# reduce(user_qpairs, function(x, y) {
#   
#     bind_rows(x, y) %>%
#     group_by(Question1, Question2) %>%
#     summarise(n_common_stu = sum(n_common_stu)) %>%
#     ungroup()
# 
# }) -> all_users_qpair_counts
# 
# all_users_qpair_counts %>%
#   arrange(desc(n_common_stu))
# 
# save(all_users_qpair_counts, file = "all_users_qpair_counts.Rdata")
# 
# load(all_users_qpair_counts)
