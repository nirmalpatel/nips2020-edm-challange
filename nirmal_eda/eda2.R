library(tidyverse)
library(mirt)
library(lubridate)

source("../load_data.R")
source("../helpers.R")

train_rdf
ansmd_rdf
qmd_df
stumd_rdf
subjmd_rdf

# quiz_stats_df <- ansmd_rdf %>%
#   inner_join(select(train_rdf, UserId, QuestionId, AnswerId)) %>%
#   group_by(QuizId, SchemeOfWorkId, GroupId) %>%
#   summarise(n = n(), nq = n_distinct(QuestionId), nu = n_distinct(UserId))
# save(quiz_stats_df, file = "quiz_stats_df.Rdata")

load("quiz_stats_df.Rdata")

quiz_stats_df

quizid <- 13139

onequiz_df <- ansmd_rdf %>%
  filter(QuizId == quizid) %>%
  inner_join(filter(train_rdf, AnswerId %in% filter(ansmd_rdf, QuizId == quizid)$AnswerId))

n_distinct(onequiz_df$QuestionId)
n_distinct(onequiz_df$UserId)

# finding inter question similarity by calculating the # of common students
# between every pair of questions
quiz_simmat_df <- onequiz_df %>%
  select(QuestionId, UserId) %>%
  rename(Question1 = QuestionId) %>%
  inner_join(onequiz_df %>%
               select(UserId, QuestionId) %>%
               rename(Question2 = QuestionId)) %>%
  group_by(Question1, Question2) %>%
  summarise(n_common_stu = n_distinct(UserId)) %>%
  ungroup() %>%
  spread(Question2, n_common_stu, fill = 0)
  
quiz_simmat <- quiz_simmat_df %>%
  select(-1) %>%
  as.matrix()

rownames(quiz_simmat) <- colnames(quiz_simmat)

quiz_dsimmat <- max(quiz_simmat) - quiz_simmat

hclust_mod <- hclust(as.dist(quiz_dsimmat), method = "ward.D")

plot(hclust_mod)

clust_assigments <- cutree(hclust_mod, k = 9)

quiz_clusters <- tibble(
  QuestionId = as.numeric(names(clust_assigments)),
  ClusterId = as.numeric(clust_assigments)
) %>%
  arrange(ClusterId)

quiz_clusters %>%
  count(ClusterId) 

# item analysis for a single cluster

onequiz_df %>%
  inner_join(quiz_clusters) %>%
  filter(ClusterId == 3) %>%
  select(UserId, QuestionId, IsCorrect) %>%
  spread(QuestionId, IsCorrect) %>%
  select(-1) %>%
  as.matrix() -> itemresp_mat

item_mod <- mirt(itemresp_mat, 1, "2PL")

coef(item_mod, IRTpars = T, simplify = T)

# how to get question similarity matrix for a big number of questions?

bigquiz_df <- ansmd_rdf %>%
  filter(QuizId == 2210) %>%
  inner_join(filter(train_rdf, AnswerId %in% filter(ansmd_rdf, QuizId == 2210)$AnswerId))

bigquiz_skill_df <- bigquiz_df %>%
  inner_join(qmd_all_level_df)

bigquiz_skill_dfs_split <- split(bigquiz_skill_df, bigquiz_skill_df$SubjectParent2)

bigquiz_skill_dfs_split

get_question_responses(5950)

get_user_responses(75830)

bigquiz_df %>%
  count(GroupId)

get_question_responses(5950) %>%
  count(QuizId)

get_quiz_responses(1379) %>%
  arrange(DateAnswered)

submit12_rdf %>%
  filter(UserId == 59208)

get_group_responses(6244)

get_quiz_responses(189) %>%
  count(QuestionId)




ansmd_rdf

ansmd_rdf

# bigquiz_df %>%
#   group_by(UserId) %>%
#   summarise(nq = n_distinct(QuestionId)) %>%
#   ungroup() %>%
#   summary()
# 
# bigquiz_df %>%
#   count(UserId) %>%
#   arrange(n) %>%
#   pull(UserId) -> all_users
# 
# reduce(all_users, function(x, y) {
#   
#   # x is a df
#   # y is a uid
#   cat(y)
#   cat("\n")
#   all_questions <- bigquiz_df %>%
#     filter(UserId == y) %>%
#     pull(QuestionId) %>%
#     unique()
#   
#   crossing(Question1 = all_questions, Question2 = all_questions) %>%
#     mutate(n_common_stu = 1) %>%
#     bind_rows(x) %>%
#     group_by(Question1, Question2) %>%
#     summarise(n_common_stu = sum(n_common_stu)) %>%
#     ungroup()
#   
# }, .init = drop_na(tibble(Question1 = NA_character_, Question2 = NA_character_))) -> all_users_qpair_counts
# 
# save(all_users_qpair_counts, file = "all_users_qpair_counts.Rdata")

load(all_users_qpair_counts)

all_users_qpair_counts

all_users_qpair_counts %>%
  arrange(desc(n_common_stu))

bigquiz_simmat_df <- all_users_qpair_counts %>%
  spread(Question2, n_common_stu, fill = 0)

bigquiz_simmat <- bigquiz_simmat_df %>%
  select(-1) %>%
  as.matrix()

rownames(bigquiz_simmat) <- colnames(bigquiz_simmat)

bigquiz_dsimmat <- max(bigquiz_simmat) - bigquiz_simmat

hclust_mod <- hclust(as.dist(bigquiz_dsimmat), method = "ward.D")
hclust_mod_exp <- hclust(as.dist(bigquiz_dsimmat), method = "ward.D2")

plot(hclust_mod_exp, labels = FALSE)

clust_assigments <- cutree(hclust_mod_exp, k = 1500)

bigquiz_clusters <- tibble(
  QuestionId = as.numeric(names(clust_assigments)),
  ClusterId = as.numeric(clust_assigments)
) %>%
  arrange(ClusterId)

bigquiz_clusters %>%
  count(ClusterId) %>%
  arrange(n)

bigquiz_clusters %>%
  count(ClusterId) %>%
  arrange(desc(n))

bigquiz_clusters %>%
  count(ClusterId) %>%
  filter(n == 10)

bigquiz_df %>%
  inner_join(bigquiz_clusters) %>%
  filter(ClusterId == 49) %>%
  select(UserId, QuestionId, IsCorrect) %>%
  spread(QuestionId, IsCorrect) %>%
  select(-1) %>%
  as.matrix() -> itemresp_mat

item_mod <- mirt(itemresp_mat, 1, "2PL")

coef(item_mod, IRTpars = T, simplify = T)

bigquiz_df %>%
  filter(QuestionId == 17479)

submit12_rdf %>%
  filter(QuestionId == 17479)

bigquiz_df %>%
  arrange(UserId, DateAnswered) %>%
  View()

get_question_responses(4796) %>%
  count(QuizId)

get_user_responses(0) %>%
  filter(QuizId == 2210) %>%
  mutate(DateAnswered = as.Date(DateAnswered)) %>%
  filter(DateAnswered == ymd(20190430)) %>%
  pull(QuestionId) -> x

get_questions_responses(x) %>%
  count(QuizId, QuestionId) %>%
  mutate(contains_question = QuestionId %in% x) %>%
  group_by(QuizId) %>%
  summarise(n = sum(contains_question)) %>%
  ungroup() %>%
  arrange(desc(n))

# group same day item responses of a student

x

get_quiz_responses(584) %>%
  count(QuestionId)
