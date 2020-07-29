library(tidyverse)

ansmd12_rdf <- read_csv("data/metadata/answer_metadata_task_1_2.csv")
qmd12_rdf <- read_csv("data/metadata/question_metadata_task_1_2.csv")
stumd12_rdf <- read_csv("data/metadata/student_metadata_task_1_2.csv")
subjmd_rdf <- read_csv("data/metadata/subject_metadata.csv")

train12_rdf <- read_csv("data/train_data/train_task_1_2.csv")

task1_submit_rdf <- read_csv("data/starter_kit/submission_templates/submission_task_1_2.csv")

ansmd12_rdf

qmd12_df <- map2(qmd12_rdf$QuestionId, qmd12_rdf$SubjectId, function(x, y) {
  
  y_clean <- y %>%
    str_remove("\\[") %>%
    str_remove("\\]")
  
  subjects <- as.numeric(strsplit(y_clean, ", ")[[1]])
  
  data.frame(QuestionId = rep(x, length(subjects)),
             SubjectId = subjects)
  
}) %>%
  bind_rows() %>%
  as_tibble()

stu_nresp_df <- train12_df %>%
  count(UserId, sort = TRUE)
  
stu_nresp_df %>%
  summary()

stu_nresp_df %>%
  filter(n < 200)

stuids <- c(4340)

train12_stu_df <- train12_df %>%
  filter(UserId %in% stuids)
  
ansmd12_rdf %>%
  filter(AnswerId %in% train12_stu_df$AnswerId) %>%
  inner_join(train12_stu_df) %>%
  View()

ansmd12_rdf %>%
  mutate(DateAnswered = as.Date(DateAnswered)) %>%
  count(DateAnswered) %>%
  ggplot(aes(DateAnswered, n)) +
  geom_col()

mean(is.na(ansmd12_rdf$Confidence))

n_distinct(ansmd12_rdf$GroupId)
n_distinct(ansmd12_rdf$QuizId)

ansmd12_rdf %>%
  count(QuizId, sort = TRUE)
  summary()
  
ansmd12_rdf %>%
  filter(QuizId == 2210) %>%
  count(GroupId)

sum(is.na(ansmd12_rdf$QuizId))

train12_df <- train12_rdf %>%
  inner_join(select(ansmd12_rdf, AnswerId, GroupId))
  
train12_df %>%
  group_by(GroupId) %>%
  summarise(avg_correct = mean(IsCorrect)) %>%
  ungroup() %>%
  ggplot(aes(avg_correct)) +
  geom_histogram()

train12_df %>%
  group_by(QuestionId) %>%
  summarise(avg_correct = mean(IsCorrect)) %>%
  ungroup() %>%
  ggplot(aes(avg_correct)) +
  geom_histogram()

train12_df %>%
  count(QuestionId) %>%
  ggplot(aes(n)) +
  geom_histogram()

user_subj_rdf <- train12_df %>%
  select(UserId, QuestionId) %>%
  inner_join(qmd12_df)

uid_subj_count_rdf <- user_subj_rdf %>%
  count(UserId, SubjectId)

all_users <- unique(uid_subj_count_rdf$UserId)

task1_submit_rdf %>%
  mutate(in_train = UserId %in% all_users) %>%
  count(in_train)

submit_uid_subj_df <- task1_submit_rdf %>%
  select(-1) %>%
  inner_join(qmd12_df) %>%
  select(-QuestionId)

submit_uid_subj_available <- submit_uid_subj_df %>%
  left_join(uid_subj_count_rdf)

mean(is.na(submit_uid_subj_available$n))
