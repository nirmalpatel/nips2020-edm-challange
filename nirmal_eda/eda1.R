library(tidyverse)
library(mirt)

source("../load_data.R")

train_rdf
ansmd_rdf
qmd_df
stumd_rdf
subjmd_rdf

stumd_df <- stumd_rdf %>%
  mutate(DateOfBirth = as.Date(DateOfBirth))
  
stumd_df %>%
  count(DateOfBirth) %>%
  View()

# valid birth year range: 1999 - 2013

stumd_df %>%
  count(PremiumPupil, Gender)

qmd_df %>%
  inner_join(subjmd_rdf) %>%
  group_by(Level, SubjectId, Name) %>%
  summarise(n = n_distinct(QuestionId)) %>%
  View()

qmd_df %>%
  inner_join(subjmd_rdf) %>%
  group_by(Level) %>%
  summarise(n = n_distinct(QuestionId)) %>%
  View()

# ------ important 

qmd_max_level_df <- qmd_df %>%
  inner_join(subjmd_rdf) %>%
  group_by(QuestionId) %>%
  filter(Level == max(Level)) %>%
  ungroup()

qmd_all_level_df <- qmd_df %>%
  inner_join(subjmd_rdf) %>%
  mutate(ParentId = as.numeric(ParentId)) %>%
  filter(Level == 3) %>%
  rename(Subject = Name) %>%
  select(-Level) %>%
  inner_join(subjmd_rdf %>%
               select(SubjectId, Name, ParentId) %>%
               rename(SubjectParent1 = Name,
                      ParentId1 = ParentId), c("ParentId" = "SubjectId")) %>%
  mutate(ParentId1 = as.numeric(ParentId1)) %>%
  inner_join(subjmd_rdf %>%
               select(SubjectId, Name, ParentId) %>%
               rename(SubjectParent2 = Name,
                      ParentId2 = ParentId), c("ParentId1" = "SubjectId")) %>%
  mutate(ParentId2 = as.numeric(ParentId2)) %>%
  inner_join(subjmd_rdf %>%
               select(SubjectId, Name, ParentId) %>%
               rename(SubjectParent3 = Name,
                      ParentId3 = ParentId), c("ParentId2" = "SubjectId")) %>%
  select(QuestionId, SubjectId, Subject, SubjectParent1, SubjectParent2, SubjectParent3)

ansmd_rdf %>%
  group_by(QuizId) %>%
  summarise(min_ts = min(DateAnswered),
            max_ts = max(DateAnswered),
            duration = max_ts - min_ts) %>%
  View()

quiz_stats_df <- ansmd_rdf %>%
  inner_join(select(train_rdf, UserId, QuestionId, AnswerId)) %>%
  group_by(QuizId, SchemeOfWorkId, GroupId) %>%
  summarise(n = n(), nq = n_distinct(QuestionId), nu = n_distinct(UserId))

qid <- 13139
onequiz_df <- ansmd_rdf %>%
  filter(QuizId == qid) %>%
  inner_join(filter(train_rdf, AnswerId %in% filter(ansmd_rdf, QuizId == qid)$AnswerId))

n_distinct(onequiz_df$QuestionId)
n_distinct(onequiz_df$UserId)

onequiz_df %>%
  select(QuestionId, UserId) %>%
  rename(Question1 = QuestionId) %>%
  inner_join(onequiz_df %>%
               select(UserId, QuestionId) %>%
               rename(Question2 = QuestionId)) %>%
  group_by(Question1, Question2) %>%
  summarise(n_common_stu = n_distinct(UserId)) %>%
  spread(Question2, n_common_stu, fill = 0) %>%
  View()
  
onequiz_df %>%
  inner_join(qmd_all_level_df) %>%
  group_by(QuestionId, Subject, SubjectParent1, SubjectParent2, SubjectParent3) %>%
  summarise(maxts = max(DateAnswered),
            mints = min(DateAnswered)) %>%
  arrange(as.Date(mints), SubjectParent3, SubjectParent2, SubjectParent1) %>%
  View()

onequiz_df %>%
  select(UserId, QuestionId, IsCorrect) %>%
  spread(QuestionId, IsCorrect) %>%
  select(-1) %>%
  as.matrix() -> itemresp_mat

onequiz_df %>%
  filter(QuestionId == 10983) %>% View()
  count(IsCorrect)

item_mod <- mirt(itemresp_mat, 1, "2PL")

coef(item_mod, IRTpars = T, simplify = T)

onequiz_df %>%
  summarise(n = n_distinct(Quie))

onequiz_df %>%
  inner_join(qmd_all_level_df) %>%
  group_by(dateid = as.Date(DateAnswered), SubjectParent2, SubjectParent3) %>%
  summarise(n = n(),
            n_stu = n_distinct(UserId)) %>%
  ungroup() %>%
  ggplot(aes(dateid, n_stu)) +
  geom_col() +
  facet_wrap(~ SubjectParent2 + SubjectParent3)

n_distinct(onequiz_df$GroupId)

unique(onequiz_df$QuestionId) %in% submit12_rdf$QuestionId

onequiz_df %>%
  select(UserId, QuestionId, IsCorrect) %>%
  spread(QuestionId, IsCorrect) %>%
  gather(key = "QuestionId", value = "IsCorrect", -UserId) %>%
  mutate(QuestionId = as.numeric(QuestionId)) %>%
  filter(is.na(IsCorrect)) %>%
  inner_join(submit12_rdf)

# for big quizzes
# find out items that have common students
# set some threshold, and make new quiz Ids

n_distinct(ansmd_rdf$QuizId)

