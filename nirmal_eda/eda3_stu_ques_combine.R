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

itr_rdf <- train_rdf %>%
  inner_join(ansmd_rdf)

question_stats_df <- itr_rdf %>%
  count(QuizId, QuestionId, IsCorrect)

response_count_df <- question_stats_df %>%
  group_by(QuizId, QuestionId) %>%
  summarise(ndistresp = n(),
            resp = sum(n)) %>%
  ungroup()

response_count_df %>%
  filter(QuestionId == 6167) %>%
  arrange(desc(resp))

ques_quiz_need_reasmt <- response_count_df %>%
  group_by(QuizId) %>%
  mutate(n_questions = n()) %>%
  ungroup() %>%
  mutate(need_reasmt = if_else(ndistresp == 2 & QuizId != 2210 & n_questions >= 4, FALSE, TRUE)) %>%
  select(QuizId, QuestionId, need_reasmt)

quiz_question_count_rdf <- itr_rdf %>%
  count(QuestionId, QuizId)

qid_reassign_df <- quiz_question_count_rdf %>%
  arrange(QuestionId, desc(n)) %>%
  group_by(QuestionId) %>%
  slice(1) %>%
  ungroup() %>%
  select(-n)

qid_reassign_map <- setNames(qid_reassign_df$QuizId, qid_reassign_df$QuestionId)

itr_fix_rdf <- itr_rdf %>%
  inner_join(ques_quiz_need_reasmt) %>%
  mutate(QuizId = case_when(
    need_reasmt == TRUE ~ qid_reassign_map[as.character(QuestionId)],
    need_reasmt == FALSE ~ QuizId
  ))

quiz_stats_df <- itr_fix_rdf %>%
  group_by(QuizId) %>%
  summarise(n_questions = n_distinct(QuestionId),
            n_responses = n(),
            n_students = n_distinct(UserId)) %>%
  ungroup() %>%
  mutate(possible_responses = n_students * n_questions,
         available_responses = n_responses / possible_responses * 100)

quiz_ok_df <- quiz_stats_df %>%
  filter(n_questions >= 5,
         n_students >= 50,
         n_students > n_questions,
         between(available_responses, 60, 80)) %>%
  arrange(available_responses)

# quiz_stats_rdf %>%
#   ggplot(aes(available_responses)) +
#   geom_histogram() +
#   labs(x = "Available % of responses in a quiz",
#        y = "# of quizzes",
#        title = "How much data do we have available at the quiz level?")







cat("Iter", iter, "\n")
cat("Ok quizzes", nrow(quiz_ok_df), "\n")
cat("Total quizzes", nrow(quiz_stats_df), "\n")







reassign_quiz_questions(itr_nextit_df, quiz_ok_df, iter - 1)



# get_mirt_mod(12852)
  
quiz_stats_rdf %>%
  anti_join(quiz_ok_iter1)


quiz_question_map_iter1 <- quiz_question_count_rdf %>%
  semi_join(quiz_stats_rdf %>%
              anti_join(quiz_ok_iter1)) %>%
  arrange(QuestionId, desc(n)) %>%
  group_by(QuestionId) %>%
  slice(1) %>%
  ungroup() %>%
  select(-n)
  

Sys.time()

QuestionId QuizId     n
<dbl>  <dbl> <int>
1       6167      0    20
2      11245      0    20-----------
3       6083      0    18
4       4825      0    16-----------
5      24419      0    16

quiz_question_count_rdf %>%
  filter(QuestionId == 24419) %>%
  arrange(desc(n))

get_question_responses(24419) 

get_quiz_responses(8348) %>%
  count(QuestionId)

group_by(QuizId) %>%
  summarise(n_questions = n(),
            n_responses = sum(n)) %>%
  View()

# find out quizzes where the questions have most responses
quizid_reassign_rdf <- itr_rdf %>%
  count(QuestionId, QuizId) %>%
  arrange(QuestionId, desc(n)) %>%
  group_by(QuestionId) %>%
  top_n(1, n) %>%
  ungroup()

quizid_reassign_rdf %>%
  filter(QuestionId == 6876)

quizid_reassign_rdf %>%
  count(QuizId)

quizid_reassign_rdf %>%
  count(QuizId) %>%
  arrange(desc(n)) %>%
  filter(n == 1) %>%
  count(n) %>%
  View()
  
get_quiz_responses(11)

get_question_responses(6876) %>%
  count(QuizId)


submit12_rdf %>%
  filter(QuestionId == 6876)
  
qid_quizid_map <- quizid_reassign_rdf %>%
  select(QuestionId, QuizId)

qid_quizid_map %>%
  count(QuizId) %>%
  count(n) %>%
  View()


q2210 <- get_quiz_responses(2210)

q2210

q2210 %>%
  filter(UserId == 53831) %>%
  arrange(DateAnswered)

get_question_responses(1999) %>%
  count(QuizId) %>%
  filter(QuizId != 2210) %>%
  pull(QuizId) -> quizzes

qids <- c(13734, 9717, 18065, 1999)

get_questions_responses(qids) %>%
  count(QuizId, QuestionId) %>%
  group_by(QuizId) %>%
  summarise(n_questions = n(), avg_responses = mean(n)) %>%
  arrange(desc(n_questions), desc(avg_responses))



lapply(quizzes, get_quiz_responses) %>%
  bind_rows() %>%
  count(QuizId, QuestionId) %>%
  View()

get_quiz_responses(12655)
