library(tidyverse)
library(recosystem)
library(recommenderlab)
library(mltools)
library(data.table)
library(parallel)

train_rdf <- read_csv("../data/train_data/train_task_1_2.csv")
ansmd_rdf <- read_csv("../data/metadata/answer_metadata_task_1_2.csv")
qmd_rdf <- read_csv("../data/metadata/question_metadata_task_1_2.csv")
stumd_rdf <- read_csv("../data/metadata/student_metadata_task_1_2.csv")
subjmd_rdf <- read_csv("../data/metadata/subject_metadata.csv")

submit_rdf <- read_csv("../data/starter_kit/submission_templates/submission_task_1_2.csv") %>%
  select(-1)

t1_submit_rdf <- read_csv("../task1/submission_task_1.csv")

qmd_df <- map2(qmd_rdf$QuestionId, qmd_rdf$SubjectId, function(x, y) {
  
  y_clean <- y %>%
    str_remove("\\[") %>%
    str_remove("\\]")
  
  subjects <- as.numeric(strsplit(y_clean, ", ")[[1]])
  
  data.frame(QuestionId = rep(x, length(subjects)),
             SubjectId = subjects)
  
}) %>%
  bind_rows() %>%
  as_tibble()

quiz_question_mapping_rdf <- train_rdf %>%
  select(QuestionId, AnswerId) %>%
  inner_join(select(ansmd_rdf, AnswerId, QuizId)) %>%
  count(QuizId, QuestionId)

quiz_question_popular_mapping_rdf <- quiz_question_mapping_rdf %>%
  group_by(QuestionId) %>%
  top_n(1, n) %>%
  ungroup() %>%
  select(-n)

quiz_n_questions_df <- quiz_question_mapping_rdf %>%
  group_by(QuizId) %>%
  summarise(n_questions = n()) %>%
  ungroup()

prediction_blacklist <- submit_rdf %>%
  inner_join(quiz_question_popular_mapping_rdf) %>%
  # arrange(UserId, QuizId) %>%
  group_by(UserId, QuizId) %>%
  summarise(n_user_responses = n()) %>%
  ungroup() %>%
  inner_join(quiz_n_questions_df) %>%
  filter(n_questions == n_user_responses)

# correct answers for quizzes

correct_answers_rdf <- train_rdf %>%
  select(QuestionId, AnswerId, IsCorrect, CorrectAnswer) %>%
  inner_join(select(ansmd_rdf, AnswerId, QuizId)) %>%
  filter(IsCorrect == 1) %>%
  count(QuizId, QuestionId, CorrectAnswer)

correct_answers_df <- correct_answers_rdf %>%
  arrange(QuestionId, n) %>%
  group_by(QuestionId) %>%
  slice(1) %>%
  ungroup()

# popular incorrect answers for the quizzes

popular_incorrect_answers_rdf <- train_rdf %>%
  filter(IsCorrect == 0) %>%
  select(QuestionId, AnswerId, AnswerValue) %>%
  inner_join(select(ansmd_rdf, AnswerId, QuizId)) %>%
  count(QuizId, QuestionId, AnswerValue) %>%
  group_by(QuizId, QuestionId) %>%
  top_n(1, n) %>%
  ungroup()

popular_incorrect_answers_df <- popular_incorrect_answers_rdf %>%
  arrange(QuestionId, desc(n)) %>%
  group_by(QuestionId) %>%
  slice(1) %>%
  ungroup() %>%
  rename(PopularIncorrectAnswer = AnswerValue) %>%
  select(-n, -QuizId)

# find out missing values for the people who answered at least
# one question in the quiz associated with their question

# people who got it correct according to our task 1 submission get the correct answer

t2_pred_t1_correct_rdf <- t1_submit_rdf %>%
  filter(IsCorrect == 1) %>%
  inner_join(select(correct_answers_df, QuestionId, CorrectAnswer)) %>%
  select(-IsCorrect) %>%
  rename(AnswerValue = CorrectAnswer)

# people who got it incorrect according to task 1 get their imputed response as the predicted response

# t1_submit_rdf %>%
#   filter(IsCorrect == 0) %>%
#   count(QuestionId) %>%
#   pull(QuestionId) -> 

user_quiz_responses_rdf <- train_rdf %>%
  select(UserId, QuestionId, AnswerId, AnswerValue) %>%
  inner_join(select(ansmd_rdf, AnswerId, QuizId))

# trying to group together students who took the quiz at the same
q2210_data <- train_rdf %>%
  inner_join(filter(ansmd_rdf, QuizId == 2210))

q2210_data %>%
  arrange(QuestionId) %>%
  filter(UserId == 87630)

q2210_data %>%
  filter(QuestionId %in% c(19707, 12024, 19106, 10370, 25370))

user_quiz_responses_rdf %>%
  filter(QuizId == 2210) %>%
  inner_join(inner_join(qmd_df, subjmd_rdf)) %>%
  filter(Level > 1) %>%
  mutate(QuizId = paste0(QuizId, "_", SubjectId)) %>%
  group_by(QuizId) %>%
  summarise(n1 = n_distinct(UserId),
            n2 = n_distinct(QuestionId),
            n3 = n()) %>%
  arrange(desc(n2))

# added skill into the quiz Id to make smaller quizzes
user_quiz_2210_responses_df <- user_quiz_responses_rdf %>%
  filter(QuizId == 2210) %>%
  inner_join(inner_join(qmd_df, subjmd_rdf)) %>%
  filter(Level > 1) %>%
  mutate(QuizId = paste0(QuizId, "_", SubjectId)) %>%
  select(all_of(colnames(user_quiz_responses_rdf)))

user_quiz_responses_df <- user_quiz_responses_rdf %>%
  filter(QuizId != 2210) %>%
  mutate(QuizId = as.character(QuizId)) # %>%
  # bind_rows(user_quiz_2210_responses_df)

user_quiz_responses_list <- split(user_quiz_responses_df, user_quiz_responses_df$QuizId)

# user_quiz_responses_list[["2210_144"]] %>%
#   summarise(nu = n_distinct(UserId),
#             nq = n_distinct(QuestionId))

# user_quiz_responses_list[["2210_144"]] %>%
#   count(QuestionId) %>%
#   View()

# quizd 9974
# quizid 1076
# quizid 1

# quizid_sample <- c("2210_144")
# quizids_sample <- sample(unique(quiz_n_questions_df$QuizId), 50)
quizids_all <- names(user_quiz_responses_list)

# cl <- makeCluster(7)
# 
# clusterEvalQ(cl, library(tidyverse))
# clusterEvalQ(cl, library(mltools))
# clusterEvalQ(cl, library(data.table))
# clusterEvalQ(cl, library(recommenderlab))

# save(user_quiz_responses_list, file = "task2_quiz_responses.Rdata")

proc_start_time <- Sys.time()
lapply(user_quiz_responses_list[as.character(quizids_all)], function(x_rdf) {
  
  cat(unique(x_rdf$QuizId))
  cat("\n")
  
  # x_rdf <- user_quiz_responses_list[["2210_144"]]
  
  all_questions <- unique(x_rdf$QuestionId)
  
  answer_factor_levels_df <- tibble(
    QuestionId = rep(all_questions, each = 4),
    AnswerValue = rep(c(1, 2, 3, 4), length(all_questions))
  ) %>%
    mutate(AnswerFactor = paste0(QuestionId, "_", AnswerValue)) %>%
    select(-AnswerValue)
  
  x_tall_df <- x_rdf %>%
    select(UserId, QuestionId, AnswerValue) %>%
    inner_join(answer_factor_levels_df) %>%
    mutate(UserResponse = paste0(QuestionId, "_", AnswerValue),
           Selected = as.numeric(AnswerFactor == UserResponse)) %>%
    select(UserId, AnswerFactor, Selected)
  
  x_wide_rdf <- x_tall_df %>%
    spread(AnswerFactor, Selected)
  
  # x_recosys_tall_rdf <- x_wide_rdf %>%
  #   gather(key = "QuestionOption", value = "Response", -UserId)
  # 
  # x_recosys_train_df <- x_recosys_tall_rdf %>%
  #   filter(!is.na(Response))
  # 
  # x_recosys_test_df <- x_recosys_tall_rdf %>%
  #   filter(is.na(Response))
  
  mat_incomplete <- x_wide_rdf %>%
    select(-1) %>%
    as.matrix()
  
  quiz_preds_df <- tryCatch({
    
    # reco <- Reco()
    # 
    # reco$train(data_memory(x_recosys_train_df$UserId, x_recosys_train_df$QuestionOption, x_recosys_train_df$Response),
    #            opts = list(dim = 20, costp_l2 = 0.01, costq_l2 = 0.01, nthread = 7)
    # )
    # 
    # pred <- reco$predict(data_memory(submit_df$UserId, subj_ords[submit_df$SubjectId]), out_memory())
    # 
    # submit_df_upload <- submit_df %>%
    #   mutate(predval = pred) %>%
    #   group_by(UserId, QuestionId) %>%
    #   summarise(IsCorrect = if_else(mean(predval, na.rm = TRUE) >= .50, 1, 0)) %>%
    #   ungroup()
    
    mat_incomplete_modinp <- as(mat_incomplete, "realRatingMatrix")
    mat_incomplete_mod <- Recommender(mat_incomplete_modinp, method = "UBCF")
    mat_pred_raw <- predict(mat_incomplete_mod, mat_incomplete_modinp, type = "ratings")

    mat_pred <- as(mat_pred_raw, "matrix")

    mat_pred[mat_pred > 1] <- 1
    mat_pred[mat_pred < 0] <- 0

    # setting NAs to 0 for addition
    mat_pred[is.na(mat_pred)] <- 0
    mat_incomplete[is.na(mat_incomplete)] <- 0

    # combining both matrices
    mat_complete <- mat_incomplete + mat_pred

    mat_complete %>%
      as.data.frame() %>%
      mutate(UserId = x_wide_rdf$UserId) %>%
      gather(key = "QuestionId_AnswerValue", value = "SelectProb", -UserId) %>%
      separate("QuestionId_AnswerValue", c("QuestionId", "AnswerValue")) %>%
      mutate(QuestionId = as.numeric(QuestionId),
             AnswerValue = as.numeric(AnswerValue)) %>%
      filter(SelectProb > 0.0) %>%
      group_by(UserId, QuestionId) %>%
      top_n(1, SelectProb) %>%
      ungroup()
    
  }, error = function(e) {
    data.frame()
  })
  
  quiz_preds_df
  
}) -> incorrect_impute_res
proc_end_time <- Sys.time()

proc_end_time - proc_start_time

# stopCluster(cl)

incorrect_impute_res_df <- incorrect_impute_res %>%
  bind_rows() %>%
  select(UserId, QuestionId, AnswerValue) %>%
  group_by(UserId, QuestionId) %>%
  summarise(AnswerValue = min(AnswerValue))

t2_pred_t1_incorrect_rdf <- submit_rdf %>%
  anti_join(t2_pred_t1_correct_rdf) %>%
  left_join(incorrect_impute_res_df)

sum(!is.na(t2_pred_t1_incorrect_rdf$AnswerValue))

# find out missing values for people who did not answer any question on their quiz
# fill those values with the most popular responses

t2_pred_t1_incorrect_filled_rdf <- t2_pred_t1_incorrect_rdf %>%
  left_join(popular_incorrect_answers_df) %>%
  mutate(AnswerValue = if_else(is.na(AnswerValue), PopularIncorrectAnswer, AnswerValue)) %>%
  select(-PopularIncorrectAnswer)

t2_pred_final <- bind_rows(
  t2_pred_t1_correct_rdf,
  t2_pred_t1_incorrect_filled_rdf
)

submit_df <- submit_rdf %>%
  left_join(t2_pred_final) %>%
  mutate(AnswerValue = if_else(is.na(AnswerValue), 2, AnswerValue))

which(is.na(submit_df))

submit_df %>%
  write_csv("submission_task_2.csv")
