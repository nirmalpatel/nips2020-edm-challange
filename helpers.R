get_question_responses <- function(qid) {
  
  train_rdf %>%
    filter(QuestionId == qid) %>%
    inner_join(filter(ansmd_rdf, AnswerId %in% filter(train_rdf, QuestionId == qid)$AnswerId))
  
}

get_questions_responses <- function(qids) {
  
  train_rdf %>%
    filter(QuestionId %in% qids) %>%
    inner_join(filter(ansmd_rdf, AnswerId %in% filter(train_rdf, QuestionId %in% qids)$AnswerId))
  
}

get_user_responses <- function(uid) {
  
  train_rdf %>%
    filter(UserId == uid) %>%
    inner_join(filter(ansmd_rdf, AnswerId %in% filter(train_rdf, UserId == uid)$AnswerId))
  
}


get_quiz_responses <- function(quizid) {
  
  ansmd_rdf %>%
    filter(QuizId == quizid) %>%
    inner_join(filter(train_rdf, AnswerId %in% filter(ansmd_rdf, QuizId == quizid)$AnswerId))
  
}

get_mirt_mod <- function(quizid) {
  
  get_quiz_responses(quizid) %>%
    select(UserId, QuestionId, IsCorrect) %>%
    spread(QuestionId, IsCorrect) %>%
    select(-1) %>%
    as.matrix() -> itemresp_mat
  
  item_mod <- mirt(itemresp_mat, 1, "2PL")
  
  coef(item_mod, IRTpars = T, simplify = T)
  
}

get_group_responses <- function(gid) {
  
  ansmd_rdf %>%
    filter(GroupId == gid) %>%
    inner_join(filter(train_rdf, AnswerId %in% filter(ansmd_rdf, GroupId == gid)$AnswerId))
  
}
