library(tidyverse)
library(mirt)
library(lubridate)
library(recosystem)

load("../data/quiz_clusters.Rdata")

source("../load_data.R")
source("../helpers.R")

train_rdf
ansmd_rdf
qmd_df
stumd_rdf
subjmd_rdf

submit12_clust_rdf <- submit12_rdf %>%
  inner_join(quiz_clusters)

responses_df <- ansmd_rdf %>%
  inner_join(train_rdf)

quiz_clusters

responses_clust_df <- responses_df %>%
  inner_join(quiz_clusters)

# looping over 3000 cluster quizzes
reduce(1:3000, function(x, y) {
  
  # x <- data.frame()
  # y <- 1
  
  cat(y,"\n")
  
  # if (y %% 100 == 0) cat(y, "\n")
  
  itemresp_df <- responses_clust_df %>%
    filter(ClusterId == y) %>%
    select(UserId, QuestionId, IsCorrect)
  
  itemresp_wide_df <- itemresp_df %>%
    spread(QuestionId, IsCorrect)
  
  itemresp_mat <- itemresp_wide_df %>%
    select(-1) %>%
    as.matrix()
  
  rownames(itemresp_mat) <- itemresp_wide_df$UserId
  
  res <- tryCatch({
    
    item_mod <- mirt(itemresp_mat, 1, "2PL", verbose = FALSE)
    
    # coef(item_mod, IRTpars = T, simplify = T)
    
    thetas <- fscores(item_mod)
    
    sapply(1:ncol(itemresp_mat), function(x) {
      expected.item(extract.item(item_mod, x), thetas)
    }) -> pred_itemresp_mat
    
    colnames(pred_itemresp_mat) <- colnames(itemresp_mat)
    
    pred_responses <- pred_itemresp_mat %>%
      as_tibble() %>%
      mutate(UserId = as.numeric(rownames(itemresp_mat))) %>%
      gather(key = "QuestionId", value = "IsCorrectProb", -UserId) %>%
      mutate(QuestionId = as.numeric(QuestionId))
    
    pred_train_responses <- pred_responses %>%
      inner_join(itemresp_df, by = c("UserId", "QuestionId"))
      
    pred_test_responses <- pred_responses %>%
      semi_join(filter(submit12_clust_rdf, ClusterId == y), by = c("UserId", "QuestionId"))
    
    list(pred_train_responses, pred_test_responses)
    
  }, error = function(e) {
    
    list(data.frame(), data.frame())
    
  })
  
  # first df is train preds
  # second df is test preds
  list(rbind(x[[1]], res[[1]]), rbind(x[[2]], res[[2]]))
  
}, .init = list(data.frame(), data.frame())) -> pred_irt_v1

pred_train_irt_v1 <- pred_irt_v1[[1]] %>%
  select(UserId, QuestionId, IsCorrect, IsCorrectProb) %>%
  rename(pred_irt_v1 = IsCorrectProb) 

pred_test_irt_v1 <- pred_irt_v1[[2]] %>%
  mutate(analysis = "irt_v1")

save(pred_train_irt_v1, pred_test_irt_v1, file = "pred_irt_v1.Rdata")


