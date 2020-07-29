library(tidyverse)

train_rdf <- read_csv("../data/train_data/train_task_1_2.csv")
ansmd_rdf <- read_csv("../data/metadata/answer_metadata_task_1_2.csv")
qmd_rdf <- read_csv("../data/metadata/question_metadata_task_1_2.csv")
stumd_rdf <- read_csv("../data/metadata/student_metadata_task_1_2.csv")
subjmd_rdf <- read_csv("../data/metadata/subject_metadata.csv")

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

submit12_rdf <- read_csv("../data/starter_kit/submission_templates/submission_task_1_2.csv") %>%
  select(-1)
