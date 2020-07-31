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
