library(tidyverse)

load("qpair_common_stu.Rdata")

# qpair_common_stu_sorted <- qpair_common_stu %>%
#   arrange(desc(n_common_stu))

# qpair_common_stu_sorted %>%
#   slice(1:10000) %>%
#   View()

quiz_simmat_df <- qpair_common_stu %>%
  spread(Question2, n_common_stu, fill = 0)

quiz_simmat <- quiz_simmat_df %>%
  select(-1) %>%
  as.matrix()

rownames(quiz_simmat) <- colnames(quiz_simmat)

quiz_dsimmat <- max(quiz_simmat) - quiz_simmat

hclust_mod <- hclust(as.dist(quiz_dsimmat), method = "ward.D")

gc()

plot(hclust_mod, labels = FALSE)

clust_assigments <- cutree(hclust_mod, k = 3000)

quiz_clusters <- tibble(
  QuestionId = as.numeric(names(clust_assigments)),
  ClusterId = as.numeric(clust_assigments)
) %>%
  arrange(ClusterId)

quiz_clusters %>%
  count(ClusterId) %>%
  View()

save(quiz_clusters, file = "quiz_clusters.Rdata")
