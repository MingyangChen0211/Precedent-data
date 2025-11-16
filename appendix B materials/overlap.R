library(readxl)
library(jiebaR)
library(dplyr)
caseset <- read_excel("theft_temp_with_articles.xlsx")
# caseset <- read_excel("rob_temp_with_articles.xlsx")
jieba <- worker(bylines = TRUE)

#############################
#############################
#############################
###section 1: token
#############################
#############################
#############################

library(tm)
library(chinese.misc)
corpus <- as.character(caseset$对应法条全文) # use $说理 to get reasoning
reasons <- corp_or_dtm(
  corpus, 
  from = "v",
  type = "corpus",
  enc = "auto",
  mycutter =  worker(),
  stop_word = "jiebaR",
  stop_pattern = NULL,
  control = "auto",
  myfun1 = NULL,
  myfun2 = NULL,
  special = "",
  use_stri_replace_all = FALSE
)
dtm <- DocumentTermMatrix(reasons)
dtm.tfidf.mat <- as.matrix(dtm)
s <- dtm.tfidf.mat # s for reasoning
s2 <- dtm.tfidf.mat # s2 for legal provisions

# define jaccard similarity, 
# but change union into the length of reasoning
jaccard_similarity <- function(A, B) {
  intersection = length(intersect(A, B))
  union = length(A)
  return (intersection/union)
}

# calculate
simi <- data.frame(similarity = numeric(0))
for (i in c(1:12243)){ # 12243 for theft. 5839 for robbery
  sim <- jaccard_similarity(s[i,],s2[i,])
  simi <- rbind(simi, data.frame(similarity = sim))
  
}
options(scipen = 999)
summary(simi)

#############################
#############################
#############################
###section 2: char
#############################
#############################
#############################

# define function: how many char from legal provisions?
calculate_char_overlap <- function(reasoning, law) {
  reasoning_chars <- unlist(strsplit(reasoning, ""))
  law_chars <- unlist(strsplit(law, ""))
  overlap_chars <- length(intersect(reasoning_chars, law_chars))
  total_reasoning_chars <- length(reasoning_chars)
  if (total_reasoning_chars > 0) {
    return(overlap_chars / total_reasoning_chars)
  } else {
    return(0)
  }
}

# calculate
simi <- data.frame(similarity = numeric(0))
for (i in c(1:5839)){ # 12243 for theft. 5839 for robbery
  sim <- calculate_char_overlap(caseset$说理[i],caseset$对应法条全文[i])
  simi <- rbind(simi, data.frame(similarity = sim))
}
summary(simi)






