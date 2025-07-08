library(text2vec)
library(car)
library(ggplot2)
library(flextable)
library(corrplot)
library(readxl)
library(jiebaR)
caseset <- read_excel("final rob data.xlsx")
# caseset <- read_excel("caseset.xlsx")
# caseset.xlsx is burglary data. final rob data.xlsx is robbery data

#data process############################################
#####################################################
jieba <- worker(bylines = TRUE)
tokens <- segment(caseset$案情, jieba)
v = create_vocabulary(itoken(tokens))
v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.5)
it = itoken(tokens)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)
tcm = create_tcm(it, vectorizer, skip_grams_window = 3)
glove_model = GloVe$new(rank = 50, x_max = 10)
wv = glove_model$fit_transform(tcm, n_iter = 3)
# get average of main and context vectors as proposed in GloVe paper
wv = wv + t(glove_model$components)
rwmd_model = RelaxedWordMoversDistance$new(dtm, wv)
rwms = rwmd_model$sim2(dtm[1:12243, ]) # robbery = 5842,theft = 12243
numberCase <- rwms[1,]

tokens1 <- segment(caseset$说理, jieba)
v1 = create_vocabulary(itoken(tokens1))
v1 = prune_vocabulary(v1, term_count_min = 5, doc_proportion_max = 0.5)
it1 = itoken(tokens1)
vectorizer1 = vocab_vectorizer(v1)
dtm1 = create_dtm(it1, vectorizer)
tcm1 = create_tcm(it1, vectorizer, skip_grams_window = 3)
glove_model1 = GloVe$new(rank = 50, x_max = 10)
wv1 = glove_model1$fit_transform(tcm, n_iter = 3)
# get average of main and context vectors as proposed in GloVe paper
wv1 = wv1 + t(glove_model1$components)
rwmd_model1 = RelaxedWordMoversDistance$new(dtm1, wv1)
rwms1 = rwmd_model1$sim2(dtm1[1:12243, ])
numberReason <- rwms1[1,]

# get-dimension
vector_dim <- ncol(wv)
cat("Word vector dimension:", vector_dim, "\n")

# norm distribution
norms <- sqrt(rowSums(wv^2))
hist(norms, main = "Distribution of Robbery Word Vector Norms", xlab = "Norm", breaks = 50)

# get-dimension
vector_dim1 <- ncol(wv1)
cat("Word vector dimension:", vector_dim1, "\n")

# norm distribution
norms1 <- sqrt(rowSums(wv1^2))
hist(norms1, main = "Distribution of Robbery Word Vector Norms", xlab = "Norm", breaks = 50)

# documetns lengths
dtm <- as.matrix(dtm)
doc_lengths <- rowSums(dtm)
hist(log(doc_lengths), main = "Distribution of Burglary Document Lengths", xlab = "Number of Words", breaks = 100)

dtm1 <- as.matrix(dtm1)
doc_lengths <- rowSums(dtm1)
hist(log(doc_lengths), main = "Distribution of Burglary Document Lengths", xlab = "Number of Words", breaks = 100)


#facts-reason###########################################
#####################################################
results <- data.frame()

for (i in 1:length(numberCase)) {
  model <- lm(rwms[i,][-i] ~ rwms1[i,][-i])
  
  results <- rbind(results, data.frame(
    iteration = i,
    slope = coef(model)[2],    
    se <- sqrt(diag(vcov(model)))[2]
  ))
  
  if (i %% 100 == 0) {
    cat("已完成", i, "次回归\n")
  }
}

ggplot(results, aes(x = slope)) +
  geom_density(alpha = 0.5,             
               fill = "blue",
               linewidth = 0) +   
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + 
  labs(title = "Facts-Reasoning Coefficients Distribution (Burglary)",  
       x = "Coefficient",                  
       y = "Density") +                
  theme_bw() 

lm1 <- lm(numberReason[-1] ~ numberCase[-1])
slope = coef(lm1)[2]
se <- sqrt(diag(vcov(lm1)))[2]
summary(lm1)
plot(lm1)

numberVerdict <- rep(NA, length(numberReason))
for (i in 1:length(numberReason)){
  numberVerdict[i] <- sqrt((caseset$刑期[i] - caseset$刑期[1])^2)
}
numberVerdict <- log1p(numberVerdict)

lm2 <- lm(numberVerdict[-1] ~ numberReason[-1])
summary(lm2)
plot(lm2)



#reason-sentencing###########################################
#####################################################
results1 <- data.frame()

for (i in 1:length(numberCase)) {
  
  numberVerdict <- rep(NA, length(numberReason))
  for (s in 1:length(numberReason)){
    numberVerdict[s] <- sqrt((caseset$刑期[s] - caseset$刑期[i])^2)
  }
  numberVerdict <- log1p(numberVerdict)
  model <- lm(numberVerdict[-i] ~ rwms[i,][-i])
  
  results1 <- rbind(results1, data.frame(
    iteration = i,
    slope = coef(model)[2],     
    se <- sqrt(diag(vcov(model)))[2]
  ))
  
  if (i %% 100 == 0) {
    cat("已完成", i, "次回归\n")
  }
}

ggplot(results1, aes(x = slope)) +
  geom_density(alpha = 0.5,            
               fill = "blue",
               linewidth = 0) +   
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") + 
  labs(title = "Reasoning-Sentencing Coefficients Distribution (Burglary)",  
       x = "Coefficient",                  
       y = "Density") +                  
  theme_bw() 
