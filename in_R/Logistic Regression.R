library(tidyverse)
library(data.table)

df <- fread("df_1000.csv")

names(df) <- names(df) %>% 
  str_replace_all(" ","_") %>% 
  str_replace_all("-","_") %>% 
  str_replace_all("/","_")

df$loan_status %>% 
  as.factor()-> df$loan_status

target <- 'loan_status'
features <- df %>% 
  select(-loan_status) %>% 
  names()

f <- as.formula(paste
                (target, 
                  paste(features, 
                        collapse = " + "), 
                  sep = " ~ "))
lg <- glm(f, 
           family = binomial(link="logit"),
           data = df)

features %>% length() #134

coef_na <- attributes(alias(lg)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]
features %>% length()

f <- as.formula(paste(target, 
                      paste(features, 
                            collapse = " + "), 
                      sep = " ~ "))
lg <- glm(f, 
           family=binomial(link = "logit"),
           data = df)
#copy <- df
df <- copy

while(lg %>% 
      faraway::vif() %>% 
      sort(decreasing = T) %>% 
      .[1] >= 10){
  afterVIF <- lg %>% 
    faraway::vif() %>% 
    sort(decreasing = T) %>% 
    .[-1] %>% 
    names()
  f <- as.formula(paste(target, 
                        paste(afterVIF, 
                              collapse = " + "), 
                        sep = " ~ "))
  lg <- glm(f, 
            family=binomial(link = "logit"),
            data = df)
}

lg %>% 
  faraway::vif() %>% 
  sort(decreasing = T) %>% 
  names() -> features

features %>% length()

df <- df %>% 
  select(loan_status,features)

df %>% names()

# p_values ----

while(summary(lg)$coefficients %>% 
      as.data.frame() %>%  
      select(`Pr(>|z|)`) %>% 
      rename(p_value=`Pr(>|z|)`) %>%
      mutate(Feature=row.names(.)) %>% 
      mutate(p_value=round(p_value,3)) %>% 
      .[-1,] %>% 
      arrange(p_value %>% desc()) %>% 
      .[1,1]>0.05){
  
  summary(lg)$coefficients %>% 
    as.data.frame() %>%  
    select(`Pr(>|z|)`) %>% 
    rename(p_value=`Pr(>|z|)`) %>%
    mutate(Feature=row.names(.)) %>% 
    mutate(p_value=round(p_value,3)) %>% 
    .[-1,] %>% 
    arrange(p_value %>% desc()) %>% 
    .[1,2] ->v
  
  features <- features[features!=v]
  
  f <- as.formula(paste(target, 
                        paste(features, 
                              collapse = " + "), 
                        sep = " ~ "))
  lg <- glm(f, 
            family=binomial(link = "logit"),
            data = df)
  
}

summary(lg)$coefficients %>% 
  as.data.frame() %>%  
  select(`Pr(>|z|)`) %>% 
  rename(p_value=`Pr(>|z|)`) %>%
  mutate(Feature=row.names(.)) %>% 
  mutate(p_value=round(p_value,3)) %>% 
  .[-1,] %>% 
  arrange(p_value %>% desc())

features %>% length() #34

library(caTools)
set.seed(42)
sample <- sample.split(
  df$loan_status,
  SplitRatio = 0.8
)
train <- subset(df, sample==T)
test <- subset(df, sample==F)

lg <- glm(f,
          family=binomial(link = 'logit'),
          data=train)

fitted_probailities <- 
  predict(lg,
          test,
          type = 'response')

fitted_results <- ifelse(fitted_probailities>0.5,
                         1,
                         0)

mean(fitted_results==test$loan_status) #Accuracy 70%

table(fitted_results,
      test$loan_status) %>% 
  confusionMatrix()

#   0   1
# 0 743 342
# 1 257 658

df %>% 
  select(loan_status,
         features) %>% 
  write.csv("df_vif_p_1000.csv")

library(cvAUC)
AUC(fitted_results,
    test$loan_status) #0.7005

library(pROC)
plot(roc(test$loan_status,
         fitted_probailities))

link_score <- 
  predict(lg, test, type = "link")

logistic <- data.frame(link_score)
logistic$response <- fitted_probailities
logistic$loan_status <- test$loan_status

# link type Plot -----
logistic %>%
  sample_n(500) %>% 
  ggplot(aes(link_score, response, 
             color=loan_status,
             size=5
             ))+
  scale_color_manual(values=c('red', 'black'))+
  geom_point(size=2)+
  theme_bw()






