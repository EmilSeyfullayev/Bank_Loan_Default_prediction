#We have to perform Logistic Regression

df <- final_df

library(caTools)
set.seed(42)
sample <- sample.split(
  df[,1],
  SplitRatio = 0.8
)

train <- subset(df, sample==T)
test <- subset(df, sample==F)



model <- glm(
  loan_status_1~.,
  family = binomial(link = "logit"),
  data = df
)

fitted_probabilities <- 
  predict(model,
          test,
          type='response')

fitted_results <- ifelse(
  fitted_probabilities>0.5,
  1,
  0
)

table(test$loan_status_1,
      fitted_probabilities>0.5)
# 
# FALSE  TRUE
# 0  8021  2731
# 1   365 82050

(8021+82050)/(8021+2731+365+82050)
#97% Accuracy score on test set


fitted_probabilities_train <- 
  predict(model,
          train,
          type='response')

fitted_results_train <- ifelse(
  fitted_probabilities_train>0.5,
  1,
  0
)

table(train$loan_status_1,
      fitted_probabilities_train>0.5)
# 
# FALSE   TRUE
# 0  31836  11174
# 1   1671 327991

(31836+327991)/(31836+327991+1671+11174)
# 96.55% Accuracy score on Train set 
#Accuracy scores are almost the same
#There is no overfitting or
#underfitting

model %>% summary()

#generally, process was went very slowly
#maybe it was because of large dataset
#let's check without dummy variables

library(data.table)
df <- fread("df.csv")
df %>% 
  select(-c(ID, V1)) -> df

df %>% glimpse() 
#we have to now correct datatypes

df %>% names() -> names
l <- c()
for (i in names){
  l <- 
    append(l,
           df[[i]] %>% 
             unique() %>% 
             length())
}

cbind(names, l) %>% 
  as.data.frame() %>% 
  arrange(l %>% as.numeric()) -> value_count

value_count %>% View()
#first 17 variables shlould converted into faactor

for (i in names[1:17]){
  df[[i]] <- 
    df[[i]] %>% as.factor()
}

df %>% skimr::skim()
#We have 1 char var, let's see it

df %>% 
  select_if(is.character)
#it is initial_list_status
#it had to be converted but didnot
df$initial_list_status %>% 
  as.factor() -> df$initial_list_status

df %>% select_if(is.character) #0, Great

# Modeling 2 ----
set.seed(42)
sample <- sample.split(
  df[,1],
  SplitRatio = 0.8
)

train <- subset(df, sample==T)
test <- subset(df, sample==F)



model <- glm(
  loan_status~.,
  family = binomial(link = "logit"),
  data = df
)

#Error: cannot allocate vector of size 21.5 Gb
#If factor, it has same difficulties
#You will be able find modeling in 
#Python Folder, which will be performed in
#Google Colab

#Even though We achieved result, pretty good one
#We were not able to drop variables with NA coef
#Also variables which not significant and have
#Multicolliniarety problems

#Let's save our model
model1 <- model

saveRDS(model, file = "log_model.rds")
#to load back

readRDS("log_model.rda")


