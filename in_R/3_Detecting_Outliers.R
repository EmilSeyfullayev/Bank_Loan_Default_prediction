library(tidyverse)
library(inspectdf)
library(graphics)

# Numeric >> Factor ----
#let's see whether we need convert numerics 
#to factors if there are few uniques

df %>% 
  select_if(is.numeric) %>% 
  names() -> num_names

uq <- c()

for (i in num_names){
  uq <- uq %>% append(
    df[[i]] %>% unique() %>% length()
  )
}

uq_length <- cbind(num_names, uq)

uq_length <- uq_length %>% 
  as.data.frame() %>% 
  arrange(uq %>% as.numeric())

uq_length
# 
# num_names     uq
# 1                 policy_code      1
# 2                 loan_status      2
# 3                tot_coll_amt      2
# 4              acc_now_delinq      6
# 5  collections_12_mths_ex_med      9
# 6                  emp_length     11
# 7                 delinq_2yrs     24
# 8                     pub_rec     26
# 9              inq_last_6mths     28
# 10                   open_acc     62

#We should drop policy code at all
#loan status and tot_coll_amt covert to factor

#acc_now_delinq represents number of delinquent accounts
#although it has few uniquse, it is continous

#Rest variables are also continous

df <- 
  df %>% 
  select(-policy_code) #drop it
df$loan_status <- 
  df$loan_status %>% as.factor() #to factor
df$tot_coll_amt <- 
  df$tot_coll_amt %>% as.factor() #to factor


num_names <- 
  df %>% 
  select_if(is.numeric) %>% 
  names() #new list of numeric values

# Outliers ----
# Let's create a list of outliers for each variable
outliers <- list()

for (i in num_names){
  outliers[[i]] <-  boxplot(df[[i]])$out
}

#Now we have reassign outliers to upper fence
#We do not have negative values,
#So we will deal only with upper fence

chpoint_11 <- df
df <- chpoint_11

df[df[,num_names[1]], num_names[1]]
df[df[[num_names[1]]], num_names[1]]
#these codes returns us same result as
df[df$loan_amnt, "loan_amnt"]


for (i in num_names){
  df[
    df[,i] %in% outliers[[i]], 
    i
  ] <-
    quantile(df[[i]], 0.75)+
    IQR(df[[i]])*1.5
}

#to check we have to see that all are null
outliers <- list()

for (i in num_names){
  outliers[[i]] <-  boxplot(df[[i]])$out
} 

outliers %>% is.na() %>% sum() #Great!

chpoint_12 <- df
df <- chpoint_12












