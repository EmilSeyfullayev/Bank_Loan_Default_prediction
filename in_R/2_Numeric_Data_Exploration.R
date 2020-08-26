library(tidyverse)
library(data.table)
library(inspectdf)

df %>% 
  select_if(is.numeric) %>% 
  names() %>% 
  length() #38 numeric

df %>% 
  select_if(is.character) %>% 
  names() %>%
  length() # no character

df %>% 
  select_if(is.factor) %>% 
  names() %>% 
  length() #14 factors (already processed)

df %>% 
  select_if(is.numeric) %>% 
  colnames() -> num_names

num_names

# remove id -----
df$id %>% unique() %>% length()
#466256 uniques >> drop
df %>% 
  select(-id) -> df

# remove member_ID----
num_names[2]
df$member_id %>% unique() %>% length()
# drop simply
df %>% 
  select(-member_id) -> df


#Till now most of the values are not NA
#Lets focus on them
#some have > 50% of Missing, lets drop them

df %>% 
  select_if(is.numeric) %>% 
  inspect_na() %>% 
  as.data.frame() %>% 
  filter(pcnt>50) %>% 
  .[[1]] -> fiftyPcntMissing

df %>% 
  select(-fiftyPcntMissing) -> df

df %>% 
  select_if(is.numeric) %>% 
  inspect_na() %>% 
  filter(pcnt>0) %>% 
  .[[1]] -> num_having_missing

# the highest has 15% of missing

#1 tot_coll_amt-----
num_having_missing[1]
df$tot_coll_amt %>% unique() %>% length()
# 6322 uniques
df %>% 
  inspect_na() %>% 
  filter(col_name=='tot_coll_amt')
# 15% of data is missing
# let's see distribution

df %>% 
  ggplot(aes(tot_coll_amt))+
  geom_histogram()

#distribution shows it to be around 0
# let's see some values
df$tot_coll_amt[100000:150000]
#Most of the values are zero bu there are some non zero

df$tot_coll_amt %>% na.omit() %>%  max() #9152545
df$tot_coll_amt %>% na.omit() %>% min() #0
df$tot_coll_amt %>% na.omit() %>% sort(decreasing = T)

df %>% 
  arrange(desc(tot_coll_amt)) %>% 
  filter((tot_coll_amt==0)) %>% 
  count() #49750 values are non zero from 466256
#let's see their distribution

df %>% 
  arrange(desc(tot_coll_amt)) %>% 
  filter(!(tot_coll_amt==0)) -> df_tot_coll_amt

df_tot_coll_amt %>% 
  ggplot(aes(tot_coll_amt))+ 
  geom_histogram() #again we cannot see distribution

#because distribution is close to zero
df_tot_coll_amt %>% 
  filter(tot_coll_amt>500000) %>% 
  ggplot(aes(tot_coll_amt))+
  geom_histogram() #changing value does not show result
#so, I think we have to plot barplot for continous value

df_tot_coll_amt %>% 
  filter(tot_coll_amt>500000) %>% 
  ggplot(aes(tot_coll_amt %>% 
               as.character() %>% 
               as.factor()))+
  geom_bar() # again no result

df_tot_coll_amt %>% 
  filter(tot_coll_amt>100000) %>% 
  ggplot(aes(tot_coll_amt))+
  geom_histogram(bins = 450) #I change number, no result

# I cannot see distribution, filter or binsize doesnot help
# I think to create a foor loop to access data distribution
# also we can use correlation with loan amount

cor(
  x = df_tot_coll_amt$tot_coll_amt,
  y = df_tot_coll_amt$loan_amnt
)
# correlation is almost zero

#So let's create for loop 

collat <- c()

for (i in df_tot_coll_amt$tot_coll_amt){
  if (i>=9000000){
    collat <- append(collat, 10)
  }else if(i>=8000000 & i<9000000){
    collat <- append(collat,9)
  }else if(i>=7000000 & i<8000000){
    collat <- append(collat,8)
  }else if(i>=6000000 & i<7000000){
    collat <- append(collat,7)
  }else if(i>=5000000 & i<6000000){
    collat <- append(collat,6)
  }else if(i>=4000000 & i<5000000){
    collat <- append(collat,5)
  }else if(i>=3000000 & i<4000000){
    collat <- append(collat,4)
  }else if(i>=2000000 & i<3000000){
    collat <- append(collat,3)
  }else if(i>=1000000 & i<2000000){
    collat <- append(collat,2)
  }else{
    collat <- append(collat,1)
  }
}

collat %>% table() %>% data.frame()
# we have just one value with index 10
df %>% 
  filter(tot_coll_amt>9000000) %>% 
  select(tot_coll_amt) #Its value is 9,152,545
# all the rest with index 1
# so, now I think we will be able to access distribution

df_tot_coll_amt %>% 
  filter(tot_coll_amt<1000000) %>% 
  ggplot(aes(tot_coll_amt))+
  geom_histogram(bins = 450) #great progress

df_tot_coll_amt %>% 
  filter(tot_coll_amt<100000) %>% 
  ggplot(aes(tot_coll_amt))+
  geom_histogram(bins = 450)#lower by 10 than previos

df_tot_coll_amt %>% 
  filter(tot_coll_amt<10000) %>% 
  ggplot(aes(tot_coll_amt))+
  geom_histogram(bins = 450) #lower by 10 than previos

df_tot_coll_amt %>% 
  filter(tot_coll_amt<7000) %>% 
  ggplot(aes(tot_coll_amt))+
  geom_histogram(bins = 450) #great

#We will reassign those values to the upper and lowe fences

library(graphics)
boxplot(df$tot_coll_amt)$out %>% 
  sort(decreasing = F)

up_fence <- 
  quantile(df_tot_coll_amt$tot_coll_amt,
           0.75, na.rm = T)+
  IQR(df_tot_coll_amt$tot_coll_amt,
      na.rm = T)
low_fence <- 
  quantile(df_tot_coll_amt$tot_coll_amt, 
           0.25, na.rm = T)-
  IQR(df_tot_coll_amt$tot_coll_amt, na.rm = T)

#Dont forget that we deal only with non-zeros, 50,000
#Actually, if 415,000 observations are zeros
#No meaning left of value change
#Let's simply reassign them 0 for zeros and NAs
#And 1s for amounts

ifelse(
  df$tot_coll_amt %in% c(0, NA),
  0,
  1
) -> df$tot_coll_amt

chpoint_8 <- df
df <- chpoint_8
#2 tot_cur_bal----
num_having_missing[2]
df[,"tot_cur_bal"] %>% inspect_na() #15% missing
df$tot_cur_bal %>% unique() %>% length() 
#220691 uniques >> continous

df %>% 
  ggplot(aes(tot_cur_bal))+
  geom_histogram(color='black',
                 fill='red',
                 bins = 450)+
  theme_bw()

df$tot_cur_bal %>% max(na.rm = T) #8,000,0078
df$tot_coll_amt %>% max(na.rm = T) #1
df$tot_coll_amt %>% median(na.rm = T) # 0
df$tot_coll_amt %>% mean(na.rm = T) # 0,107

#let's reassign outliers to lower and upper fences

up_fence <- 
  quantile(df$tot_cur_bal,na.rm = T, 0.75)+
  IQR(df$tot_cur_bal,na.rm = T)*1.5 #479455.5

low_fence <- 
  quantile(df$tot_cur_bal, na.rm = T, 0.25)-
  IQR(df$tot_cur_bal, na.rm = T)*1.5 #-241884
#since min value is 0, 
#we will not reassign lower fence outliers


outliers <-  boxplot(df$tot_cur_bal)$out
outliers %>% length()
#length of otliers is 12,264
df[
  df$tot_cur_bal %in% outliers,
  'tot_cur_bal'
] %>% count()
#count of outliers  is 12264 
#Great
#Now simply reassign
df[
  df$tot_cur_bal %in% outliers,
  'tot_cur_bal'
] <- up_fence

df %>% 
  ggplot(aes(tot_cur_bal))+
  geom_histogram(color='black',
                 fill='red',
                 bins=450)+
  theme_bw()

#let's check correlation with loan_amnt
cor(
  df$loan_amnt,
  df$tot_cur_bal %>% 
    nafill(fill = 
              median(
                df$tot_cur_bal,
                na.rm = T
              ))
) #0.333 >> there is a positive correlation

#let's check with loan_status borrower

df %>% 
  ggplot(aes(loan_status %>% as.factor(),
             tot_cur_bal))+
  geom_boxplot()+
  theme_bw()
#median of  tot_cur_balance 
#of those who paid their loans is higher

#let's check according to grades
df %>% 
  ggplot(aes(grade, tot_cur_bal))+
  geom_boxplot()+
  theme_bw() #there are quite big differences

#let's see whether na's are more in certain grade
df[
  is.na(df$tot_cur_bal),
  grade
] %>% table() %>% as.data.frame()
#yes, there is big portion of NAs in A, B, C grades

#so, we can fill NAs according to
#loan status and grade

df %>% 
  group_by(grade, loan_status) %>% 
  summarise(median(tot_cur_bal %>% 
                     na.omit())) %>%
  as.data.frame() %>% 
  rename(median=colnames(.)[3])

# grade loan_status   median
# 1      A           0 125116.0
# 2      A           1 145391.0
# 3      B           0  63535.0
# 4      B           1  85779.0
# 5      C           0  59706.0
# 6      C           1  73487.0
# 7      D           0  53025.0
# 8      D           1  65870.0
# 9      E           0  62033.0
# 10     E           1  71245.5
# 11     F           0  57286.0
# 12     F           1  65206.0
# 13     G           0  71562.0
# 14     G           1  81001.0

df[is.na(df$tot_cur_bal) &
     grade =="A" &
     loan_status==0,
     "tot_cur_bal"
   ] <- 125116

df[is.na(df$tot_cur_bal) &
     grade =="A" &
     loan_status==1,
   "tot_cur_bal"
] <- 145391

df[is.na(df$tot_cur_bal) &
     grade =="B" &
     loan_status==0,
   "tot_cur_bal"
] <- 63535

df[is.na(df$tot_cur_bal) &
     grade =="B" &
     loan_status==1,
   "tot_cur_bal"
] <- 85779

df[is.na(df$tot_cur_bal) &
     grade =="C" &
     loan_status==0,
   "tot_cur_bal"
] <- 59706

df[is.na(df$tot_cur_bal) &
     grade =="C" &
     loan_status==1,
   "tot_cur_bal"
] <- 73487

#the rest will be filled with median
med <- df$tot_cur_bal %>% 
  median(na.rm = T) #85779

df[
  df$tot_cur_bal %>% is.na(),
  "tot_cur_bal"
] <- med

df[,"tot_cur_bal"] %>% inspect_na() #no NAs

df %>% 
  ggplot(aes(tot_cur_bal))+
  geom_histogram(color='black', 
                 fill='red')

#3 total_rev_hi_lim-----
num_having_missing[3]
df[, "total_rev_hi_lim"] %>% inspect_na() 
#15% missing
df$total_rev_hi_lim %>% unique() %>% length()
#14613 uniques, not to be discrete
df$total_rev_hi_lim[345678] #example
df$total_rev_hi_lim[248678] #example

#let's see distribution
df %>% 
  ggplot(aes(total_rev_hi_lim))+
  geom_histogram() # alot of outliers

boxplot(df$total_rev_hi_lim)$out -> outliers

min(df$total_rev_hi_lim, na.rm = T) # 0

chpoint_9 <- df
df <- chpoint_9


up_fence <- 
  quantile(df$total_rev_hi_lim,0.75, na.rm = T)+
  IQR(df$total_rev_hi_lim, na.rm = T) #62,300
  
#reassign outliers to upperfence
df[
  df$total_rev_hi_lim %in% outliers,
  "total_rev_hi_lim"
] <- up_fence 

#see NAs
df[df$total_rev_hi_lim %>% is.na(),] %>% 
  group_by(grade) %>% 
  summarise(n()) %>% 
  rename(NA_count=names(.)[2]) %>% 
  as.data.frame()

# grade NA_count
# 1     A    16735
# 2     B    21778
# 3     C    14539
# 4     D     9578
# 5     E     5013
# 6     F     1944
# 7     G      660

#Again, NA count is more in A, B, C grades

df[!is.na(df$total_rev_hi_lim),] %>% 
  ggplot(aes(grade, total_rev_hi_lim))+
  geom_boxplot()+
  facet_grid(loan_status~.)+
  theme_bw()

# Again same type of situation
# If grade is higher, example A,
# total_rev_hi_lim is higher
# and if loan_status is 1 total_rev_hi_lim 
# is higher than if loan_status 0

df %>% 
  group_by(grade, loan_status) %>% 
  summarise(median(total_rev_hi_lim,
                   na.rm = T)) %>% 
  as.data.frame() %>% 
  rename(median=names(.)[3])

# grade loan_status median
# 1      A           0  31000
# 2      A           1  34900
# 3      B           0  22300
# 4      B           1  23500
# 5      C           0  20100
# 6      C           1  20900
# 7      D           0  18400
# 8      D           1  19600
# 9      E           0  20300
# 10     E           1  20300
# 11     F           0  19900
# 12     F           1  19700
# 13     G           0  19150
# 14     G           1  21800

df[is.na(total_rev_hi_lim)&
     grade=="A" &
     loan_status==0,
   "total_rev_hi_lim"] <- 31000

df[is.na(total_rev_hi_lim)&
     grade=="A" &
     loan_status==1,
   "total_rev_hi_lim"] <- 34900

df[is.na(total_rev_hi_lim)&
     grade=="B" &
     loan_status==0,
   "total_rev_hi_lim"] <- 22300

df[is.na(total_rev_hi_lim)&
     grade=="B" &
     loan_status==1,
   "total_rev_hi_lim"] <- 23500

df[is.na(total_rev_hi_lim)&
     grade=="C" &
     loan_status==0,
   "total_rev_hi_lim"] <- 20100

df[is.na(total_rev_hi_lim)&
     grade=="C" &
     loan_status==1,
   "total_rev_hi_lim"] <- 20900

df[is.na(total_rev_hi_lim)&
     grade=="D" &
     loan_status==0,
   "total_rev_hi_lim"] <- 18400

df[is.na(total_rev_hi_lim)&
     grade=="D" &
     loan_status==1,
   "total_rev_hi_lim"] <- 19600 

df[is.na(total_rev_hi_lim)&
     grade=="E" &
     loan_status==0,
   "total_rev_hi_lim"] <- 20300

df[is.na(total_rev_hi_lim)&
     grade=="E" &
     loan_status==1,
   "total_rev_hi_lim"] <- 20300 

df[is.na(total_rev_hi_lim)&
     grade=="F" &
     loan_status==0,
   "total_rev_hi_lim"] <- 19900 

df[is.na(total_rev_hi_lim)&
     grade=="F" &
     loan_status==1,
   "total_rev_hi_lim"] <- 19700

df[is.na(total_rev_hi_lim)&
     grade=="G" &
     loan_status==0,
   "total_rev_hi_lim"] <- 19150 

df[is.na(total_rev_hi_lim)&
     grade=="G" &
     loan_status==1,
   "total_rev_hi_lim"] <- 21800
  
df[, "total_rev_hi_lim"] %>% inspect_na()
#Great

#4 revol_util------
num_having_missing[4]
df[, "revol_util"] %>% inspect_na() #0.667%

df$revol_util %>% unique() %>% length()
#1270 uniques

df %>% 
  ggplot(aes(loan_status %>% as.factor(),
             revol_util))+
  geom_boxplot() -> p
#as we cannot see boxes, 
#let's see with plotly
library(plotly)
p %>% ggplotly()
# median for 0 loan_status: 61,30
# median for 1 loan_status: 57,10
#let's fill accordingly
df[
  df$revol_util %>% is.na() &
    df$loan_status==0,
  "revol_util"
] <- 61.30

df[
  df$revol_util %>% is.na() &
    df$loan_status==1,
  "revol_util"
] <- 57.10


df[,"revol_util"] %>% inspect_na()#great

#5 collections_12_mths_ex_med------
num_having_missing[5]

df$collections_12_mths_ex_med %>% unique()
#even though there are few uniques,
#variable is continous

df %>% 
  ggplot(aes(loan_status %>% as.factor(),
             collections_12_mths_ex_med))+
  geom_boxplot() -> p

#as we cannot see boxes, let's see with plotly
p %>% ggplotly()
# most of the collections = 0

median(df$collections_12_mths_ex_med, 
       na.rm = T) # yes, = 0
mean(df$collections_12_mths_ex_med, 
       na.rm = T) # almost zero, = 0,009

nafill(df$collections_12_mths_ex_med,
       fill = 0) %>% is.na() %>% sum() 
#nafill works
df$collections_12_mths_ex_med <- 
  nafill(df$collections_12_mths_ex_med,
         fill = 0)

df[,"collections_12_mths_ex_med"] %>% 
  inspect_na() #Great

chpoint_10 <- df
df <- chpoint_10
