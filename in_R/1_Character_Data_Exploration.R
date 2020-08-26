library(tidyverse)
library(data.table)
library(inspectdf)

df <- fread('loan_data_2007_2014.csv',
            na.strings = c(NA, ""))
#read NA and "" as strings
df$V1 %>% unique() %>% length()
df %>% 
  rename(ID = V1) -> df
df$ID <- as.factor(df$ID)
#1 Missing values Exploration ------ 
#Let's check what are the percentage of missing values
df %>% 
  inspect_na() %>% 
  as.data.frame() %>% 
  select(col_name, pcnt) %>% 
  mutate(percentage=round(pcnt,3)) %>% 
  select(col_name, percentage)

#We have
# 22 char var
# 17 logical var
# 35 numeric var

df %>% 
  select_if(is.logical) %>% 
  inspect_na() # all logical variables are 100% NA


# #Exclude variables which have more than 50% of missing values
# df %>%
#   inspect_na() %>%
#   as.data.frame() %>%
#   select(col_name, pcnt) %>%
#   mutate(percentage=round(pcnt,3)) %>%
#   select(col_name, percentage) %>%
#   filter(!(percentage>50)) %>%
#   .$col_name -> colnames
# 
# df <- df %>% select(colnames)
# 
# df %>% ncol() #So we left with 54 variables

#We will drop logicals at the end

#2 Each character variable----

df %>% glimpse()

df %>% 
  select_if(is.character) %>% 
  names() -> char_names 

char_names %>% length() #22

#1 term-------
char_names[1]
df %>% select(char_names[1]) %>% unique() 
#only two uniques, so, let's make it factorial
df[,'term'] %>% inspect_na() # no NA
df$term <- df$term %>% as.factor()

#2 grade-------
df %>% select(char_names[2]) %>% unique() %>% count()
df$grade[is.na(df$grade)] # no na
#only seven uniques, no missing >> factor
df$grade <- df$grade %>% as.factor()

#3 subgrade------
df %>% select(char_names[3]) %>% unique() %>% .[[1]]
df %>% select(char_names[3]) %>% unique() %>% count()
df %>% select(char_names[3]) %>% inspect_na()
#no missing, 35 uniques >> factor
df$sub_grade <- df$sub_grade %>% as.factor()

#4 emp_title---------
df %>% select(char_names[4]) %>% 
  unique() %>% 
  count() 
#201076 uniques
df %>% select(char_names[4]) %>% 
  unique() %>%
  .[[1]] %>% 
  .[1:50] #first 50 values

#too many uniqe values, so, drop it
df %>% 
  select(-char_names[4]) -> df

#5 emp_length-------
df %>% select(char_names[5]) %>% unique() %>% count()
df %>% select(char_names[5]) %>% unique() %>% .[[1]]
#Uniques are 12, 1 of them is missing
df[,"emp_length"] %>% inspect_na() # 5% is missing
#See distribution

df$emp_length %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  ggplot(aes(x=., y=Freq))+
  geom_bar(stat = 'identity', aes(fill=Freq))+
  theme_bw()

#As we can see, frequence of given loans is high 
#if you are employed >10 years.
#its count is 150.000, second count whereas is just 40.000
#We can fill missings with ">10 years",
#but let's see if any other correlations
df %>% 
  ggplot(aes(emp_length, loan_amnt))+
  geom_boxplot()+
  theme_bw()

#As we can see, we could impute with ">10 years" as it is mode
#But we see that, those have missing "emp_length" have less amount credited
#So, their emp length shall be as lower as "< 1year"
#Because their loan amount median is lower than all the rest

df %>% 
  group_by(emp_length) %>% 
  summarise(median(loan_amnt)) %>% 
  arrange(.[,2])
#We see that if we take into account loan amount
#we have to fill NA with < 1 year
#because it is closest to it value

df$emp_length %>% unique() %>% .[2] -> y
df[is.na(df$emp_length),"emp_length"] <- y

df$emp_length %>% unique() #no NA

#Since this value is contnous, we have to replace
#characters and parse numbers
df$emp_length %>% parse_number()
#We simply can assign it to the emp_length feature
#Keep in mind, < 1 year and 1 year will be merged
#if we look at the median values for
# <1 year it is 10200
# 1 year it is 11450
# not that much difference, but we can change it

#let's create checkpoint for now
chpoint_1 <- df
df <- chpoint_1
df[df$emp_length=="< 1 year", "emp_length"] <- 0
df$emp_length %>% unique() #great
df$emp_length %>% parse_number() %>% unique() #great
df[, "emp_length"] <- df$emp_length %>% parse_number()

#6 home_ownership--------

df %>% select(char_names[6]) %>% unique() %>% .[[1]]
df %>% select(char_names[6]) %>% unique() %>% count()
#number of unique is 6
df %>% 
  group_by(home_ownership) %>% 
  summarise(n()) %>% 
  arrange(desc(.[,2]))
# Other=182, None=50, Any=1, let's combine them
df[
  df$home_ownership %in% c('OTHER',
                        'NONE',
                        'ANY'),
  "home_ownership"
] #total 233, so let's reaasign them to "OTHER"
df[
  df$home_ownership %in% c('OTHER',
                           'NONE',
                           'ANY'),
  "home_ownership"
] <- "OTHER"

df$home_ownership %>% unique() #great

#and just make them factor
df$home_ownership %>% as.factor() -> df$home_ownership

#7 verification_status------
df %>% select(char_names[7]) %>% unique() %>% .[[1]]
#just 3 unique values

df[,"verification_status"] %>% inspect_na() 
#no NA

df %>% 
  group_by(verification_status) %>% 
  summarise(n())
# almost equal distribution among three
# no need for combination
#just make it as factor

df$verification_status %>% as.factor() -> 
  df$verification_status

df[, "verification_status"] %>% glimpse() #great


#8 issue_d -----
char_names[8]

df %>% select(char_names[8]) %>% unique() %>% count()
#91 unique values, day of month and month
df[,'issue_d'] %>% inspect_na() #no NA
#We have to transform it to the Date
#However we do not have Year (2007-2015 range though)
#So, just make it factor
df$issue_d %>% 
  as.factor() -> df$issue_d

df[,"issue_d"] %>% glimpse()

#9 loan_status ~ Y variable----
char_names[9]
df %>% select(char_names[9]) %>% unique() %>% count()
# number of uniques is 9
df$loan_status %>% unique()
#This is actually our Predicted variable
#It has 9 uniques but will make it to have 2 uniques
#We have separate them into two
#Let's make chpoint
chpoint_2 <- df
df <- chpoint_2

# will be encoded as 1 who 
# Fully Paid, Current, In grace period
# rest as defaulted
df$loan_status %>% 
  unique() %>% 
  .[c(1, 3, 6)] -> paid_1

df[
  df$loan_status %in% paid_1,
  'loan_status'
] #412111 count

ifelse(
  df$loan_status %in% paid_1,
  1,
  0
) -> df$loan_status

df %>% 
  filter(loan_status==1) %>% 
  count() #412111  #great!



#10 pymnt_plan-------
char_names[10]
df$pymnt_plan %>% unique()
df[, "pymnt_plan"] %>% inspect_na() #no NA
#has two uniques, "n" and "y" >>> as factor
df$pymnt_plan <- df$pymnt_plan %>% as.factor()
df[, "pymnt_plan"] %>% glimpse() #great

#11 URL----
char_names[11]
df$url %>% unique() %>% length
#all values are unique
df$url[1] #lets examine one 
#it returns url to lending club memebrs having acces
#just drop this variable
df %>% 
  select(-url) -> df

#12 desc------
char_names[12]
df$desc %>% unique() %>% length()
#124420 unique values
df$desc[1]
df$desc[424774]
df$desc[1875]
df$desc %>% class()
#it has huge number of uniques
#We will simply drop it
df %>% 
  select(-desc) -> df

#13 purpose----
char_names[13]
df$purpose %>% unique() %>% length()
#14 unique values, no NA
df$purpose %>% unique()
df$purpose %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(Freq %>% desc())

df$purpose %>% length()
#no missing, 14 uniques >> as.factor()
df$purpose <- 
  df$purpose %>% 
  as.factor()

#14 title----
char_names[14]
df$title %>% unique() %>% length() #61395 uniques
#We could process this data, but we need NLP tools to apply

df %>% 
  select(-title) -> df
#15 zip_code----
char_names[15]
df$zip_code %>% unique() %>% length()
#888 unique values
888/nrow(df) #0.0019% of the data
df$zip_code[2445] 
#by changing index seem that all have "xx" at the end
#let's make valid our guess

grepl('xx', df$zip_code, fixed = T) %>% 
  sum() %>% 
  .[1]/nrow(df) # =1, means that all zip codes have "xx"

#But it does not mean that all have it in the end
#How to check it
#we can use sql language 'like' operator (I simply know it)

library(sqldf)
df1 <- df %>% select(-ID)

sqldf("SELECT zip_code 
      FROM df1 
      WHERE zip_code like '___xx'") -> sql_zipcode

nrow(sql_zipcode)/nrow(df) # =1, which means all have "___xx" pattern

#lets remove them and see distribution of zipcodes

df %>% 
  group_by(zip_code) %>% 
  summarise(n()) %>% 
  arrange(.[,2] %>% desc())

gsub("xx",
     "",
     df$zip_code) -> df$zip_code 

df$zip_code %>% class()  #character


df %>% 
  ggplot(aes(zip_code %>% as.numeric()))+
  geom_histogram(color='black', fill='red', bins = 60)+
  scale_x_continuous(breaks = seq(0,1000,50))+
  theme_bw() 
#Here we treat zipcode as a continous variable
#But let's treat it as factor
  
df$zip_code %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(Freq %>% desc()) -> zip_code
  

zip_code %>% glimpse() 
# "." is factorial #they are zip code names
zip_code %>% 
  rename(name='.') -> zip_code

zip_code %>%
  head(200) %>% 
  ggplot(aes(name, Freq))+
  geom_bar(color='black',
           fill='red',
           stat = 'identity')+
  theme_bw()
#if fully seen (not head of first 200) we cannot see anthg
#generally, histogram is more useful even if 
#factor variable is seen as a continous

#We can make three classes of it by putting tresholds
#let's see distribution of counts in order to set tresholds

zip_code %>% 
  ggplot(aes(Freq))+
  geom_histogram(color='black', fill='red')+
  scale_x_continuous(breaks = seq(0,6000, 250))+
  theme_bw()

#based on distribution of counts of unique zipcodes
#we created 7 classes

l <- c()

for (i in zip_code$Freq){
  if (i<250){
    l <- append(l, 1)
  }
  else if(i<500){
    l <- append(l, 2)
  }
  else if(i<750){
    l <- append(l, 3)
  }
  else if(i<1000){
    l <- append(l, 4)
  }
  else if(i<1750){
    l <- append(l,5)
  }
  else if(i<2500){
    l <- append(l,6)
  }else{
    l <- append(l,7)
  }
}

zip_code$class <- l

zip_code %>% head()

#let's now make classification for entire data set's zipcodes
#Transform factor to numeric then to character
zip_code$name <- zip_code$name %>% 
  as.factor()

zip_code %>% glimpse()
#we can merge datasets
#first of all let's create checkpoint
chpoint_3 <- df
df <- chpoint_3
#also dont forget to make them as factors,
#otherwise leftjon will work very slowly on joining

df$zip_code <- df$zip_code %>% 
  as.factor()

df %>% 
  left_join(zip_code,
      by = c('zip_code'='name')) -> df_zip

#now remove Freq and zip_code and rename class
df_zip %>%
  select(-Freq, -zip_code) %>% 
  rename(zip_class=class) -> df_zip

df <- df_zip
df[, "zip_class"] %>% glimpse() #double >>factor
df$zip_class <- df$zip_class %>% as.factor()
df[, "zip_class"] %>% inspect_na()

#16 addr_state----
char_names[16]
df$addr_state %>% unique() %>% length()
#50 unique values
df[is.na(df$addr_state),] %>% nrow() #No NAs

#>>> just make it factor 
df$addr_state <- df$addr_state %>% as.factor()

#17 earliest_cr_line------
#date variable
char_names[17]
df$earliest_cr_line %>% unique() %>% length()
# 652 unique values
df$earliest_cr_line[1] #format(%b-%y)
#let's paste day date to every value and transform to date
paste0("01-", df$earliest_cr_line) %>% 
  as.Date("%d-%b-%y") %>% is.na() %>% sum() #29 NAs

chpoint_4 <- df
df <- chpoint_4

df[, "earliest_cr_line"] <- 
  paste0("01-", df$earliest_cr_line) %>% 
  as.Date("%d-%b-%y")
df[, "earliest_cr_line"] %>% glimpse() #date format

#format is corrected
#let's check nulls
df[is.na(df$earliest_cr_line), "earliest_cr_line"] #29 NAs
#It is quite hard to predict date, 
#we have to simply drop 29 observations

df[!is.na(df$earliest_cr_line),] -> df

#Now, in order to use this column we have to transform it 
#to the continous values
#We can do it by subtracting from 2017 year
'2017-01-01' %>% 
  as.Date() %>% 
  .[1]-df$earliest_cr_line -> time_diff

time_diff %>% class() #difftime type
time_diff %>% as.numeric() -> time_diff
time_diff %>% class()
time_diff[time_diff<0] %>% length() #1169 have negative value
#What a big mistake
#we had 70 years, for example, 
#we assued them to be 1970
#bu they were transformed as 2070

#as we have 100 years of mistake we have to simply add them
100*12*30 -> to_be_added
time_diff[time_diff<0] <- 
  time_diff[time_diff<0] + to_be_added

time_diff %>% length() #great

time_diff %>% class() #numeric

#convert to difference in months
round(time_diff/30, 0) -> time_diff
chpoint_5 <- df #checkpoint
df <- chpoint_5
df$earliest_cr_line <- time_diff

df[,"earliest_cr_line"] %>% glimpse()
df[,"earliest_cr_line"] %>% inspect_na() #great

df %>% 
  rename(Months_past_first_cr=
           earliest_cr_line)->df
#18 Initial_list_status------
char_names[18]
df$initial_list_status %>% unique() %>% length()
# just 2 uniques
df$initial_list_status %>% unique()
# f anf w
df[is.na(df$initial_list_status),]
df[df$initial_list_status=="", "initial_list_status"]
#there is no missing values as well as blank
#>>> as factor
df$initial_list_status <- 
  df$initial_list_status %>% as.factor()

#19 last_pymnt_d -----
char_names[19]
df$last_pymnt_d %>% unique() %>% length()
# just 99 unique values of date_type
df$last_pymnt_d <- 
  df$last_pymnt_d %>% as.factor()

#20 next_pymnt_d---- 
char_names[20]
df$next_pymnt_d %>% unique() %>% length()
# 100 unique dates

df$next_pymnt_d <- 
  df$next_pymnt_d %>% as.factor()


#21 last_credit_pull_d ----
char_names[21]
df$last_credit_pull_d %>% unique() %>% length()
# 104 unique values
df$last_credit_pull_d <- 
  df$last_credit_pull_d %>% as.factor()

#22 application_type----- 
char_names[22]
df$application_type %>% unique() %>% length()
# just 1 unique >>> drop
df %>% 
  select(-application_type) -> df

chpoint_6 <- df
df <- chpoint_6
df %>% 
  select_if(is.logical) %>%
  .[complete.cases(.),] # no datatable to show
#so, simply drop logical variables

df %>% 
  select_if(is.logical) %>% 
  colnames() -> log_names #17 NAs >> 70-17=53 
#we should get 53 columns at the end

df %>% 
  select(-log_names) -> df

write.csv(df, "df.csv")







