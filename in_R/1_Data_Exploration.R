library(tidyverse)
library(data.table)
library(inspectdf)

df <- fread('loan_data_2007_2014.csv')
df %>% select(-V1) -> df

#1 Missing values Exploration ------ 
#Let's check what are the percentage of missing values
df %>% 
  inspect_na() %>% 
  as.data.frame() %>% 
  select(col_name, pcnt) %>% 
  mutate(percentage=round(pcnt,3)) %>% 
  select(col_name, percentage)

#Exclude variables which have more than 50% of missing values
df %>% 
  inspect_na() %>% 
  as.data.frame() %>% 
  select(col_name, pcnt) %>% 
  mutate(percentage=round(pcnt,3)) %>% 
  select(col_name, percentage) %>% 
  filter(!(percentage>50)) %>% 
  .$col_name -> colnames

df <- df %>% select(colnames)

df %>% ncol() #So we left with 54 variables

#2 Each character variable----
write.csv(df, 'df.csv')

df %>% 
  select(-V1) -> df

df %>% glimpse()

df %>% 
  select_if(is.character) %>% 
  names() -> char_names ;char_names %>% length() #22

#1 term-------
char_names[1]
df %>% select(char_names[1]) %>% unique() 
#only two uniques, so, let's make it factorial
df$term <- df$term %>% as.factor()

#2 grade-------
df %>% select(char_names[2]) %>% unique() %>% count()
#only seven uniques, no missing >> factor
df$grade <- df$grade %>% as.factor()

#3 subgrade------
df %>% select(char_names[3]) %>% unique() %>% .[[1]]
df %>% select(char_names[3]) %>% unique() %>% count()
#no missing, 35 uniques >> factor
df$sub_grade <- df$sub_grade %>% as.factor()

#4 emp_title---------
df %>% select(char_names[4]) %>% unique() %>% count() 
#201076 uniques
df %>% select(char_names[4]) %>% 
  unique() %>%
  .[[1]] %>% 
  .[1:50] #first 50 values
#too many uniqe values, so, drop it
df %>% 
  select(-char_names[4]) -> df

#5 emp_lenght-------
df %>% select(char_names[5]) %>% unique() %>% count()
df %>% select(char_names[5]) %>% unique() %>% .[[1]]
#Uniques are 12, 1 of them is missing
#Make it factor and
#See distribution


library(ggplot2)

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
  geom_boxplot()

#As we can see, we could impute with ">10 years" as it is mode
#But we see that, those have missing "emp_length" have less amount credited
#So, their emp length shall be as lower as "< 1year"
#Because their loan amount median is lower than all the rest

df$emp_length %>% unique() %>% .[2] -> y
df[df$emp_length=="","emp_length"] <- y

df$emp_length %>% as.factor() -> df$emp_length

#6 home_ownership--------

df %>% select(char_names[6]) %>% unique() %>% .[[1]]
df %>% select(char_names[6]) %>% unique() %>% count()
#number of unique is 6
#just make them factor
char_names[6]
df$home_ownership %>% as.factor() -> df$home_ownership

#7 verification_status------
df %>% select(char_names[7]) %>% unique() %>% .[[1]]
#just 3 unique values >> as factor
char_names[7]
df$verification_status %>% as.factor() -> df$verification_status

#8 XXXissue_d -----
char_names[8]

df %>% select(char_names[8]) %>% unique() %>% count()
#91 unique values, day of month and month
df$issue_d %>% .[1] %>% typeof() #character
#We have to transform it to the Date
#However we do not have Year

write.csv(df, "df.csv")

#9 XXXXloan_status ~ Y variable----
char_names[9]
df %>% select(char_names[9]) %>% unique() %>% count()
# number of uniques is 9
df$loan_status %>% unique()
#This is actually our Y variable
#It has 9 uniques but will make it to have 2 uniques








#10 pymnt_plan-------
char_names[10]
df$pymnt_plan %>% unique()
#has two uniques, "n" and "y" >>> as factor
df$pymnt_plan <- df$pymnt_plan %>% as.factor()
#11 URL----
char_names[11]
df$url %>% unique() %>% length
#all values are unique
df$url[1] #lets examine one 
#it returns url to lending club memebrs having acces
#just drop this variable
df %>% 
  select(-url) -> df

chpoint_1 <- df

#12 desc------
char_names[12]
df$desc %>% unique() %>% length()
#124420 unique values
df$desc[1]
df$desc[424774]
df$desc[1875]
df$desc %>% class()
#it is character data type and has huge uniques
#We will simply drop it
df %>% 
  select(-desc) -> df

#13 purpose----
char_names[13]
df$purpose %>% unique() %>% length()
#14 unique values
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

#15 XXXzip_code----
char_names[15]
df$zip_code %>% unique() %>% length()
#888 unique values
888/nrow(df)
df$zip_code[2445] 
#by changing index seem that all have "xx" at the end
#let's make valid our guess

grepl('xx', df$zip_code, fixed = T) %>% 
  sum() %>% 
  .[1]/nrow(df) # =1, means that all zip codes have "xx"

#lets remove them and see distribution of zipcodes
gsub("xx",
     "",
     df$zip_code) %>% 
  as.numeric() %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(Freq %>% desc()) ->zip_code
  

zip_code %>% glimpse() 
# "." is factorial #they are zip code names
zip_code %>% 
  rename(ZC_name='.') -> zip_code

zip_code$ZC_name <- zip_code$ZC_name %>% 
  as.character() %>% 
  as.numeric()

zip_code %>% glimpse()

zip_code %>% 
  ggplot(aes(Freq))+
  geom_histogram(color='black',
                 fill='red')+
  theme_bw()

df %>% 
  ggplot(aes(zip_code))+
  geom_bar(color='black',
                 fill='red')
#Frequence of most of them is same
#We can make three classes of it by putting tresholds
zip_code %>% head()

chpoint_2 <- df

#16 addr_state----
char_names[16]
df$addr_state %>% unique() %>% length()
#50 unique values
df$addr_state %>% length() %>% .[1]/nrow(df)
#no missing values
#>>> just make it factor 
df$addr_state <- df$addr_state %>% as.factor()

#17 XXXXearliest_cr_lin------
char_names[17]
df$earliest_cr_line %>% unique() %>% length()
# 665 unique values
















