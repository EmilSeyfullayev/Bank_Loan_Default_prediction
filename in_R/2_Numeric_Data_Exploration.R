library(tidyverse)
library(data.table)
library(inspectdf)

df %>% 
  select_if(is.numeric) %>% 
  names() %>% 
  length() 

df %>% 
  select_if(is.character) %>% 
  names() %>%
  length() 

df %>% 
  select_if(is.factor) %>% 
  names() %>% 
  length()

df %>% 
  select_if(is.numeric) %>% 
  colnames() -> numnames

numnames

#1 tot_coll_amt-----
numnames[1]
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
  count() #49750 values are non zero
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
    collat <- append(collat, 9)
  }else if(i>=8000000 & i<9000000){
    collat <- append(collat,8)
  }else if(i>=7000000 & i<8000000){
    collat <- append(collat,7)
  }else if(i>=6000000 & i<7000000){
    collat <- append(collat,6)
  }else if(i>=5000000 & i<6000000){
    collat <- append(collat,5)
  }else if(i>=4000000 & i<5000000){
    collat <- append(collat,4)
  }else if(i>=3000000 & i<4000000){
    collat <- append(collat,3)
  }else if(i>=2000000 & i<3000000){
    collat <- append(collat,2)
  }else if(i>=1000000 & i<2000000){
    collat <- append(collat,1)
  }else{
    collat <- append(collat,0.5)
  }
}

collat %>% table() %>% data.frame()
# we have just one value with index 9
# all the rest with index 0.5
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





