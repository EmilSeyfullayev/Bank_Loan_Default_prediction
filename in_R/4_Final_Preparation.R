df %>% inspect_na()
# We have missing in 4 variables
# 1 next_pymnt_d      227214 4.87e+1
# 2 zip_class         139710 3.00e+1
# 3 last_pymnt_d         376 8.06e-2
# 4 last_credit_pull~     41 8.79e-3

#Let's whether we have correlation with
#loan status of next_payment date

df %>% 
  filter(loan_status==0) %>% 
  inspect_na() #42475 missing

df %>% 
  filter(loan_status==1) %>% 
  inspect_na() # 184739 missing

#In both cases it has missings,
#If not defaulted missings are more though
#It is quite difficult to predict Date
#We simply have to drop it

df %>% 
  select(-next_pymnt_d) -> df

chpoint_13 <- df
df <- chpoint_13
#Let's see zip_class variables
#Its mistake was found and amended
#Dont run the following code
# df %>% 
#   filter(loan_status==0) %>% 
#   inspect_na() #16379 missing
# df %>% 
#   filter(loan_status==1) %>% 
#   inspect_na() #123331 missing
# 
# #let's see original dataset
# library(data.table)
# data_raw <- fread("loan_data_2007_2014.csv",
#                   na.strings = c(NA, ""))
# 
# data_raw[, "zip_code"] %>% inspect_na()
# #Actually in original dataset we do not had 
# #Missing values for zipcode
# #Let's find our mistake
# 
# #codes from 1st script
# gsub("xx",
#      "",
#      data_raw$zip_code) -> data_raw$zip_code
# data_raw$zip_code %>% 
#   table() %>% 
#   as.data.frame() %>% 
#   arrange(Freq %>% desc()) -> zip_code
# zip_code %>% 
#   rename(name='.') -> zip_code
# 
# l <- c()
# 
# for (i in zip_code$Freq){
#   if (i<250){
#     l <- append(l, 1)
#   }
#   else if(i<500){
#     l <- append(l, 2)
#   }
#   else if(i<750){
#     l <- append(l, 3)
#   }
#   else if(i<1000){
#     l <- append(l, 4)
#   }
#   else if(i<1750){
#     l <- append(l,5)
#   }
#   else if(i<2500){
#     l <- append(l,6)
#   }else{
#     l <- append(l,7)
#   }
# }
# zip_code$class <- l
# 
# #here is our mistake
# data_raw$zip_code %in% zip_code$name %>% 
#   sum() #326564
# 
# zip_code$name %in% data_raw$zip_code %>% 
#   sum() #701
# 
# zip_code[, "name"] %>% glimpse()
# 
# zip_code$name <- zip_code$name %>% 
#   as.numeric() %>% 
#   as.character()
# 
# data_raw$zip_code <- data_raw$zip_code %>%
#   as.numeric() %>% 
#   as.character() %>% 
#   as.factor()
# zip_code$name <- zip_code$name %>% as.factor()
# 
# data_raw$zip_code %>% unique() %>% length()#888
# zip_code$name %>% unique() %>% length()#888
# 
# data_raw %>% 
#   left_join(zip_code,
#             by = c('zip_code'='name')) -> 
#   df_zip
# 
# data_raw$zip_code %in% zip_code$name %>% 
#   sum() #466285
# 
# zip_code$name %in% data_raw$zip_code %>% 
#   sum() #888

# Now it is okay

df %>% inspect_na()

#rest of NAs will be dropen
chpoint_14 <- df
df <- chpoint_14

df <- df[
  !is.na(df$last_pymnt_d),
]

df <- df[
  !is.na(df$last_credit_pull_d),
]

df %>% inspect_na() #Great

#if we look at some numberic variables
#they have zeros a lot
#rest of values are in minority
#let's see which are those variables

df %>% 
  names() -> num_names

l <- c()

for (i in num_names){
  l <- append(l,
    df[df[[i]]==0,] %>% nrow()
  )
}

cbind(num_names, l) %>% 
  as.data.frame() %>% 
  arrange(desc(l %>% as.numeric())) %>%
  filter(l %>% as.numeric()>nrow(df)*0.45) %>% #wjhich more 45%
  .[["num_names"]] -> vars_with_zeros

# num_names      l
# 1                 delinq_2yrs 465839
# 2                     pub_rec 465839
# 3          total_rec_late_fee 465839
# 4                  recoveries 465839
# 5     collection_recovery_fee 465839
# 6  collections_12_mths_ex_med 465839
# 7              acc_now_delinq 465839
# 8                tot_coll_amt 416122
# 9              inq_last_6mths 241348
# 10                  out_prncp 229572
# 11              out_prncp_inv 229572


#first 7 vars have all zeros
#we will drop them

#we will rest of variables into
#dummies of 0 if values is zero
#and 1 if value is non zero

chpoint_15 <- df
df <- chpoint_15

df %>% 
  select(-c(
    vars_with_zeros[1:7]
  )) -> df

#we also have to check whether they are not 
#numeric dummies

vars_with_zeros[8:
                  length(
                    vars_with_zeros
                  )]

# [1] "tot_coll_amt"  
# [2] "inq_last_6mths"
# [3] "out_prncp"     
# [4] "out_prncp_inv"

df$tot_coll_amt %>% unique() # 0 and 1
#it should be factor because uniques are 0 and 1
df$tot_coll_amt <- 
  df$tot_coll_amt %>% as.factor()

df$inq_last_6mths %>% unique() 
#uniques are 1, 2.5, 2, 0
#we have to see distribution
df %>% 
  group_by(inq_last_6mths) %>% 
  summarise(n())
# 1            0   241348
# 2            1   130029
# 3            2    57748
# 4            2.5  36714
#we will make this variable also factor
#because there is certain distribution
df$inq_last_6mths <- 
  df$inq_last_6mths %>% as.factor()

#next variable
df$out_prncp %>% unique() %>% length()
#it has 123766 uniques
#so change it

ifelse(
  df$out_prncp==0,
  0,
  1
) -> df$out_prncp

df$out_prncp <- 
  df$out_prncp %>% as.factor()

#last var
df$out_prncp_inv %>% unique() %>% length()
#too many uniques
#let's do same action

ifelse(
  df$out_prncp_inv==0,
  0,
  1
) -> df$out_prncp_inv

df$out_prncp_inv <- 
  df$out_prncp_inv %>% as.factor()

chpoint_16 <- df
df <- chpoint_16

#now let's visually see our data

df %>% select_if(is.numeric) %>% View() #great
df %>% select_if(is.factor) %>% View() #Dates are factors

#we are going to have dummy variables
#if we have 100 unique factor vars it means 100 columns
#So, we will have to drop them as well
#let's see their uniques

l <- c()

df %>% select_if(is.factor) %>% names()-> fact

for(i in fact){
  l <- append(l,
              df[[i]] %>% unique() %>% length())
}

cbind(fact, l) %>% 
  as.data.frame() %>% 
  arrange(desc(as.numeric(l)))

# 2   last_credit_pull_d    103
# 3         last_pymnt_d     98
# 4              issue_d     91

#if we are going to create dummy values
#each variable will add columns==number of uniques
#we could transform this data into continous
#if we had year, but we do not have
#so, for now we have to drop them

cbind(fact, l) %>% 
  as.data.frame() %>% 
  arrange(desc(as.numeric(l))) %>% 
  .[[1]] %>% 
  .[2:4] -> date_to_drop

df %>% 
  select(-c(date_to_drop)) -> df

chpoint_17 <- df
df <- chpoint_17

df %>% skimr::skim()
#we have 16 factors and 20 numeric





    












