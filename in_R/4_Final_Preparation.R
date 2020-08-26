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
