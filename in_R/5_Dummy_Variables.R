library(caret)

df %>%
  select_if(is.factor) %>% 
  select(-ID)-> fact_df

fact_df %>% 
  dummyVars(~., 
            data = ., 
            sep = "_", 
            drop2nd=T)->
  dummy_df

predict(dummy_df, fact_df) -> dummy_data

dummy_data %>% 
  as.data.frame() -> dummy_frame

#we have two variables for loan status,
#although it had to be dropped
#because of drop2nd argument
#we will drop manually

dummy_frame %>% 
  select(-loan_status_0) -> dummy_frame

#also before combination we should drop
#ID columns from df

df <- 
  df %>% select(-ID)

dummy_frame %>% glimpse()
#all variables are double
#We have to convert them into factor

dummy_frame %>% 
  names() -> dummy_frame_names

for(i in dummy_frame_names){
  dummy_frame[[i]] <- 
    dummy_frame[[i]] %>% as.factor()
}

dummy_frame %>% glimpse() #Great!

cbind(dummy_frame,
      df %>% 
        select_if(is.numeric)) -> final_df

#move loan_status to the first column
final_df %>% 
  select(loan_status_1,
         everything()) -> final_df
 
chpoint_18 <- final_df
final_df <- chpoint_18  
  
  
  


