# 
# Code by Lisa Palmqvist 2021-08-02
# read ALL app-data files that cannot be read in the other script i.e. id 219-223
# Second script
#

#source("scripts/read_animega_appdata.R")

#xlsx220 <- read.xlsx("data/appdata/ALL/220.xlsx")

#create empty df
# ALL_df <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("id", "time_played_ALL_tot"))))
# df_tot <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("id", "time_played_ALL")))) 
# id_df <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("id", "time_played_ALL"))))



app_data_df <- full_join(ALL_df, animega_df, by = "id")
app_data_df[is.na(app_data_df)] <- 0
app_data_df$time_played_animega <- app_data_df$time_played_animega/60
app_data_df <- app_data_df %>% mutate(tot_train_time = time_played_ALL_tot+time_played_animega) 

