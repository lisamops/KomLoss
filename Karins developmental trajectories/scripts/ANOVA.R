# 
# Code by Lisa Palmqvist 2021-08-05
# Run ANOVA
#

#If you have not already loaded the data run following code
source("scripts/read_data.R")

set.seed(42)

#Time trained

data_df_ALL <- data_df %>% filter(group == 2) 
mean(data_df_ALL$tot_train_time, na.rm = T)/60

data_df_animgea <- data_df %>% filter(group == 3) 
mean(data_df_animgea$tot_train_time, na.rm = T)/60

data_df_kombi <- data_df %>% filter(group == 4) 
mean(data_df_kombi$tot_train_time, na.rm = T)/60

data_df$group <- as.factor(data_df$group)

summary(aov(tot_train_time ~ group , data_df))
a1 <- aov(tot_train_time ~ group , data_df)
TukeyHSD(a1)


library(afex)

data_df$group <- as.numeric(data_df$group)
data_df <- data_df[rowSums(is.na(data_df)) != ncol(data_df),]
a1 <- aov_ez("id", "phoneme", data_df, between = "group", within = "time")

summary(a1)

library("emmeans")  # package emmeans needs to be attached for follow-up tests.

emmeans(a1,  ~ group | time, contr = "pairwise")

a2 <- aov_ez("id", "word_reading_total", data_df, between = "group", within = "time")
summary(a2)
emmeans(a2,  ~ group | time, contr = "pairwise")

a3 <- aov_ez("id", "DLS_reading_comp_sum", data_df, between = "group", within = "time")
summary(a3)
emmeans(a3,  ~ group | time, contr = "pairwise")


a4 <- aov_ez("id", "reading_total", data_df, between = "group", within = "time")
summary(a4)
emmeans(a4,  ~ group | time, contr = "pairwise")
