# 
# Code by Lisa Palmqvist 2021-09-29
# Replicate Karin's paper IV - developmental trajectories
#

#If you have not already loaded the data run following code
source("scripts/read_data.R")

set.seed(42)

library(ggplot2)
library(ggpubr)

#filterar ut t1 ####
data_df_t1 <- data_df %>% filter(time == 1) 
#Remove id 403 as the testing was not done correctly
myData1 <- data_df_t1 %>% filter(id != 403)

## plot of PA - IQ
ggplot(myData1, aes(x=IQ, y=phoneme)) + 
  geom_smooth(method = "loess") + 
  scale_color_manual(values = c("red", "blue"), labels = c("Decoding", "Reading comprehension")) + 
#  geom_point(aes(shape=variable)) + 
  scale_shape_manual(values = c(5, 16), labels = c("Decoding", "Reading comprehension")) + 
  guides(color = guide_legend("Variable"), shape = guide_legend("Variable")) + 
  stat_cor(label.x = 130) +
  stat_regline_equation(label.x = 75, aes(label = ..rr.label..)) + labs(x="IQ")


## plot of reading comprehension - IQ
ggplot(myData1, aes(x=IQ, y=DLS_reading_comp_sum)) + 
  geom_smooth(method = "loess") + 
  scale_color_manual(values = c("red", "blue"), labels = c("Decoding", "Reading comprehension")) + 
  #  geom_point(aes(shape=variable)) + 
  scale_shape_manual(values = c(5, 16), labels = c("Decoding", "Reading comprehension")) + 
  guides(color = guide_legend("Variable"), shape = guide_legend("Variable")) + 
  stat_cor(label.x = 130) +
  stat_regline_equation(label.x = 75, aes(label = ..rr.label..)) + labs(x="IQ")

