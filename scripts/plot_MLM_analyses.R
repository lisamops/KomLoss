# 
# Code by Lisa Palmqvist 2021-11-16
# Plot MLM analyses
# 
#


#source("scripts/read_data.R")


library(ggplot2)

#Plot data for PA
ggplot(data      = mlm_data,
       aes(x     = days,
           y     = PA,
           col   = group,
           group = group))+ #to add the colours for different classes
  geom_smooth(method   = lm,
              se       = T,
              size     = 1,
              linetype = 1,
              alpha    = .2)+
  theme_minimal()+
  labs(title    = "Phonological awareness",
       subtitle = "change over time")+
  scale_color_manual(name   =" Group",
                     labels = c("Control", "ALL", "Animega-is", "Combo"),
                     values = c("blue", "red", "green", "yellow"))

#Plot data for word
ggplot(data      = mlm_data,
       aes(x     = days,
           y     = word,
           col   = group,
           group = group))+ #to add the colours for different classes
  geom_smooth(method   = lm,
              se       = T,
              size     = 1,
              linetype = 1,
              alpha    = .2)+
  theme_minimal()+
  labs(title    = "word reading over days",
       subtitle = "change in word reading over time in the different groups")+
  scale_color_manual(name   =" Group",
                     labels = c("Control", "ALL", "Animega-is", "Combo"),
                     values = c("blue", "red", "green", "yellow"))


#Plot data for DLS
ggplot(data      = mlm_data,
       aes(x     = days,
           y     = DLS,
           col   = group,
           group = group))+ #to add the colours for different classes
  geom_smooth(method   = lm,
              se       = T,
              size     = 1,
              linetype = 1,
              alpha    = .2)+
  theme_minimal()+
  labs(title    = "DLS over days",
       subtitle = "change in sentence reading over time in the different groups")+
  scale_color_manual(name   =" Group",
                     labels = c("Control", "ALL", "Animega-is", "Combo"),
                     values = c("blue", "red", "green", "yellow"))


#Plot data for letter
ggplot(data      = mlm_data,
       aes(x     = days,
           y     = letter,
           col   = group,
           group = group))+ #to add the colours for different classes
  geom_smooth(method   = lm,
              se       = T,
              size     = 1,
              linetype = 1,
              alpha    = .2)+
  theme_minimal()+
  labs(title    = "word reading over days",
       subtitle = "change in letter-sound regognition over time in the different groups")+
  scale_color_manual(name   =" Group",
                     labels = c("Control", "ALL", "Animega-is", "Combo"),
                     values = c("blue", "red", "green", "yellow"))

