# 
# Code by Lisa Palmqvist 2021-09-21
# Run multi level model analysis on longitudinal data
# 
#


source("scripts/read_data.R")
set.seed(42)

library(lme4) # for the analysis
library(ggplot2)
library(lmerTest)# to get p-value estimations that are not part of the standard lme4 packages
library(emmeans)
library(sjstats)

mlm_data <- data_df %>% 
  select(id, group, time, letter_sum, fonem_sum, fonemsynthesis_sum, rhyme_sum, word_reading_total, DLS_reading_comp_sum, IQ, tot_train_time) %>%
  mutate(PA= fonem_sum+fonemsynthesis_sum+rhyme_sum) %>%
  select(-fonem_sum, -fonemsynthesis_sum, -rhyme_sum) 

summary(mlm_data)

mlm_data$time <- as.factor(mlm_data$time)
mlm_data$group <- factor(mlm_data$group,
                         levels = c(1,2,3,4),
                         labels = c("Control", "ALL", "Animega-is", "Combi"))

#IQ in the different groups
IQ_group_anova <- aov(IQ ~ group, mlm_data)
summary(IQ_group_anova)
emmeans(IQ_group_anova, pairwise~group, adjust = "tukey")

ggplot(mlm_data, aes(x = group, y = IQ_scale)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Group") +
  ylab("Scores") 



#total trained time in the different groups
mlm_data_no_control <- mlm_data %>% filter(group != "Control")
traintime_group_anova <- aov(tot_train_time ~ group, mlm_data_no_control)
summary(traintime_group_anova)
emmeans(traintime_group_anova, pairwise~group, adjust = "tukey")

ggplot(mlm_data_no_control, aes(x = group, y = tot_train_time)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Group") +
  ylab("Scores") 

#MULTILEVELMODEL analyses ####
model_LS <- lmer(letter_sum ~ time*group + (1|id), data = mlm_data)
anova(model_LS)
effectsize::eta_squared(model_LS, partial = T)
emmeans(model_LS, pairwise~time, adjust = "tukey")
emms_PA <-emmeans(model_LS, pairwise~group|time)
contrast(model_LS, interaction = "pairwise")


model_PA <- lmer(PA ~ time*group + (1|id), data = mlm_data)
anova(model_PA)
effectsize::eta_squared(model_PA, partial = T)
emmeans(model_PA, pairwise~time, adjust = "tukey")
emms_PA <-emmeans(model_PA, pairwise~group|time)
contrast(emms_PA, interaction = "pairwise")


model_PA_traintime <- lmer(PA ~ time*group*tot_train_time + (1|id), data = mlm_data)
anova(model_PA_traintime)

model_PA_IQ <- lmer(PA ~ time*group*IQ + (1|id), data = mlm_data)
anova(model_PA_IQ)
effectsize::eta_squared(model_PA_IQ, partial = T)
emmeans(model_PA_IQ, pairwise~time, adjust = "tukey")
emmeans(model_PA_IQ, pairwise~time|group|IQ)

emms1 <- emmeans(model_PA_IQ, ~ IQ*group | time)
con1 <- contrast(emms1, interaction = "pairwise")
pairs(con1, by = NULL)

emmip(model_PA_IQ, group*time ~ IQ, cov.reduce = range)

model_words <- lmer(word_reading_total ~ time*group + (1|id), data = mlm_data)
anova(model_words)
effectsize::eta_squared(model_words, partial = T)
emmeans(model_words, pairwise~time, adjust = "tukey")
emmeans(model_words, pairwise~group|time)

model_words_tot_train_time <- lmer(word_reading_total ~ time*group*tot_train_time + (1|id), data = mlm_data)
anova(model_words_tot_train_time)

model_words_IQ <- lmer(word_reading_total ~ time*group*IQ + (1|id), data = mlm_data)
anova(model_words_IQ)
effectsize::eta_squared(model_words_IQ, partial = T)
emmeans(model_words_IQ, pairwise~time, adjust = "tukey")
emmeans(model_words_IQ, pairwise~time|group|IQ)

## OBS! Denna model verkar inte vara linjär. Frågan är vad vi gör åt det?
#plot(resid(model_DLS))
model_DLS <- lmer(DLS_reading_comp_sum ~ time*group + (1|id), data = mlm_data)
anova(model_DLS)
effectsize::eta_squared(model_DLS, partial = T)
emmeans(model_DLS, pairwise~time, adjust = "tukey")
emmeans(model_DLS, pairwise~group|time)

model_DLS_tot_train_time <- lmer(DLS_reading_comp_sum ~ time*group*tot_train_time + (1|id), data = mlm_data)
anova(model_DLS_tot_train_time)

model_DLS_IQ <- lmer(DLS_reading_comp_sum ~ time*group*IQ + (1|id), data = mlm_data)
anova(model_DLS_IQ)

#Plot data for PA 
ggplot(data      = mlm_data,
       aes(x     = time,
           y     = PA,
           col   = group,
           group = group))+ #to add the colours for different classes
  geom_smooth(method   = loess,
              se       = T, 
              size     = 1, 
              linetype = 1, 
              alpha    = .2)+
  theme_minimal()+
  labs(title    = "Phonological awareness over time",
       subtitle = "change in phonological awareness over time in the different groups")+
  scale_color_manual(name   =" Group",
                     labels = c("Control", "ALL", "Animega-is", "Combo"),
                     values = c("blue", "red", "green", "yellow"))

#Plot data for word 
ggplot(data      = mlm_data,
       aes(x     = time,
           y     = word_reading_total,
           col   = group,
           group = group))+ #to add the colours for different classes
  geom_smooth(method   = loess,
              se       = T, 
              size     = 1, 
              linetype = 1, 
              alpha    = .2)+
  theme_minimal()+
  labs(title    = "word reading over time",
       subtitle = "change in word reading training over time in the different groups")+
  scale_color_manual(name   =" Group",
                     labels = c("Control", "ALL", "Animega-is", "Combo"),
                     values = c("blue", "red", "green", "yellow"))

#Plot data for DLS 
ggplot(data      = mlm_data,
       aes(x     = time,
           y     = DLS_reading_comp_sum,
           col   = group,
           group = group))+ #to add the colours for different classes
  geom_smooth(method   = loess,
              se       = T, 
              size     = 1, 
              linetype = 1, 
              alpha    = .2)+
  theme_minimal()+
  labs(title    = "DLS over time",
       subtitle = "change in DLS over time in the different groups")+
  scale_color_manual(name   =" Group",
                     labels = c("Control", "ALL", "Animega-is", "Combo"),
                     values = c("blue", "red", "green", "yellow"))

