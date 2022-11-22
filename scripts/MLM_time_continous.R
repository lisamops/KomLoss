  # 
  # Code by Lisa Palmqvist 2021-09-21
  # Run multi level model analysis on longitudinal data
  # 
  #
  source("scripts/read_data.R")
  set.seed(42)
  
  library(dplyr)
  library(lme4) # for the analysis
  library(ggplot2)
  library(lmerTest)# to get p-value estimations that are not part of the standard lme4 packages
  library(emmeans)
  library(sjstats)
  
  #options(scipen=999)
  
  mlm_data <- data_df %>% 
    select(id, group, time, days, gender, letter = letter_sum, fonem_sum, fonemsynthesis_sum, rhyme_sum, word = word_reading_total, DLS = DLS_reading_comp_sum, IQ_scale, IQ, tot_train_time_scale, tot_train_time, age_in_days) %>%
    mutate(PA= fonem_sum+fonemsynthesis_sum+rhyme_sum) %>%
    select(-fonem_sum, -fonemsynthesis_sum, -rhyme_sum) %>% 
    mutate(group_ALL= ifelse(group == 2, 1, 0)) %>% 
    mutate(group_animega= ifelse(group == 3, 1, 0)) %>% 
    mutate(group_combi= ifelse(group == 4, 1, 0)) %>% 
    mutate(intervention_y_n= ifelse(group == 1, 0, 1)) 
  
  mlm_data$centred <- scale(mlm_data$days, center = TRUE)
  mlm_data$days_not_centred <- scale(mlm_data$days, center = FALSE)
  
  #write.csv(mlm_data,'mlm_data.csv', na = "9999")
  
  
  #mlm_data$time <- as.factor(mlm_data$time)
  mlm_data$group <- as.factor(mlm_data$group)
  mlm_data$group <- factor(mlm_data$group,
                           levels = c(1,2,3,4),
                           labels = c("Control", "ALL", "Animega-is", "Combi"))
  
  # Comparing combi with other intervention groups
  mlm_data_combi <- mlm_data %>% filter(group != "Control") #exclude control group
  
  #IQ in the different groups
  IQ_group_anova <- aov(IQ_scale ~ group, mlm_data)

  # PHONOLOGICAL AWARENESS (PA) ####
  # PA CONTROLL VS INTERVENTION ####
  
  #Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. PA)
  #model with random intercept with ID estimating PA difference over time
  model_PA_1 <- lmer(PA ~ days + (1|id), data = mlm_data, REML = FALSE)
  
  # adding random slope, no covariance structure (variance components)
  model_PA_2 <- lmer(PA ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
  #anova(model_PA_1, model_PA_2) #Not sig better
  
  # adding random slope, unstructured covariance 
  model_PA_2b <- lmer(PA ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
  #anova(model_PA_2, model_PA_2b) # - not better -2*loglikelihood thus skipping random slopes in following analyses
  
  #conditional model (e.g. including PA)
  #adding intervention (no random slope) improves model significantly (chi2 2*loglikelihood)
  model_PA_3 <- lmer(PA ~ days*intervention_y_n + (1|id), data = mlm_data, REML = FALSE)
  #anova(model_PA_1, model_PA_3) #sig better model.
  
  summary(lm(write ~ race.f, hsb2))
  summary(lmer(PA ~ days*group + (1|id), data = mlm_data, REML = FALSE))

  
  
  summary(model_PA_3)
  
  #Adding IQ - model improves a lot. IQ can explain how well the participants are developing over time #Kan Emil testa detta sig. i excell?
  model_PA_4 <- lmer(PA ~ days*intervention_y_n + IQ_scale*days + (1|id), data = mlm_data, REML = FALSE)

  #Conclusion: Intervention groups improves more than control. IQ can explain how well the participants are developing over time (higher IQ = better improvement)
  
  # PA COMBI VS OTER INTERVENTION ####
  
  #Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. PA)
  #model with random intercept with ID estimating PA difference over time
  model_PA_5 <- lmer(PA ~ days + (1|id), data = mlm_data_combi, REML = FALSE)
  
  # adding random slope - not better -2*loglikelihood thus skipping random slopes in following analyses
  #no covariance structure (variance components)
  model_PA_6 <- lmer(PA ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data_combi, REML = FALSE) #scaling days due to convergence issues
  #anova(model_PA_5, model_PA_6) #not sig. better.
  
  # adding random slope - not better -2*loglikelihood thus skipping random slopes in following analyses
  #unstructured covariance
  model_PA_6b <- lmer(PA ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data_combi, REML = FALSE) #scaling days due to convergence issues
  #anova(model_PA_5, model_PA_6b) #not sig. better.
  
  
  #conditional model (e.g. including PA)
  #adding combi group (no random slope) 
  model_PA_7 <- lmer(PA ~ days*group_combi + (1|id), data = mlm_data_combi, REML = FALSE)
  
  #anova(model_PA_5, model_PA_7) #sig better model.

  #adding IQ
  model_PA_8 <- lmer(PA ~ days*group_combi + IQ_scale*days + (1|id), data = mlm_data_combi, REML = FALSE)

  AIC(model_PA_7)
  AIC(model_PA_8) #AIC improves KAN EMIL TESTA DETTA MED CHI2 TEST?
  
  #Conclusion: Combi groups improves more than other intervention groups. IQ can explain how well the participants are developing over time 
  #(higher IQ = better improvement). Thus, might underestimate the effect of the combigroup due to Animega-is group having higher IQ.
  
  # WORD ####
  # WORD CONTROLL VS INTERVENTION ####
  
  #Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. word)
  #model with random intercept with ID estimating word difference over time
  model_word_1 <- lmer(word ~ days + (1|id), data = mlm_data, REML = FALSE)
  
  # adding random slope, no covariance structure (variance components)
  model_word_2 <- lmer(word ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
  #anova(model_word_1, model_word_2) #model improves sig. 
  
  # adding random slope, unstructured covariance 
  model_word_2b <- lmer(word ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
  #anova(model_word_2, model_word_2b) #model improves sig. using unstructured covariance))
  
  #conditional model (e.g. including word)
  #adding intervention (OBS! with random slope) # no sig interaction
  model_word_3 <- lmer(word ~ scale(days, center = FALSE)*intervention_y_n + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE)
  #anova(model_word_2b, model_word_3) # not  sig. improved
  
  
  summary(lmer(DLS ~ days*group + (1|id), data = mlm_data, REML = FALSE))
  
  #Adding IQ - model improves. IQ can explain how well the participants are developing over time (p = .070)
  model_word_4 <- lmer(word ~ scale(days, center = FALSE) + IQ_scale*days + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE)

  
  AIC(model_word_3) #867
  AIC(model_word_4) #838
  
  #Conclusion: Intervention groups does not improve more than control. IQ can explain how well the participants are developing over time (higher IQ = better improvement)
  
  # WORD COMBI VS OTER INTERVENTION ####
  
  #Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. word)
  #model with random intercept with ID estimating word difference over time
  model_word_5 <- lmer(word ~ days + (1|id), data = mlm_data_combi, REML = FALSE)
  
  # adding random slope, no covariance structure (variance components)
  model_word_6 <- lmer(word ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data_combi, REML = FALSE) #scaling days due to convergence issues
  
  # adding random slope, unstructured covariance 
  model_word_6b <- lmer(word ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data_combi, REML = FALSE) #scaling days due to convergence issues
  
  #anova(model_word_6, model_word_6b) #does not improve sig (p = .097) using no covariance strucutre
  
  #conditional model (e.g. including word)
  #adding combi group (random slope) #ingen sig interaktion
  model_word_7 <- lmer(word ~ scale(days, center = FALSE)*group_combi + (1+scale(days, center = FALSE)||id), data = mlm_data_combi, REML = FALSE)

  #anova(model_word_6, model_word_7) #not improves sig. 
  
  #adding IQ
  model_word_8 <- lmer(word ~ scale(days, center = FALSE)*group_combi + IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data_combi, REML = FALSE)

  AIC(model_word_7) #678
  AIC(model_word_8) #675
  
  #Conclusion: Combi groups does not improve more than other intervention groups. Tendency for IQ to explain how well the participants are developing over time 
  #(higher IQ = better improvement). Thus, might underestimate the effect of the combigroup due to Animega-is group having higher IQ
  
  # DLS ####
  # DLS CONTROLL VS INTERVENTION ####
  
  #Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. DLS)
  #model with random intercept with ID estimating DLS difference over time
  model_DLS_1 <- lmer(DLS ~ days + (1|id), data = mlm_data, REML = FALSE)

  
  # DLS has a strongly skewed distribution, thus a poisson distrobution is used for fitting the models rather than a normal distribution
    DLS_poisson <- glmer(DLS ~ scale(days, center = FALSE) + (1|id), data = mlm_data,family=poisson)
  #summary(DLS_poisson) #model is much beter in terms of AIC and loglikelihood, using poisson dist istället.

  DLS_poisson_2 <- glmer(DLS ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data,family=poisson)

  DLS_poisson_2b <- glmer(DLS ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data,family=poisson)

  #anova(DLS_poisson_2, DLS_poisson_2b)  #not sig.
  
  DLS_poisson_3 <- glmer(DLS ~ scale(days, center = FALSE)*intervention_y_n + (1+scale(days, center = FALSE)||id), data = mlm_data,family=poisson)

  #anova(DLS_poisson_2, DLS_poisson_3)  #not sig.
  
  #Adding IQ - model improves a lot. IQ can explain how well the participants are developing over time
  #Does not include intervention as it did not improve model
  
  DLS_poisson_4 <- glmer(DLS ~ scale(days, center = FALSE) + IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data,family=poisson)

  AIC(DLS_poisson_3) #1046
  AIC(DLS_poisson_4) #1008
  
  #Conclusion: Intervention groups does NOT improves more than control. IQ can explain how well the participants are developing over time (higher IQ = better improvement)
  
  # DLS COMBI VS OTER INTERVENTION ####
  
  #Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. DLS)
  #model with random intercept with ID estimating DLS difference over time
   DLS_poisson_5 <- glmer(DLS ~ scale(days, center = FALSE) + (1|id), data = mlm_data_combi,family=poisson)

  # adding random slope, no covariance structure (variance components)
  DLS_poisson_6 <- glmer(DLS ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data_combi,family=poisson)
  #anova(DLS_poisson_5, DLS_poisson_6) #improves sig.
  
  # adding random slope, with unsctructed covariance
  DLS_poisson_6b <- glmer(DLS ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data_combi,family=poisson)
  #anova(DLS_poisson_6, DLS_poisson_6b)  #not sig.
  
  
  #conditional model (e.g. including DLS)
  #adding combi group (WITH random slope, no covariance structure) 
  #OBS! estimaten negativa... kan ha med Animega-is gruppens IQ att göra. 
  DLS_poisson_7 <- glmer(DLS ~ scale(days, center = FALSE)*group_combi + (1+scale(days, center = FALSE)||id), data = mlm_data_combi,family=poisson)

  #anova(DLS_poisson_6, DLS_poisson_7) #not sig.
  
  #adding IQ removes intervention (combigroup)
  DLS_poisson_8 <- glmer(DLS ~ IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data_combi,family=poisson)

  AIC(DLS_poisson_6) #1043
  AIC(DLS_poisson_8) #1008
  
  
  #Conclusion: Combi groups does not improve more than other intervention groups. IQ can explain how well the participants are developing over time 
  #(higher IQ = better improvement). Thus, might underestimate the effect of the combigroup due to Animega-is group having higher IQ
  
  # letter ####
  # letter CONTROLL VS INTERVENTION ####
  
  #Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. letter)
  #model with random intercept with ID estimating letter difference over time
  model_letter_1 <- lmer(letter ~ days + (1|id), data = mlm_data, REML = FALSE)

  # adding random slope, no covariance structure (variance components)
  model_letter_2 <- lmer(letter ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
  #isSingular(model_letter_2) #MODEL IS SINGULAR. Often indicates an overfitted model. Won't use variance components
  
  # adding random slope, with unsctructed covariance
  model_letter_2b <- lmer(letter ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues

  #anova(model_letter_1, model_letter_2b) # sig.
  
  #conditional model (e.g. including letter)
  #adding intervention (WITH random slope) improves model significantly 
  model_letter_3 <- lmer(letter ~ scale(days, center = FALSE)*intervention_y_n + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE)

  #anova(model_letter_2, model_letter_3) # sig.
  
  
  #Adding IQ - OBS! IQ does not interact with days. Only interaction between days and intervention! 
  model_letter_4 <- lmer(letter ~ scale(days, center = FALSE)*intervention_y_n + IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE)

  AIC(model_letter_3) #1877
  AIC(model_letter_4) #1831
  
  #Conclusion: Intervention groups improves more than control. IQ CANNOT explain how well the participants are developing over time 
  
  # letter COMBI VS OTER INTERVENTION ####
  
  #Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. letter)
  #model with random intercept with ID estimating letter difference over time
  model_letter_5 <- lmer(letter ~ days + (1|id), data = mlm_data_combi, REML = FALSE)

  # adding random slope, no covariance structure (variance components)
  model_letter_6 <- lmer(letter ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data_combi, REML = FALSE) #scaling days due to convergence issues
  #isSingular(model_letter_6) #MODEL IS SINGULAR. Often indicates an overfitted model. Won't use variance components
  
  # adding random slope, with unsctructed covariance
  model_letter_6 <- lmer(letter ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data_combi, REML = FALSE) #scaling days due to convergence issues

  #anova(model_letter_5, model_letter_6) #Sig.
  
  #conditional model (e.g. including letter)
  #adding combi group (WITH random slope) #INGEN INTERAKTION!!!
  model_letter_7 <- lmer(letter ~ scale(days, center = FALSE)*group_combi + (1+scale(days, center = FALSE)|id), data = mlm_data_combi, REML = FALSE)

  #anova(model_letter_6, model_letter_7) # not sig. 
  
  #adding IQ #IQ PÅVERKAR MEN FORTFARANDE INGEN INTERAKTION MELLAN GRUPPERNA 
  model_letter_8 <- lmer(letter ~ scale(days, center = FALSE)*group_combi + IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data_combi, REML = FALSE)
  AIC(model_letter_6) #1486
  AIC(model_letter_8) #1490
  
  
  #Conclusion: intervention groups better than control. Combi groups DOES NOT improve more than other intervention groups. IQ can explain how well the participants are developing over time 
  #(higher IQ = better improvement). Thus, might underestimate the effect of the combigroup due to Animega-is group having higher IQ
  
  #ALL better than other on letter recognition? ####
  
  
  #conditional model (e.g. including letter)
  #adding ALL group (with random slope) #SIG INTERAKTION???
  model_letter_9 <- lmer(letter ~ scale(days, center = FALSE)*group_ALL + (1+scale(days, center = FALSE)|id), data = mlm_data_combi, REML = FALSE)

  #anova(model_letter_6, model_letter_9) #  sig
  
  #Conclusion ALL better on letter recognition!
  
  #Animega-is better than other on letter recognition? ####
  
  #conditional model (e.g. including letter)
  #adding animega-is group (with random slope) # NO SIG INTERAKTION!!!
  model_letter_10 <- lmer(letter ~ scale(days, center = FALSE)*group_animega + (1+scale(days, center = FALSE)|id), data = mlm_data_combi, REML = FALSE)

  #anova(model_letter_6, model_letter_10) #  not sig. 
  
  #Conclusion Animega-is not better on letter recognition!
  
  # Explorativ analys: Kan PA predicera utvecklingen av letter sound, ordläsining och meningsläsning över tid? nja! ####
  model_PA_letter <- lmer(letter ~ intervention_y_n*scale(PA, center = TRUE)*days + (1|id), data = mlm_data, REML = FALSE)
summary(model_PA_letter)
  model_PA_word <- lmer(word ~ intervention_y_n*scale(PA, center = TRUE)*days + (1|id), data = mlm_data, REML = FALSE)
  summary(model_PA_word)
  model_PA_DLS <- lmer(DLS ~ intervention_y_n*scale(PA, center = TRUE)*days + (1|id), data = mlm_data, REML = FALSE)
  summary(model_PA_DLS)
  
  # #Checking assumptions
  # require("lattice")
  # 
  # plot(resid(model_PA_3)) #linearity
  # plot(model_PA_3) #homogeneity of variance
  # qqmath(model_PA_3, id=0.05) #normally distributed residuals
  # 
  # plot(resid(model_PA_4))#linearity
  # plot(model_PA_4)#homogeneity of variance
  # qqmath(model_PA_4, id=0.05) #normally distributed residuals
  # 
  # plot(resid(model_word_3))#linearity
  # plot(model_word_3) #homogeneity of variance #Ev problematic
  # qqmath(model_word_3, id=0.05) #normally distributed residuals
  # 
  # plot(resid(model_word_4))#linearity
  # plot(model_word_4) #homogeneity of variance #Ev problematic
  # qqmath(model_word_4, id=0.05) #normally distributed residuals
  # 
  # plot(resid(model_DLS_3)) #linearity #PROBLEMATIC
  # plot(model_DLS_3) #homogeneity of variance #PROBLEMATIC
  # qqmath(model_DLS_3, id=0.05) #normally distributed residuals #PROBLEMATIC
  # 
  # plot(resid(model_DLS_4))#linearity  #PROBLEMATIC
  # plot(model_DLS_4)#homogeneity of variance #PROBLEMATIC
  # qqmath(model_DLS_4, id=0.05) #normally distributed residuals #PROBLEMATIC
  
  
  # 
  # #model_PA_IQ <- lmer(PA ~ days*group_ALL*IQ + days*group_animega*IQ + days*group_combi*IQ + (1|id), data = mlm_data)
  # 
  # #total trained time in the different groups
  # mlm_data_no_control <- mlm_data %>% filter(group != "Control")
  # traintime_group_anova <- aov(tot_train_time ~ group, mlm_data_no_control)
  # summary(traintime_group_anova)
  # emmeans(traintime_group_anova, pairwise~group, adjust = "tukey")
  # 
  # ggplot(mlm_data_no_control, aes(x = group, y = tot_train_time)) +
  #   geom_boxplot(fill = "grey80", colour = "blue") +
  #   scale_x_discrete() + xlab("Group") +
  #   ylab("Scores") 
  # 
  # 
  # 
  # library(ggplot2)
  # 
  #  
  # 
  # #Plot data for PA
  # ggplot(data      = mlm_data,
  #        aes(x     = days,
  #            y     = PA,
  #            col   = group,
  #            group = group))+ #to add the colours for different classes
  #   geom_smooth(method   = lm,
  #               se       = T,
  #               size     = 1,
  #               linetype = 1,
  #               alpha    = .2)+
  #   theme_minimal()+
  #   labs(title    = "Phonological awareness",
  #        subtitle = "change over time")+
  #   scale_color_manual(name   =" Group",
  #                      labels = c("Control", "ALL", "Animega-is", "Combo"),
  #                      values = c("blue", "red", "green", "yellow"))
  # 
  # #Plot data for DLS
  # ggplot(data      = mlm_data,
  #        aes(x     = days,
  #            y     = DLS,
  #            col   = group,
  #            group = group))+ #to add the colours for different classes
  #   geom_smooth(method   = lm,
  #               se       = T,
  #               size     = 1,
  #               linetype = 1,
  #               alpha    = .2)+
  #   theme_minimal()+
  #   labs(title    = "DLS over days",
  #        subtitle = "change in sentence readin over time in the different groups")+
  #   scale_color_manual(name   =" Group",
  #                      labels = c("Control", "ALL", "Animega-is", "Combo"),
  #                      values = c("blue", "red", "green", "yellow"))
  # 
  # 
  # #Plot data for word
  # ggplot(data      = mlm_data,
  #        aes(x     = days,
  #            y     = word,
  #            col   = group,
  #            group = group))+ #to add the colours for different classes
  #   geom_smooth(method   = lm,
  #               se       = T,
  #               size     = 1,
  #               linetype = 1,
  #               alpha    = .2)+
  #   theme_minimal()+
  #   labs(title    = "word reading over days",
  #        subtitle = "change in word reading training over time in the different groups")+
  #   scale_color_manual(name   =" Group",
  #                      labels = c("Control", "ALL", "Animega-is", "Combo"),
  #                      values = c("blue", "red", "green", "yellow"))
  # 
  # 
  # #Plot data for word
  # ggplot(data      = mlm_data,
  #        aes(x     = days,
  #            y     = letter,
  #            col   = group,
  #            group = group))+ #to add the colours for different classes
  #   geom_smooth(method   = lm,
  #               se       = T,
  #               size     = 1,
  #               linetype = 1,
  #               alpha    = .2)+
  #   theme_minimal()+
  #   labs(title    = "word reading over days",
  #        subtitle = "change in word reading training over time in the different groups")+
  #   scale_color_manual(name   =" Group",
  #                      labels = c("Control", "ALL", "Animega-is", "Combo"),
  #                      values = c("blue", "red", "green", "yellow"))

  