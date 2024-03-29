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
  select(id, group, time, days, gender, age_in_days, age_scale, letter = letter_sum, PA, word = word_reading_total, DLS = DLS_reading_comp_sum, 
         IQ_scale, IQ, tot_train_time_scale, tot_train_time, PPC,	education_father,	occupation_mother,	occupation_father, SES, `Only ID`, Autismspektrumtillstand, `Downs syndrom`, CP, `ADHD/ADD`, Other, time_played_ALL_tot, time_played_animega) %>%
  mutate(contrast_1vs234= ifelse(group == 1, (-1),  (1/3))) %>%  #control vs ALL,animega,combi
  mutate(contrast_4vs23= ifelse(group == 4, 1, ifelse(group == 1, 0, -1/2))) %>%  #combi vs ALL,animega
  mutate(contrast_1vs2= ifelse(group == 1, (-1),  ifelse(group == 2, 1, 0))) %>%  #control vs ALL
  mutate(contrast_1vs3= ifelse(group == 1, (-1),  ifelse(group == 3, 1, 0))) #control vs animega



#mlm_data$tot_train_time_scale <- scale(mlm_data$tot_train_time, center = F)

#write.csv(mlm_data,'mlm_data.csv', na = "9999")


#mlm_data$time <- as.factor(mlm_data$time)
mlm_data$group <- factor(mlm_data$group,
                         levels = c(1,2,3,4),
                         labels = c("Control", "ALL", "Animega-is", "Combi"))


#IQ in the different groups
IQ_group_anova <- aov(IQ_scale ~ group, mlm_data)


#All outcome variables are SCALED to get standardized estimates in the model
mlm_data$PA_scale <- scale(mlm_data$PA, center = F)
mlm_data$DLS_scale <- scale(mlm_data$DLS, center = F)
mlm_data$days_scale <- scale(mlm_data$days, center = F)
mlm_data$PPC_scale <- scale(mlm_data$PPC, center = F)

# PHONOLOGICAL AWARENESS (PA) ####

#Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. PA)
#model with random intercept with ID estimating PA difference over time
#model_PA_1 <- lmer(PA_scale ~ days_scale + (1|id), data = mlm_data, REML = FALSE)

# adding random slope, no covariance structure (variance components) (specified using double-bar ||)
#model_PA_2 <- lmer(PA_scale ~ days_scale + (1+days_scale||id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
#anova(model_PA_1, model_PA_2) #Not sig better

# adding random slope, unstructured covariance using single bar |
#model_PA_2b <- lmer(PA_scale ~ scale(days, center = FALSE) + (1+days_scale|id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
#anova(model_PA_2, model_PA_2b) # - not better -2*loglikelihood thus skipping random slopes in following analyses

#conditional model (e.g. including PA)
#adding control vs ALL, control vs Animega, and combi vs ALL+animega, improves model significantly (chi2 2*loglikelihood)
#With scaled variables
model_PA_3 <- lmer(PA_scale ~ days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + days_scale*contrast_4vs23 + (1|id), data = mlm_data, REML = FALSE)
#Without scaled variables
#model_PA_3 <- lmer(PA ~ days*contrast_1vs2 + days*contrast_1vs3 + days*contrast_4vs23 + (1|id), data = mlm_data, REML = FALSE)


#adding exploratory variables
#model_PA_3_expl <- lmer(PA_scale ~ IQ_scale + age_scale + PPC_scale + days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + days_scale*contrast_4vs23 + (1|id), data = mlm_data, REML = FALSE)

#summary(model_PA_3)
#summary(model_PA_3_expl)

#anova(model_PA_2b, model_PA_3) # sig better model.

#Adding IQ - model improves a lot. IQ can explain how well the participants are developing over time 
#model_PA_4 <- lmer(PA_scale ~ days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + days_scale*contrast_4vs23 + days_scale*IQ_scale + (1|id), data = mlm_data, REML = FALSE)

#Adding age - model improves sig. age differs at t1, but does not interacts with time.
#model_PA_4 <- lmer(PA_scale ~ days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + days_scale*contrast_4vs23 + days_scale*age_scale + (1|id), data = mlm_data, REML = FALSE)
#anova(model_PA_3, model_PA_4) # sig better model

#Conclusion: Combi groups improves more than other intervention groups. IQ can explain how well the participants are developing over time 
#(higher IQ = better improvement). Thus, might underestimate the effect of the combi group due to Animega-is group having higher IQ.

# WORD ####

#Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. word)
#model with random intercept with ID estimating word difference over time
#model_word_1 <- lmer(word ~ days_scale + (1|id), data = mlm_data, REML = FALSE)

# adding random slope, no covariance structure (variance components) (specified using double-bar ||)
#model_word_2 <- lmer(word ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
#anova(model_word_1, model_word_2) #model improves sig. 

# adding random slope, unstructured covariance 
#model_word_2b <- lmer(word ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
#anova(model_word_2, model_word_2b) #model improves sig. using unstructured covariance)) i.e. single bar |

#conditional model (e.g. including word)
#adding intervention (OBS! with random slope) # no sig interaction
model_word_3 <- lmer(word ~ days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + days_scale*contrast_4vs23 + (1+days_scale|id), data = mlm_data, REML = FALSE)
#anova(model_word_2b, model_word_3) # not  sig. improved


#adding exploratory variables
#model_word_3_expl <- lmer(word ~ IQ_scale + age_scale + PPC_scale + days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + days_scale*contrast_4vs23 + (1+days_scale|id), data = mlm_data, REML = FALSE)
#model_word_3_expl <- lmer(word ~ days_scale*IQ_scale + days_scale*age_scale + days_scale*PPC_scale + days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + days_scale*contrast_4vs23 + (1+days_scale|id), data = mlm_data, REML = FALSE)

#summary(model_word_3)
#summary(model_word_3_expl)

#Adding IQ - model improves. IQ can explain how well the participants are developing over time (p = .070)
#model_word_4 <- lmer(word ~ scale(days, center = FALSE)*contrast_1vs2 + scale(days, center = FALSE)*contrast_1vs3 + scale(days, center = FALSE)*contrast_4vs23  + IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE)

#Conclusion: Intervention groups does not improve more than control. IQ can explain how well the participants are developing over time (higher IQ = better improvement)
#Conclusion: Combi groups does not improve more than other intervention groups. Tendency for IQ to explain how well the participants are developing over time 
#(higher IQ = better improvement). Thus, might underestimate the effect of the combigroup due to Animega-is group having higher IQ

# Explorativ analys: Kan PA predicera utvecklingen av letter sound, ordläsining och meningsläsning över tid? nja! ####
#model_PA_word <- lmer(word ~ scale(days, center = FALSE)*contrast_1vs2 + scale(days, center = FALSE)*contrast_1vs3 + scale(days, center = FALSE)*contrast_4vs23  + IQ_scale*scale(days, center = FALSE) + scale(days, center = FALSE)*scale(PA, center = TRUE) + (1|id), data = mlm_data, REML = FALSE)

#summary(model_PA_word)

# DLS ####

#Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. DLS)
#model with random intercept with ID estimating DLS difference over time
#model_DLS_1 <- lmer(DLS ~ days + (1|id), data = mlm_data, REML = FALSE) #normal distribution

# DLS has a strongly skewed distribution, thus a poisson distribution is used for fitting the models rather than a normal distribution
#Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. word)
#model with random intercept with ID estimating DLS difference over time
#DLS_poisson <- glmer(DLS ~ days_scale + (1|id), data = mlm_data,family=poisson) #rescaling due to convergence issues
#summary(DLS_poisson) #model is much better in terms of AIC and loglikelihood, using poisson dist. instead of normal dist. 

# adding random slope, no covariance structure (variance components) (specified using double-bar ||)
#DLS_poisson_2 <- glmer(DLS ~ days_scale + (1+days_scale||id), data = mlm_data,family=poisson)
#anova(DLS_poisson, DLS_poisson_2)  # sig.

# adding random slope, unstructured covariance specified using single bar |
#DLS_poisson_2b <- glmer(DLS ~ days_scale + (1+days_scale|id), data = mlm_data,family=poisson)

#anova(DLS_poisson_2, DLS_poisson_2b)  #not sig.i.e use double bar ||

#conditional model (e.g. including intervention)
#adding intervention (OBS! with random slope) # no sig interaction
#DLS_poisson_3 <- glmer(DLS ~ days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + 
#                         days_scale*contrast_4vs23 + (1+days_scale||id), data = mlm_data,family=poisson) #does not converge

DLS_poisson_3_no_rand_slope <- glmer(DLS ~ days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + 
                         days_scale*contrast_4vs23 + (1|id), data = mlm_data,family=poisson) #removes random slope

# 
# anova(DLS_poisson_2, DLS_poisson_3_no_rand_slope)  #not sig.
# 
# #Not sure if the model is overdispersed. 
# #Check for overdispersion ( when the observed variance is higher than the variance of a theoretical model.)
# qqnorm(resid(DLS_poisson_3_no_rand_slope)) #not straight
# hist(resid(DLS_poisson_3_no_rand_slope))
# plot(fitted(DLS_poisson_3_no_rand_slope),resid(DLS_poisson_3_no_rand_slope))
# library("blmeco") 
# dispersion_glmer(DLS_poisson_3_no_rand_slope) #it should between 0.75 and 1.4. 0.91
# 
# 
# #build a zero inflation poission model. Cannot plot with tab_model() thus use poission instead
# #Explore proportion of 0´s in the data
# data.hp.tab <- table(mlm_data$DLS == 0)
# data.hp.tab/sum(data.hp.tab)
# # proportion of 0's expected from a Poisson distribution
# mu <- mean(mlm_data$DLS, na.rm = T)
# s <- sd(mlm_data$DLS, na.rm = T)
# cnts <- rpois(1000, mu)
# data.hp.tab <- table(cnts == 0)
# data.hp.tab/sum(data.hp.tab)
# 
# #Fit zero inflated poisson distribution
# eq <- DLS ~ days_scale*contrast_1vs2 + days_scale*contrast_1vs3 + 
#                   days_scale*contrast_4vs23 + (1|id)
# library(glmmTMB)
# zipoisson1 <- glmmTMB(eq,
#                       data=mlm_data,
#                       ziformula=~1,
#                       family=poisson)
# 
# 
# 
# #Extract the IRR coefficients from the DLS model. If I want to replace in he DLS model when presenting the results in text. 
# fixed <- fixef(DLS_poisson_3_no_rand_slope)
# confnitfixed <- confint(DLS_poisson_3_no_rand_slope, parm = "beta_", method = "Wald") # Beware: The Wald method is less accurate but much, much faster.
# # The exponentiated coefficients are also known as Incidence Rate Ratios (IRR)
# IRR <- exp(cbind(fixed, confnitfixed))


#summary(DLS_poisson_3)
#summary(DLS_poisson_3_no_rand_slope)

#tab_model(DLS_poisson_3_no_rand_slope)

#Adding IQ - model improves a lot. IQ can explain how well the participants are developing over time
#DLS_poisson_4 <- glmer(DLS ~ scale(days, center = FALSE)*contrast_1vs2 + scale(days, center = FALSE)*contrast_1vs3 + scale(days, center = FALSE)*contrast_4vs23 
#                       + IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data,family=poisson) #fails to converge
#DLS_poisson_4 <- glmer(DLS ~ IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data,family=poisson) #removes intervention variables


#Conclusion: Intervention groups does NOT improves more than control. IQ can explain how well the participants are developing over time (higher IQ = better improvement)
#Conclusion: Combi groups does not improve more than other intervention groups. IQ can explain how well the participants are developing over time 
#(higher IQ = better improvement). Thus, might underestimate the effect of the combigroup due to Animega-is group having higher IQ

# Explorativ analys: Kan PA predicera utvecklingen av letter sound, ordläsining och meningsläsning över tid? nja! ####
#model_PA_DLS <- glmer(DLS ~ IQ_scale*scale(days, center = FALSE) + scale(days, center = FALSE)*scale(PA, center = TRUE) + (1|id), data = mlm_data,family=poisson) #removes intervention variables AND days as random slope due to convergence issues

# letter ####
# letter CONTROLL VS INTERVENTION ####

#Unconditional model - only includes temporal predictor and not other explanatory variables (e.g. letter)
#model with random intercept with ID estimating letter difference over time
#model_letter_1 <- lmer(letter ~ days + (1|id), data = mlm_data, REML = FALSE)

# adding random slope, no covariance structure (variance components)
#model_letter_2 <- lmer(letter ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)||id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues
#isSingular(model_letter_2) #MODEL IS SINGULAR. Often indicates an overfitted model. Won't use variance components

# adding random slope, with unsctructed covariance
#model_letter_2b <- lmer(letter ~ scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE) #scaling days due to convergence issues

#anova(model_letter_1, model_letter_2b) # sig.

#conditional model (e.g. including letter)
#adding intervention (WITH random slope) improves model significantly 
#model_letter_3 <- lmer(letter ~ scale(days, center = FALSE)*contrast_1vs2 + scale(days, center = FALSE)*contrast_1vs3 + scale(days, center = FALSE)*contrast_4vs23 + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE)
#summary(model_letter_3)
#anova(model_letter_2, model_letter_3) # sig.


#Adding IQ - OBS! IQ does not interact with days. Only interaction between days and intervention! 
#model_letter_4 <- lmer(letter ~ scale(days, center = FALSE)*contrast_1vs2 + scale(days, center = FALSE)*contrast_1vs3 + scale(days, center = FALSE)*contrast_4vs23 + IQ_scale*scale(days, center = FALSE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE)
#summary(model_letter_4)

#Conclusion: Intervention groups improves more than control. IQ CANNOT explain how well the participants are developing over time 

#Conclusion: intervention groups better than control. Combi groups DOES NOT improve more than other intervention groups. IQ can explain how well the participants are developing over time 
#(higher IQ = better improvement). Thus, might underestimate the effect of the combigroup due to Animega-is group having higher IQ
#Conclusion ALL better on letter recognition!

# Explorativ analys: Kan PA predicera utvecklingen av letter sound, ordläsining och meningsläsning över tid? nja! ####
#model_PA_letter <- lmer(letter ~ scale(days, center = FALSE)*contrast_1vs2 + scale(days, center = FALSE)*contrast_1vs3 + scale(days, center = FALSE)*contrast_4vs23 + IQ_scale*scale(days, center = FALSE) + scale(days, center = FALSE)*scale(PA, center = TRUE) + (1+scale(days, center = FALSE)|id), data = mlm_data, REML = FALSE)

#summary(model_PA_letter)


#PLOT mlm model
# effects_PA <- effects::effect(term= "days*contrast_4vs23", mod= model_PA_3)
# # Save the effects values as a df:
# x_PA <- as.data.frame(effects_PA)
# ggplot() + 
#   geom_point(data=mlm_data, aes(days*contrast_4vs23, log(days)), color="orange") + 
#   geom_point(data=x_PA, aes(x=days*contrast_4vs23, y=fit), color="green") +
#   geom_line(data=x_PA, aes(x=days*contrast_4vs23, y=fit), color="blue") +
#   geom_ribbon(data= x_PA, aes(x=days*contrast_4vs23, ymin=lower, ymax=upper), alpha= 0.3, fill="red") +
#   labs(x="Days", y="Phonological Awareness")



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



