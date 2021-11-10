# 
# Code by Lisa Palmqvist 2021-08-05
# Run SEM analysis
# 
#
# https://github.com/doomlab/learnSEM
#vignette("lecture_data_screen", "learnSEM")
#förslag på tidskift: Learning Disability Quarterly

source("scripts/read_data.R")
set.seed(42)

library(lavaan)
library(dplyr)
library(rio)
library(lme4)
library(semPlot)
library(mice)
library(nonnest2)

#filterar ut t1 ####
data_df_t1 <- data_df %>% filter(time == 1) 

mean(myData1$age_in_days)/365

#data cleaning a la Dr Buchanan
#change character to factor
# myData <- myData1 %>% select(id, letter_sum, fonem_sum, fonemsynthesis_sum, rhyme_sum, OS64_sum, OLAF_sum,   
#                                       raven, ID_level, education_mother,	education_father,	occupation_mother,	occupation_father, 
#                                       decode_short_words, decode_complex_words)

myData <- myData1 %>% select(id, letter_sum, fonem_sum, fonemsynthesis_sum, OS64_sum, OLAF_sum,   
                             raven, education_mother, education_father,	occupation_mother, occupation_father, 
                             decode_short_words)

myData$SES <- myData$education_mother + myData$education_father +	myData$occupation_mother + myData$occupation_father


myData <- myData1 %>% select(id, letter_sum, fonem_sum, fonemsynthesis_sum, OS64_sum, OLAF_sum,   
                             raven, education_mother, decode_short_words)
#summary(myData)

#### Mice imputation
tempData <- mice(myData,m=1,maxit=50,meth='pmm',seed=500)
no_missing <- complete(tempData)


#Find outliers
#check correlations with and witout outliers to see if it changes the correlation patterns. 
mahal <- mahalanobis(no_missing[ , -c(1,9, 10, 11, 12, 13)], #take note here removes variables ID and ordinal data
                     colMeans(no_missing[ , -c(1,9, 10, 11, 12, 13)], na.rm=TRUE),
                     cov(no_missing[ , -c(1,9, 10, 11, 12, 13)], use ="pairwise.complete.obs"), tol=1e-20)

cutoff <- qchisq(p = 1 - .001, #1 minus alpha
                 df = ncol(no_missing[ , -c(1,9,10, 11, 12, 13)])) # number of columns
no_outliers <- subset(no_missing, mahal < cutoff) #check which ones get removed (often 327 & 329)

# #additivity
# library(corrplot)
# #correlations phoneme latent variable
# corrplot(cor(no_missing[ , c(2, 3, 4, 5)]), method = "number")
# #correlations word reading latent variable
# corrplot(cor(no_missing[ , c(6, 7, 14, 15)]), method = "number")
# #correlations developmental level latent variable
# corrplot(cor(no_missing[ , c(8, 9)]), method = "number")
# #correlations SES latent variable
# corrplot(cor(no_missing[ , c(10,12)]), method = "number")
# library("PerformanceAnalytics")
# chart.3orrelation(no_outliers[ , c(2, 3, 4, 5)], histogram=TRUE, pch=19)
# 
# corrplot(cor(no_missing[ , -c(1)]), method = "number")


Read.model1 <- '
# measurement model
read  =~ OS64_sum + OLAF_sum + decode_short_words    
phonology =~ letter_sum + fonem_sum + fonemsynthesis_sum
# regressions
read  ~ phonology 
'
fit1 <- sem(Read.model1, data=no_missing)
summary(fit1, fit.measures = TRUE, standardized=TRUE, rsquare = TRUE)
semPaths(fit1, what = "std")
#modificationindices(fit1, sort = T)

Read.model2 <- '
# measurement model
read  =~ OS64_sum + OLAF_sum + decode_short_words    
phonology =~ letter_sum + fonem_sum + fonemsynthesis_sum
# regressions
phonology ~ raven
read  ~  phonology + raven
'
fit2 <- sem(Read.model2, data=no_missing)
summary(fit2, fit.measures = TRUE, standardized=TRUE, rsquare = TRUE)
semPaths(fit2, what = "std")
#modificationindices(fit2, sort = T)

Read.model3 <- '
# measurement model
read  =~ OS64_sum + OLAF_sum + decode_short_words    
phonology =~ letter_sum + fonem_sum + fonemsynthesis_sum
# regressions
phonology ~ raven + education_mother
read  ~  phonology + raven + education_mother
'
fit3 <- sem(Read.model3, data=no_missing)
summary(fit3, fit.measures = TRUE, standardized=TRUE, rsquare = TRUE)
semPaths(fit3, what = "std", nCharNodes = 0, theme = "colorblind", 
         filetype = "pdf", width = 8, height = 6, filename = "final SEM model" #  Save to PDF
) 

semPaths(fit3, what = "std")

vuongtest(fit1, fit2) #significant difference, fit2 better than fit1
vuongtest(fit2, fit3) # not a significant difference, we make no assumption about either of the models being the true model.
vuongtest(fit1, fit3) #significant difference, fit3 better than fit1

# We can make this nicer. First let's define the node labels:
# nodeNames <- c(
#   "Letter sum",
#   "Phoneme sum",
#   "Phonemsynthesis",
#   "Developmental level",
#   "Mother education",
#   "OS 64",
#   "OLAF",
#   "Decode simple words ")

# # Now we can plot:
# semPaths(fit3,
#          what = "std", # this argument controls what the color of edges represent. In this case, standardized parameters
#       #   whatLabels = "est", # This argument controls what the edge labels represent. In this case, parameter estimates
#       #   style = "lisrel", # This will plot residuals as arrows, closer to what we use in class
#       #   residScale = 8, # This makes the residuals larger
#          theme = "colorblind", # qgraph colorblind friendly theme
#          nCharNodes = 0, # Setting this to 0 disables abbreviation of nodes
#         # manifests = paste0("Q",1:10), # Names of manifests, to order them appropriatly.
#          reorder = T, # This disables the default reordering
#          nodeNames = nodeNames, # Add a legend with node names
#          legend.cex = 5, # Makes the legend smaller
#       #   rotation = 2, # Rotates the plot
#          layout = "tree2", # tree layout options are "tree", "tree2", and "tree3"
#       #   cardinal = "lat cov", # This makes the latent covariances connet at a cardinal center point
#        #  curvePivot = TRUE, # Changes curve into rounded straight lines
#          sizeMan = 8, # Size of manifest variables
#          sizeLat = 12, # Size of latent variables
#      #    mar = c(2,5,2,5.5) # Figure margins
#         filetype = "pdf", width = 8, height = 6, filename = "SEM" #  Save to PDF
# )

