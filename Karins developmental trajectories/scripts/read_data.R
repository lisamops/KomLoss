# 
# Code by Lisa Palmqvist 2021-08-05
# read cognitive data files
# Third script
#

library(rio)
library(dplyr)

#uncomment to run script to read app data
#source("scripts/read_ALL_appdata.R")

# OR load Environment instead of running code to save time
load('app_data.RData')

#Copied form Karin.
## 999 are replaced with NA Use for mine as well? 
# imported_df <- imported_df %>%
#   mutate_all(list(~na_if(., "999")))
#is this taking the means of all mice vaules? tror inte det. men vad gör de då här?
# complete_data_list <- vector(mode = "list", length = number_of_datasets)
# for (i in 1:number_of_datasets) {
#   complete_data_list[[i]] <- complete(imputed_datsets, i)
# }


#Data_inmatning
data_inmatning <- import("data/data_inmatning.xlsx")
data_df <- data_inmatning %>% select(id = ID, group, time, test_leader, letter_sum, fonem_sum, fonemsynthesis_sum, 
                                            rhyme_sum, OS64_sum, OLAF_sum, word_reading_total, word_rep_sum, sentence_rep_sum,	
                                            rep_total, DLS_reading_comp_sum, DLS_listening_comp_sum, reading_total)

data_df$letter_sum <- as.numeric(data_df$letter_sum)
data_df$fonem_sum <- as.numeric(data_df$fonem_sum)
data_df$fonemsynthesis_sum <- as.numeric(data_df$fonemsynthesis_sum)
data_df$rhyme_sum <- as.numeric(data_df$rhyme_sum)
data_df$OS64_sum <- as.numeric(data_df$OS64_sum)
data_df$OLAF_sum <- as.numeric(data_df$OLAF_sum)
data_df$word_reading_total <- as.numeric(data_df$word_reading_total)
data_df$word_rep_sum <- as.numeric(data_df$word_rep_sum)
data_df$sentence_rep_sum <- as.numeric(data_df$sentence_rep_sum)
data_df$rep_total <- as.numeric(data_df$rep_total)
data_df$DLS_reading_comp_sum <- as.numeric(data_df$DLS_reading_comp_sum)
data_df$DLS_listening_comp_sum <- as.numeric(data_df$DLS_listening_comp_sum)
data_df$reading_total <- as.numeric(data_df$reading_total)

#Read backgrond data and Raven
background_raven <- import("data/bakgrundsdata_raven.xlsx")
background_raven <- background_raven %>% select(id = ID, group, gender, date_of_birth, date_t1, IQ = standard_score)
background_raven <- background_raven %>% mutate(age_in_days = date_t1-date_of_birth)
background_raven$IQ <- as.numeric(background_raven$IQ)

data_df <- full_join(data_df, background_raven, by = c("id", "group"))
data_df <- full_join(data_df, app_data_df, by = c("id"))

#Parent survey
parent <- import("data/parent_survey.xlsx")
parent <- parent %>% select(id = "Elevens kod:", education_mother = "education_mother_or_other",	education_father,	occupation_mother,	occupation_father, SES)
parent$education_mother <- as.numeric(parent$education_mother)
parent$education_father <- as.numeric(parent$education_father)
parent$occupation_mother <- as.numeric(parent$occupation_mother)
parent$occupation_father <- as.numeric(parent$occupation_father)
data_df <- full_join(data_df, parent, by = c("id"))

data_df <- data_df %>% 
  #Set trained time and days played to 0 in control group (group 1)
  mutate(time_played_ALL_tot= ifelse(group == 1, 0, time_played_ALL_tot)) %>% 
  mutate(days_played_ALL= ifelse(group == 1, 0, days_played_ALL))%>% 
  mutate(time_played_animega= ifelse(group == 1, 0, time_played_animega))%>% 
  mutate(days_played_animega= ifelse(group == 1, 0, days_played_animega))%>% 
  mutate(tot_train_time= ifelse(group == 1, 0, tot_train_time))%>% 
  # create new phoneme variable
  mutate(phoneme= fonem_sum+fonemsynthesis_sum+rhyme_sum)

#Read student survey
student_survey <- import("data/student_survey.xlsx")
student_survey <- student_survey %>% select(id = "student_code", before1_after2_intervention, synthesis,	segmentation,	
                                            sound_letter_connection,	understand_rhyme, decode_short_words, decode_complex_words, sight_word_reading)

student_survey <- student_survey %>% filter(before1_after2_intervention == 1)
student_survey <- student_survey %>% select(-before1_after2_intervention)
data_df <- full_join(data_df, student_survey, by = c("id"))

# teacher_rating <- data_df %>% select(id, synthesis,	segmentation,	sound_letter_connection,	understand_rhyme, decode_short_words, decode_complex_words, sight_word_reading,
#                                      letter_sum, fonem_sum, fonemsynthesis_sum, 
#                                      rhyme_sum, OS64_sum, OLAF_sum, word_reading_total, DLS_reading_comp_sum, reading_total)
# library("PerformanceAnalytics")
# chart.Correlation(teacher_rating[ , -c(1)], histogram=TRUE, pch=19)
# corrplot(cor(no_missing[ , -c(1)]), method = "number")

