# 
# Code by Lisa Palmqvist 2021-08-05
# read cognitive data files
# Third script
#

library(rio)
library(dplyr)
library(tidyr)

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
                                            rhyme_sum, OS64_sum, OLAF_sum, word_rep_sum, sentence_rep_sum,	
                                            DLS_reading_comp_sum, DLS_listening_comp_sum)



data_df$letter_sum <- as.numeric(data_df$letter_sum)
data_df$fonem_sum <- as.numeric(data_df$fonem_sum)
data_df$fonemsynthesis_sum <- as.numeric(data_df$fonemsynthesis_sum)
data_df$rhyme_sum <- as.numeric(data_df$rhyme_sum)
data_df$OS64_sum <- as.numeric(data_df$OS64_sum)
data_df$OLAF_sum <- as.numeric(data_df$OLAF_sum)
data_df$word_rep_sum <- as.numeric(data_df$word_rep_sum)
data_df$sentence_rep_sum <- as.numeric(data_df$sentence_rep_sum)
data_df$DLS_reading_comp_sum <- as.numeric(data_df$DLS_reading_comp_sum)
data_df$DLS_listening_comp_sum <- as.numeric(data_df$DLS_listening_comp_sum)

#create indices on OS64 and OLAF to create word_reading_total. z-transformation used.
data_df$word_reading_total <- (scale(data_df$OS64_sum, center = F)+scale(data_df$OLAF_sum, center = F))/2


#Read Insamlad data alla grupper for extracting the dates for testing
dates_tested_wide <- import("data/insamlad_data_alla_grupper.xlsx")
dates_tested_wide <- dates_tested_wide %>% select(id = "Kod", t1_dag, t2_dag,	t3_dag,	t4_dag)
dates_tested <- gather(dates_tested_wide, time, days, t1_dag:t4_dag, factor_key=TRUE)
dates_tested$time <- as.numeric(dates_tested$time)

data_df <- full_join(data_df, dates_tested, by = c("id", "time"))

#Read backgrond data and Raven
background_raven <- import("data/bakgrundsdata_raven.xlsx")
background_raven <- background_raven %>% select(id = ID, group, raven = score_total, gender, date_of_birth, date_t1, IQ = standard_score)
background_raven <- background_raven %>% mutate(age_in_days = date_t1-date_of_birth)
background_raven <- background_raven %>% mutate(age_scale = scale(age_in_days))

background_raven$IQ <- as.numeric(background_raven$IQ)
background_raven <- background_raven %>% mutate(IQ_scale = scale(IQ))

data_df <- full_join(data_df, background_raven, by = c("id", "group"))
data_df <- full_join(data_df, app_data_df, by = c("id"))

#Parent survey
parent <- import("data/parent_survey.xlsx")
parent <- parent %>% select(id = "Elevens kod:", education_mother = "education_mother_or_other",	education_father,	occupation_mother,	occupation_father, SES, `Only ID`, Autismspektrumtillstand, `Downs syndrom`, CP, `ADHD/ADD`, Other)
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
  mutate(tot_train_time_scale= scale(tot_train_time))%>% 
  mutate(PA = fonem_sum+fonemsynthesis_sum+rhyme_sum)   # create new phoneme variable called PA

#Read student survey
student_survey <- import("data/student_survey.xlsx")
student_survey <- student_survey %>% select(id = "student_code", before1_after2_intervention, synthesis,	segmentation,	
                                            sound_letter_connection,	understand_rhyme, decode_short_words, decode_complex_words, sight_word_reading)
student_survey <- student_survey %>% filter(before1_after2_intervention == 1)
student_survey <- student_survey %>% select(-before1_after2_intervention)
data_df <- full_join(data_df, student_survey, by = c("id"))

#Read speech data

ppc <- import("data/Taldata PPC_PCC_PVC.xlsx")
ppc <- ppc %>% filter(time == 1)
ppc <- ppc %>% select(id = "ID", PPC = "PPC_only_BAF")
ppc$PPC <- as.numeric(ppc$PPC)
data_df <- full_join(data_df, ppc, by = c("id"))


#remove 403 due to testning not being done correctly 
data_df <- data_df %>% filter(id != 403)
#remove 110 terminates participating  
data_df <- data_df %>% filter(id != 110)
#remove 439 not being in school
data_df <- data_df %>% filter(id != 439)
#remove 403 due to testning not being done correctly 
data_df <- data_df %>% filter(id != 413)
#remove IQ over 90
#data_df <- data_df %>% filter(is.na(IQ)|IQ<90)


# teacher_rating <- data_df %>% select(id, synthesis,	segmentation,	sound_letter_connection,	understand_rhyme, decode_short_words, decode_complex_words, sight_word_reading,
#                                      letter_sum, fonem_sum, fonemsynthesis_sum, 
#                                      rhyme_sum, OS64_sum, OLAF_sum, word_reading_total, DLS_reading_comp_sum, reading_total)
# library("PerformanceAnalytics")
# chart.Correlation(teacher_rating[ , -c(1)], histogram=TRUE, pch=19)
# corrplot(cor(no_missing[ , -c(1)]), method = "number")


