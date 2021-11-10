#### Load packages ####
library(readxl)
library(tidyverse)
library(janitor)
library(psych)
library(dplyr)
library(mice)
library(GGally)
library(lavaan)
library(nonnest2)
library(broom)
library(ggplot2)
library(ggpubr)

seed <- 1


#### Import data ####
## Create list of sheetnames according to the Excel file
sheetname_list <- c("word_recognition_timed", "word_recognition_untimed", "phonological_decoding_timed", "phonological_decoding_untimed", 
                    "visual_stm", "reading_comprehension", "verbal_fluency_letter", "verbal_fluency_category", 
                    "blending", "elision", "fortysix_items", "ran_colors", 
                    "ran_letters", "verbal_elwm", "vocabulary", "listening_comprehension", "home_literacy", "phonological_stm", 
                    "grammatical_comprehension", "visual_elwm", "spatial_stm", "hearing", "vision")

## Read in data from excel file
read_data_function <- function(sheet) {
  read_excel("data/rawdata_aidr.xlsx", sheet = sheet)
}

data_decoding <- map(.x = sheetname_list, ~read_data_function(.x))

#convert variable names to snake (will mostly remove Swedish characters in variable names) and adds sheetname to all variables except participant
for (i in 1:length(sheetname_list)) {
  data_decoding[[i]] <- clean_names(data_decoding[[i]], "snake")
  colnames(data_decoding[[i]])[2:length(colnames(data_decoding[[i]]))] <- paste0(sheetname_list[[i]], "__",
                                                                                 colnames(data_decoding[[i]])[2:length(colnames(data_decoding[[i]]))]) #adds __ as separator between test name och variable
}

## Read sample characteristics from a separate file
data_sample_characteristics <- read_excel("data/samplechar_aidr.xlsx") %>%
  clean_names("snake") %>%
  dplyr::select(participant, sex_0m_1f, ca_months_tot, iq_estimate) #only selects the variables that are used later

## Join and convert to one dataframe
imported_df <- data_decoding %>% reduce(full_join, by = "participant") %>%
  full_join(data_sample_characteristics, by = "participant")


## In the preregistration, two types of missing was specified. However, only one type of missing was used, 999. The other missing code for missing due to not completion of a task due to too difficult is not used. Below, all 999 are replaced with NA 
imported_df <- imported_df %>%
  mutate_all(list(~na_if(., "999")))


#### Excluding participants that don't meet the inclusion criteria####

# Exclusion can be made on any of the following criteria- related to the data
# NA on most test, n=161 -> n=139
# NA on any of the decoding tests, n=138
# Hearing, PTA up to 25, n=136
# Vision, visual acuity >=0.8, none exclude, n=136 


# calculation number of NAs per persons so that persons with too many NAs can be excluded
number_of_NAs <- apply(imported_df, MARGIN = 1, function(x) sum(is.na(x)))
imported_df <- cbind(imported_df, number_of_NAs)


# Excluding persons with too many NAs or NA on any of the four decoding tests 
# Too many means that they have not been tested on basically all tests. Here operationalized as more than 890 NAs of 920 variables
# NA on any of the four decoding test have here been operationalized as NA on the first item of each test
imported_df <- imported_df %>%
  dplyr::filter(number_of_NAs<890) %>%
  dplyr::filter(!is.na(word_recognition_timed__ar_1)) %>%
  dplyr::filter(!is.na(word_recognition_untimed__is_1)) %>%
  dplyr::filter(!is.na(phonological_decoding_timed__us_1)) %>%
  dplyr::filter(!is.na(phonological_decoding_untimed__of_1))


# Excluding two persons related to hearing (PTA > 25 or hearing not tested) but keeping hearing aid users who have NA on PTA because they cannot be tested with the equipment
imported_df <- imported_df %>%
  mutate(hearing_better_ear_pta=pmin(hearing__right_pta, hearing__left_pta, na.rm=TRUE)) %>%
  dplyr::filter((hearing_better_ear_pta <= 25)|(hearing__hearing_aid=="yes")) #removes two participants with PTA>25 but keeps the hearing aid user aidr111 who don't have a pta


# Excluding none with vision problems (inclusion criterion any vision measure >=0.8) 
imported_df <- imported_df %>%
  dplyr::filter((as.numeric(vision__vision_10_feet) >= 0.8)|(as.numeric(vision__vision_16_inches) >= 0.8)) #removes zero participants with vision <= 0.8


# removing the measures used for exclusion from the dataframe because they are not needed below
imported_df <- imported_df %>%
  dplyr::select(- number_of_NAs, -contains("hearing"), -contains("vision"))







#### Correcting home literacy for single parents####


## Doubling the scores for single parents on home literacy according to pre-registration
imported_df <- imported_df %>%
  mutate(home_literacy__home_literacy_parent2_books = if_else(is.na(home_literacy__home_literacy_parent2_books), home_literacy__home_literacy_parent1_books, home_literacy__home_literacy_parent2_books)) %>%
  mutate(home_literacy__home_literacy_parent2_web = if_else(is.na(home_literacy__home_literacy_parent2_web), home_literacy__home_literacy_parent1_web, home_literacy__home_literacy_parent2_web)) %>%
  mutate(home_literacy__home_literacy_parent2_comics = if_else(is.na(home_literacy__home_literacy_parent2_comics), home_literacy__home_literacy_parent1_comics, home_literacy__home_literacy_parent2_comics)) %>%
  mutate(home_literacy__home_literacy_parent2_magazines = if_else(is.na(home_literacy__home_literacy_parent2_magazines), home_literacy__home_literacy_parent1_magazines, home_literacy__home_literacy_parent2_magazines)) %>%
  mutate(home_literacy__home_literacy_parent2_education = if_else(is.na(home_literacy__home_literacy_parent2_education), home_literacy__home_literacy_parent1_education, home_literacy__home_literacy_parent2_education))






####Calculate sum variables 
## Calculate the sum for each test that has sum scores for all variables except home literacy. No variables, except home literacy, has missing for a single item. Either they did the full test or not. Therefore, home literacy is separated here and joined back with the dataframe after the calculations

df_home_literacy <- imported_df %>%
  dplyr::select(participant, contains("home_literacy"))


df_for_imputation <- imported_df %>% 
  dplyr::select(everything() , -contains("total")) %>% #some variables has a total score in the data file. This should not be included in the sum. It has manually been checked that the calculated sum and the total score are the same
  gather(key = "key", value = "value", -contains("participant"), -contains("error") #excludes the RAN errors from the sum of RAN scores
         , -contains("home_literacy")) %>% #excludes home literacy variables
  separate(col = "key", into = c("variable_name", "item"), sep = "__", fill="right") %>% #the sample characteristics will be kept here with NA for item
  mutate(value = as.numeric(value)) %>% 
  dplyr::filter(!is.na(value)) %>%
  group_by(participant, variable_name) %>%
  summarise(sum = sum(value, na.rm = TRUE)) %>%
  spread(variable_name, sum) %>%
  ungroup()





#### Calculate computed variables for mental age, decoding, Phonological awareness, RAN and Verbal fluency based on variables without NA####

df_for_imputation <- df_for_imputation %>%
  mutate(mental_age = ca_months_tot * iq_estimate / 100)

#### PCA analysis decoding variables ####


decoding_pca1 <- df_for_imputation %>%
  dplyr::select(contains("word_recognition"), contains("phonological_decoding")) %>%
  principal(nfactors = 1, method = "regression")

# It is doubble checked that a one component solution is good by inspecting the eigenvalues
decoding_pca1[["values"]]
# [1] 3.2758489 0.4061958 0.2188975 0.0990578
# As can be seen, only the first value is higher than 1, so a one component solution is used


decoding_variable <- as.data.frame(decoding_pca1[["scores"]])

df_for_imputation$dec_composite <- decoding_variable$PC1


#calculate measures from the PCA to be used in the text
pca_decoding_loadings_min <- min(decoding_pca1[["loadings"]])
pca_decoding_loadings_max <- max(decoding_pca1[["loadings"]])
pca_decoding_explained_variance_percent <- decoding_pca1[["Vaccounted"]][2,1]*100

#### z-transformed measures ####

## PA ##

pa_ztrans <- df_for_imputation %>%
  select(blending, elision, fortysix_items) %>%
  scale() %>%
  as.data.frame()

pa_ztrans$sum_z_pa <- rowSums(cbind(pa_ztrans$blending, pa_ztrans$elision, pa_ztrans$fortysix_items))

df_for_imputation$pa_composite <- pa_ztrans$sum_z_pa



## RAN ## 

ran_ztrans <- df_for_imputation %>%
  select(ran_letters, ran_colors) %>%
  scale() %>%
  as.data.frame()

ran_ztrans$sum_z_ran <- rowSums(cbind(ran_ztrans$ran_letters, ran_ztrans$ran_colors))

df_for_imputation$ran_composite <- ran_ztrans$sum_z_ran



## Verbal fluency ##

vf_ztrans <- df_for_imputation %>%
  select(verbal_fluency_category, verbal_fluency_letter) %>%
  scale() %>%
  as.data.frame()

vf_ztrans$sum_z_vf <- rowSums(cbind(vf_ztrans$verbal_fluency_category, vf_ztrans$verbal_fluency_letter))

df_for_imputation$vf_composite <- vf_ztrans$sum_z_vf



#calculate correlations between the variables that are z-transformed so that they can be reported in the text
cor_pa <- df_for_imputation %>%
  select(blending, elision, fortysix_items) %>%
  cor()
cor_ran <- df_for_imputation %>%
  select(ran_letters, ran_colors) %>%
  cor()
cor_vf <- df_for_imputation %>%
  select(verbal_fluency_category, verbal_fluency_letter) %>%
  cor()

cor_ztransform_variables_min <-min(cor_pa[2,1], cor_pa[3,1], cor_pa[3,2],
                                   cor_ran[2,1], cor_vf[2,1])
cor_ztransform_variables_max <-max(cor_pa[2,1], cor_pa[3,1], cor_pa[3,2],
                                   cor_ran[2,1], cor_vf[2,1])




#calculate number of included participants and number of girls to be used in the text
#n_included <- nrow(df_for_imputation)
#n_girls <- sum(df_for_imputation$sex_0m_1f) #girls are coded as 1 and boys as 0, so a sum gives number of girls




# adding the home literacy variables to the dataframe again
df_for_imputation <- full_join(df_for_imputation, df_home_literacy, by="participant")


#### Missing values imputation####

## Checking percentage of missing data 
pMiss <- function(x){sum(is.na(x))/length(x)*100}
missing_per_column <- apply(df_for_imputation,2,pMiss)
missing_per_column_max <- max(missing_per_column)
missing_average <- mean(missing_per_column)
# The variable with most missing was a home literacy variable with 2.2% missing. Therefore imputation of variables are conducted

## Impute missing data
#initiatie the imputation so that the method is visible in meth and so that the predictor matrix can be manipulated
init = mice(df_for_imputation, maxit=0)
meth = init$method
predM = init$predictorMatrix

#Manipulate the predictor matrix so that participant, the decoding variables and reading comprehension is not used as a basis for the imputation
predM[, c("participant")]=0
predM[, c("word_recognition_timed")]=0
predM[, c("word_recognition_untimed")]=0
predM[, c("phonological_decoding_timed")]=0
predM[, c("phonological_decoding_untimed")]=0
predM[, c("reading_comprehension")]=0
predM[, c("dec_composite")]=0

number_of_datasets <- 5
imputed_datsets <- mice(df_for_imputation, predictorMatrix=predM, m=number_of_datasets, maxit=5, meth='pmm', seed=seed) #pmm is the default method 


complete_data_list <- vector(mode = "list", length = number_of_datasets)
for (i in 1:number_of_datasets) {
  complete_data_list[[i]] <- complete(imputed_datsets, i)
}


# A function that sums the home literacy variables of one dataframe and then adds the home_literacy sum to the dataframe and removes the home literacy variables for individual questions
calculate_home_literacy_sum_function <- function(df) {
  df_hl <- df %>%
    dplyr::select(participant, contains("home_literacy"), -contains("total")) %>% #selects all home literacy variables except the total
    gather(key = "key", value = "value", -participant) %>%
    separate(col = "key", into = c("variable_name", "item"), sep = "__") %>%
    group_by(participant, variable_name) %>%
    summarise(sum = sum(value, na.rm = TRUE), .groups="drop") %>%
    spread(variable_name, sum) 
  
  df <- left_join(df, df_hl, by = "participant") %>%
    dplyr::select(-contains("home_literacy__"))
  
  return(df)
}


complete_data_list <- map(complete_data_list, ~calculate_home_literacy_sum_function(.x)) #runs the calculate_home_literacy_sum_function on all complete datasets

devtrajdata <- as.data.frame(complete_data_list[1])

diagnoses <- read_excel("data/diagnoses.xlsx")
comorbid_data <- diagnoses %>%
  dplyr::filter(!is.na(comorbidity)) %>%
  dplyr::filter(!is.na(participant)) %>%
  dplyr::select(participant, comorbidity, ASD, other, contains("ADHD")) 


devtrajdata <- merge(devtrajdata, comorbid_data, by = "participant")

devtrajdata <- devtrajdata %>%
  mutate(comorbidity_status = case_when(comorbidity == 0 ~ "ID_only", 
                                        comorbidity == 1 ~ "comorbidity"))

devtrajdata <- devtrajdata %>%
  mutate(age_group = case_when(mental_age <= 103 ~ "MAmax103",
                               mental_age > 103 ~ "MAmin103"))


describeBy(devtrajdata, group = "age_group")



## Creating a composite variable of phonological decoding
phondec_ztrans <- devtrajdata %>%
  select(phonological_decoding_timed, phonological_decoding_untimed) %>%
  scale() %>%
  as.data.frame()

phondec_ztrans$phon_dec <- rowSums(cbind(phondec_ztrans$phonological_decoding_timed, phondec_ztrans$phonological_decoding_untimed))

devtrajdata$phon_dec <- phondec_ztrans$phon_dec

## Creating a composite variable of word recognition
wordrec_ztrans <- devtrajdata %>%
  select(word_recognition_timed, word_recognition_untimed) %>%
  scale() %>%
  as.data.frame()

wordrec_ztrans$word_rec <- rowSums(cbind(wordrec_ztrans$word_recognition_timed, wordrec_ztrans$word_recognition_untimed))

devtrajdata$word_rec <- wordrec_ztrans$word_rec

## Inverting RAN scores for easier visual interpretation

devtrajdata <- devtrajdata %>%
  mutate(ran_inverted = ran_composite * -1)


## z-transformed measures
devtrajdata$reading_comprehension_z <-scale(devtrajdata$reading_comprehension)

devtrajdata$listening_comprehension_z <-scale(devtrajdata$listening_comprehension)

devtrajdata$vocabulary_z <- scale(devtrajdata$vocabulary)

devtrajdata$verbal_elwm_z <- scale(devtrajdata$verbal_elwm)

devtrajdata$pa_z <- scale(devtrajdata$pa_composite)

devtrajdata$dec_z <- scale(devtrajdata$dec_composite)

devtrajdata$ran_inverted_z <- scale(devtrajdata$ran_inverted)


## Creating an SVR-variable (the product of listening comprehension and decoding)
devtrajdata <- devtrajdata %>%
  mutate(svrproduct = (dec_z + 100) * (listening_comprehension_z +100))

devtrajdata$svrproduct_z <- scale(devtrajdata$svrproduct)

## selecting variables and changing to long format

dec_rc_comorb <- devtrajdata %>%
  select(mental_age, age_group, comorbidity_status, dec_z, reading_comprehension_z) %>%
  gather(key = "variable", value = "value", -mental_age, -age_group, -comorbidity_status)

testsvr <- devtrajdata %>%
  select(mental_age, age_group, svrproduct_z, reading_comprehension_z, dec_z, listening_comprehension_z) %>%
  gather(key = "variable", value = "value", -mental_age, -age_group)

dec_comorb_ma <- devtrajdata %>%
  select(mental_age, age_group, comorbidity_status, dec_z) %>%
  gather(key = "variable", value = "value", -mental_age, -age_group, -comorbidity_status)


plotdata_list <- devtrajdata %>%
  select(ca_months_tot, mental_age, iq_estimate, age_group, dec_z, reading_comprehension_z, listening_comprehension_z, pa_z) %>%
  gather(key = "variable", value = "value", -ca_months_tot, -mental_age, -iq_estimate, -age_group)

plotdata_decoding <- devtrajdata %>%
  select(ca_months_tot, mental_age, iq_estimate, word_rec, phon_dec, reading_comprehension_z, listening_comprehension_z) %>%
  gather(key = "variable", value = "value", -ca_months_tot, -mental_age, -iq_estimate)

plotdata_all <- devtrajdata %>%
  select(ca_months_tot, mental_age, age_group, dec_z, reading_comprehension_z, pa_z, vocabulary_z, verbal_elwm_z, ran_inverted_z) %>%
  gather(key = "variable", value = "value", -ca_months_tot, -mental_age, -age_group)



## plot of decoding, reading comprehension - mental age
ggplot(dec_rc_comorb, aes(x=mental_age, y=value, color=variable)) + 
  geom_smooth(method = "loess") + 
  scale_color_manual(values = c("red", "blue"), labels = c("Decoding", "Reading comprehension")) + 
  geom_point(aes(shape=variable)) + 
  scale_shape_manual(values = c(5, 16), labels = c("Decoding", "Reading comprehension")) + 
  guides(color = guide_legend("Variable"), shape = guide_legend("Variable")) + 
  stat_cor(label.x = 130) +
  stat_regline_equation(label.x = 75, aes(label = ..rr.label..)) + labs(x="Mental age (months)")


## plot of phonological decoding, word recognition, reading comprehension, listening comprehension - chronological age
ggplot(plotdata_decoding, aes(x=ca_months_tot, y=value, color=variable)) + geom_smooth(method = "loess") + 
  scale_color_manual(values = c("red", "blue", "green", "pink") , labels = c("Listening comprehension", "Phonological decoding", "Reading comprehension", "Word recognition")) + 
  geom_point(aes(shape=variable)) + 
  scale_shape_manual(values = c(16, 16, 16, 16), labels = c("Listening comprehension", "Phonological decoding", "Reading comprehension", "Word recognition")) + 
  guides(color = guide_legend("Variable"), shape = guide_legend("Variable")) + 
  stat_cor(label.x = 180) +
  stat_regline_equation(label.x = 150, aes(label = ..rr.label..)) + labs(x="Chronological age (months)")


## plot of phonological decoding, word recognition, reading comprehension, listening comprehension - mental age
ggplot(plotdata_decoding, aes(x=mental_age, y=value, color=variable)) + geom_smooth(method = "loess") + 
  scale_color_manual(values = c("red", "blue", "green", "pink") , labels = c("Listening comprehension", "Phonological decoding", "Reading comprehension", "Word recognition")) + 
  geom_point(aes(shape=variable)) + 
  scale_shape_manual(values = c(16, 16, 16, 16), labels = c("Listening comprehension", "Phonological decoding", "Reading comprehension", "Word recognition")) + 
  guides(color = guide_legend("Variable"), shape = guide_legend("Variable")) + 
  stat_cor(label.x = 130) +
  stat_regline_equation(label.x = 75, aes(label = ..rr.label..)) + labs(x="Mental age (months)")



## decoding, readcomp, ma and comorbidity status
ggplot(dec_rc_comorb, aes(x=mental_age, y=value, color=variable)) + 
  geom_smooth(method = "loess") + 
  scale_color_manual(values = c("red", "blue"), labels = c("Decoding", "Reading compr")) + 
  geom_point(aes(shape=variable)) + 
  scale_shape_manual(values = c(5, 16), labels = c("Decoding", "Reading compr")) + 
  facet_wrap(~ comorbidity_status) +
  guides(color = guide_legend("Variable"), shape = guide_legend("Variable")) + 
  stat_cor(label.x = 120) +
  stat_regline_equation(label.x = 70, aes(label = ..rr.label..)) + labs(x="Mental age (months)")


## decoding, readcomp, ma divided into two age groups
ggplot(dec_rc_comorb, aes(x=mental_age, y=value, color=age_group)) + 
  geom_smooth(method = "lm") +
  geom_point() + 
  facet_grid(~ variable) +
  stat_cor(label.x = 120) +
  stat_regline_equation(label.x = 70, aes(label = ..rr.label..)) + labs(x="Mental age (months)")

## decoding, pa, reading comprehension, listening comprehension, ma divided into age groups
ggplot(plotdata_list, aes(x=mental_age, y=value, color=age_group)) + 
  geom_smooth(method = "lm") +
  geom_point() + 
  facet_wrap(~ variable) +
  stat_cor(label.x = 120) +
  stat_regline_equation(label.x = 70, aes(label = ..rr.label..)) + labs(x="Mental age (months)")

## svr product, decoding, readcomp, listcomp, ma
ggplot(testsvr, aes(x=mental_age, y=value, color=variable)) + 
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("red", "blue", "green", "gold")) +
  geom_point() + 
  stat_cor(label.x = 120) +
  stat_regline_equation(label.x = 70, aes(label = ..rr.label..)) + labs(x="Mental age (months)")


## decoding, readcomp, vocabulary, verbal elwm, pa, ran - ma divided into age groups
ggplot(plotdata_all, aes(x=mental_age, y=value, color=age_group)) + 
  geom_smooth(method = "lm") +
  geom_point() + 
  facet_wrap(~ variable) +
  stat_cor(label.x = 120) +
  stat_regline_equation(label.x = 70, aes(label = ..rr.label..)) + labs(x="Mental age (months)")
