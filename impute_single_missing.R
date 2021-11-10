# 
# Code by Lisa Palmqvist 2021-09-14
# impute single missing values in a test with mice-method.
# thereafter, I have manually replaced the values in data_inmatning.xlsx
#


library(rio)
library(dplyr)

set.seed(42)

#Data_inmatning
data_inmatning <- import("data/data_inmatning_missing.xlsx")

summary(data_inmatning)

#impute letters variable
letters <- data_inmatning %>% select(ID, letter_1, letter_2, letter_3, letter_4, letter_5, letter_6, letter_7, letter_8, letter_sum)
letters <- letters %>% filter(!is.na(letter_sum))
summary(letters)
#### Mice imputation
tempLetters <- mice(letters,m=1,maxit=50,meth='pmm',seed=500)
no_missing_letters <- complete(tempLetters)


#impute fonem variable
fonems <- data_inmatning %>% select(ID, fonem_a, fonem_b, fonem_c, fonem_d, fonem_e, fonem_f, fonem_g, fonem_h, fonem_i, fonem_sum)
fonems <- fonems %>% filter(!is.na(fonem_sum))
summary(fonems)
#### Mice imputation
tempfonems <- mice(fonems,m=1,maxit=50,meth='pmm',seed=500)
no_missing_fonems <- complete(tempfonems)


#impute fonemsynthesis variable
fonemsynthesiss <- data_inmatning %>% select(ID, fonemsynthesis_a, fonemsynthesis_b, fonemsynthesis_c, fonemsynthesis_d, fonemsynthesis_e, fonemsynthesis_f, fonemsynthesis_g, fonemsynthesis_h, fonemsynthesis_i, fonemsynthesis_sum)
fonemsynthesiss <- fonemsynthesiss %>% filter(!is.na(fonemsynthesis_sum))
summary(fonemsynthesiss)
#### Mice imputation
tempfonemsynthesiss <- mice(fonemsynthesiss,m=1,maxit=50,meth='pmm',seed=500)
no_missing_fonemsynthesiss <- complete(tempfonemsynthesiss)

#impute rhyme variable
rhymes <- data_inmatning %>% select(ID, rhyme_a, rhyme_b, rhyme_c, rhyme_d, rhyme_e, rhyme_f, rhyme_g, rhyme_h, rhyme_i, rhyme_sum)
rhymes <- rhymes %>% filter(!is.na(rhyme_sum))
summary(rhymes)
#### Mice imputation
temprhymes <- mice(rhymes,m=1,maxit=50,meth='pmm',seed=500)
no_missing_rhymes <- complete(temprhymes)


#impute OS64 variable
OS64s <- data_inmatning %>% select(ID, OS64_1, OS64_2, OS64_3, OS64_4, OS64_5, OS64_6, OS64_7, OS64_8, OS64_9, OS64_10, OS64_11, OS64_12, OS64_13, OS64_14, OS64_15, OS64_sum)
OS64s <- OS64s %>% filter(!is.na(OS64_sum))
summary(OS64s)
#### Mice imputation
tempOS64s <- mice(OS64s,m=1,maxit=50,meth='pmm',seed=500)
no_missing_OS64s <- complete(tempOS64s)


#impute OLAF variable
OLAFs <- data_inmatning %>% select(ID, OLAF_1, OLAF_2, OLAF_3, OLAF_4, OLAF_5, OLAF_6, OLAF_7, OLAF_8, OLAF_9, OLAF_10, OLAF_11, OLAF_12, OLAF_13, OLAF_sum)
OLAFs <- OLAFs %>% filter(!is.na(OLAF_sum))
summary(OLAFs)
#### Mice imputation
tempOLAFs <- mice(OLAFs,m=1,maxit=50,meth='pmm',seed=500)
no_missing_OLAFs <- complete(tempOLAFs)


#impute word_rep variable
word_reps <- data_inmatning %>% select(ID, word_rep_1, word_rep_2, word_rep_3, word_rep_4, word_rep_5, word_rep_6, word_rep_7, word_rep_8, word_rep_9, word_rep_10, word_rep_sum)
word_reps <- word_reps %>% filter(!is.na(word_rep_sum))
summary(word_reps)
#### Mice imputation
tempword_reps <- mice(word_reps,m=1,maxit=50,meth='pmm',seed=500)
no_missing_word_reps <- complete(tempword_reps)


#impute DLS_reading_comp variable
DLS_reading_comps <- data_inmatning %>% select(ID, DLS_reading_comp_1, DLS_reading_comp_2, DLS_reading_comp_3, DLS_reading_comp_4, DLS_reading_comp_5, DLS_reading_comp_6, 
                                               DLS_reading_comp_7, DLS_reading_comp_8, DLS_reading_comp_9, DLS_reading_comp_10, DLS_reading_comp_11, DLS_reading_comp_12, 
                                               DLS_reading_comp_13, DLS_reading_comp_14, DLS_reading_comp_15, DLS_reading_comp_16, DLS_reading_comp_17, DLS_reading_comp_18, DLS_reading_comp_19, DLS_reading_comp_20, DLS_reading_comp_sum)
DLS_reading_comps <- DLS_reading_comps %>% filter(!is.na(DLS_reading_comp_sum))
summary(DLS_reading_comps)
#### Mice imputation
tempDLS_reading_comps <- mice(DLS_reading_comps,m=1,maxit=50,meth='pmm',seed=500)
no_missing_DLS_reading_comps <- complete(tempDLS_reading_comps)


#impute DLS_listening_comp variable
DLS_listening_comps <- data_inmatning %>% select(ID, DLS_listening_comp_1, DLS_listening_comp_2, DLS_listening_comp_3, DLS_listening_comp_4, DLS_listening_comp_5, DLS_listening_comp_6, 
                                               DLS_listening_comp_7, DLS_listening_comp_8, DLS_listening_comp_9, DLS_listening_comp_10, DLS_listening_comp_11, DLS_listening_comp_12, 
                                               DLS_listening_comp_13, DLS_listening_comp_14, DLS_listening_comp_15, DLS_listening_comp_16, DLS_listening_comp_17, DLS_listening_comp_18, DLS_listening_comp_19, DLS_listening_comp_20, DLS_listening_comp_sum)
DLS_listening_comps <- DLS_listening_comps %>% filter(!is.na(DLS_listening_comp_sum))
summary(DLS_listening_comps)
#### Mice imputation
tempDLS_listening_comps <- mice(DLS_listening_comps,m=1,maxit=50,meth='pmm',seed=500)
no_missing_DLS_listening_comps <- complete(tempDLS_listening_comps)

