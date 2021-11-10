# 
# Code by Lisa Palmqvist 2021-08-02
# read Animega-is app-data files to calculate total played time
# First script
#

## LOAD PACKAGES ####
library(rio)
library(striprtf)
library(dplyr)
library(readr)
library(reader)

# FUNCTIONS ####
# Function to extract time played and total days played from rtf file ####
rft_add_time_played_to_df <- function(file_create, file_test) {
  
  ## read .rtf file (I have manually converted them to .txt in Finder) 
  animega_create_rtf <- import(file_create)
  names(animega_create_rtf)[1] <- "Datum"
  names(animega_create_rtf)[2] <- "Typ"
  names(animega_create_rtf)[3] <- "Uppgift"
  names(animega_create_rtf)[10] <- "SL"
  
  animega_test_rtf <- import(file_test) 
  names(animega_test_rtf)[1] <- "Datum"
  names(animega_test_rtf)[2] <- "Typ"
  names(animega_test_rtf)[3] <- "Uppgift"
  names(animega_test_rtf)[10] <- "SL"
  
  animega_test_rtf_subset <- animega_test_rtf %>% select(Datum, Typ, Uppgift,SL)
  animega_create_rtf_subset <- animega_create_rtf %>% select(Datum, Typ, Uppgift,SL)
  
  animega <- bind_rows(animega_test_rtf_subset, animega_create_rtf_subset)
  animega$Datum = as.POSIXct(animega$Datum,format='%Y-%m-%d %H:%M:%S')
  animega_sorted <- arrange(animega, Datum)
  animega_sorted <- mutate(animega_sorted, Date = as.POSIXlt(as.Date(Datum)))
  
  
  # F??r att rakna ut hur l??ng tid det ??r mellan tv?? klick. G??r en column d??r varje v??rde laggar en fr??n ovan f??r att sedan kunna r??kna ut differensen. Differensen r??knas sedan ut nedan i clicksdiff- OBS. Ett extremt h??gt clicksdiff beror p?? att det v??xlar mellan tv?? deltagare som har helt olika timestamp, ex. 1970 - 2016 
  animega_sorted2 <- animega_sorted %>% 
    group_by(Date) %>%
    mutate(lagclicks=lag(Datum))  %>% 
    mutate(clicksdiff= ifelse((Datum-lagclicks) > 600, 0, Datum-lagclicks)) #If the time is longer than 10 min, then set the time to 0 
  
  # Create variables
  id <- as.numeric(gsub(".*?([0-9]+).*", "\\1", file_create))
  time_played_animega <- sum(animega_sorted2$clicksdiff, na.rm = T)
  days_played_animega <- sapply(animega_sorted2, function(x) length(unique(x)))["Date"]
  # Join the variables to create a data frame
  temp_df <- data.frame(id,time_played_animega,days_played_animega)
  # add new data to animega_df
  animega_df <- rbind(animega_df, temp_df)
}
# Function to extract time played and total days played from txt file ####
add_time_played_to_df <- function(file_create, file_test) {

  #read .txt file
  animega_create_txt <- read.delim(file_create, sep =",")
  colnames(animega_create_txt) <- animega_create_txt[1,]
  animega_create_txt <- animega_create_txt[-1, ] 
  
  animega_test_txt <- read.delim(file_test, sep =",")
  colnames(animega_test_txt) <- animega_test_txt[1,]
  animega_test_txt <- animega_test_txt[-1, ] 
  
  animega_test_txt_subset <- animega_test_txt %>% select(Datum, Typ, Uppgift,SL)
  animega_create_txt_subset <- animega_create_txt %>% select(Datum, Typ, Uppgift,SL)
  
  animega <- bind_rows(animega_test_txt_subset, animega_create_txt_subset)
  animega$Datum = as.POSIXct(animega$Datum,format='%Y-%m-%d %H:%M:%S')
  animega_sorted <- arrange(animega, Datum)
  animega_sorted <- mutate(animega_sorted, Date = as.POSIXlt(as.Date(Datum)))
  
  
  # F??r att rakna ut hur l??ng tid det ??r mellan tv?? klick. G??r en column d??r varje v??rde laggar en fr??n ovan f??r att sedan kunna r??kna ut differensen. Differensen r??knas sedan ut nedan i clicksdiff- OBS. Ett extremt h??gt clicksdiff beror p?? att det v??xlar mellan tv?? deltagare som har helt olika timestamp, ex. 1970 - 2016 
  animega_sorted2 <- animega_sorted %>% 
    group_by(Date) %>%
    mutate(lagclicks=lag(Datum))  %>% 
    mutate(clicksdiff= ifelse((Datum-lagclicks) > 600, 0, Datum-lagclicks)) #If the time is longer than 10 min, then set the time to 0 
  
  # Create variables
  id <- as.numeric(gsub(".*?([0-9]+).*", "\\1", file_create))
  time_played_animega <- sum(animega_sorted2$clicksdiff, na.rm = T)
  days_played_animega <- sapply(animega_sorted2, function(x) length(unique(x)))["Date"]
  # Join the variables to create a data frame
  temp_df <- data.frame(id,time_played_animega,days_played_animega)
  # add new data to animega_df
  animega_df <- rbind(animega_df, temp_df)
}
#om endast CREATE finns (txt)
CREATE_add_time_played_to_df <- function(file_create) {
  
  #START ####
  
  #read .txt file
  animega_create_txt <- read.delim(file_create, sep =",")
  colnames(animega_create_txt) <- animega_create_txt[1,]
  animega_create_txt <- animega_create_txt[-1, ] 
  
  animega_create_txt_subset <- animega_create_txt %>% select(Datum, Typ, Uppgift,SL)
  
  animega <- animega_create_txt_subset
  animega$Datum = as.POSIXct(animega$Datum,format='%Y-%m-%d %H:%M:%S')
  animega_sorted <- arrange(animega, Datum)
  animega_sorted <- mutate(animega_sorted, Date = as.POSIXlt(as.Date(Datum)))
  
  
  # F??r att rakna ut hur l??ng tid det ??r mellan tv?? klick. G??r en column d??r varje v??rde laggar en fr??n ovan f??r att sedan kunna r??kna ut differensen. Differensen r??knas sedan ut nedan i clicksdiff- OBS. Ett extremt h??gt clicksdiff beror p?? att det v??xlar mellan tv?? deltagare som har helt olika timestamp, ex. 1970 - 2016 
  animega_sorted2 <- animega_sorted %>% 
    group_by(Date) %>%
    mutate(lagclicks=lag(Datum))  %>% 
    mutate(clicksdiff= ifelse((Datum-lagclicks) > 600, 0, Datum-lagclicks)) #If the time is longer than 10 min, then set the time to 0 
  
  # Create variables
  id <- as.numeric(gsub(".*?([0-9]+).*", "\\1", file_create))
  time_played_animega <- sum(animega_sorted2$clicksdiff, na.rm = T)
  days_played_animega <- sapply(animega_sorted2, function(x) length(unique(x)))["Date"]
  # Join the variables to create a data frame
  temp_df <- data.frame(id,time_played_animega,days_played_animega)
  # add new data to animega_df
  animega_df <- rbind(animega_df, temp_df)
  
}
#om endast TEST finns (txt)
TEST_add_time_played_to_df <- function(file_test) {
  
  #START ####
  
  #read .txt file
  animega_test_txt <- read.delim(file_test, sep =",")
  colnames(animega_test_txt) <- animega_test_txt[1,]
  animega_test_txt <- animega_test_txt[-1, ] 
  
  animega_test_txt_subset <- animega_test_txt %>% select(Datum, Typ, Uppgift,SL)
  
  animega <- animega_test_txt_subset
  animega$Datum = as.POSIXct(animega$Datum,format='%Y-%m-%d %H:%M:%S')
  animega_sorted <- arrange(animega, Datum)
  animega_sorted <- mutate(animega_sorted, Date = as.POSIXlt(as.Date(Datum)))
  
  
  # F??r att rakna ut hur l??ng tid det ??r mellan tv?? klick. G??r en column d??r varje v??rde laggar en fr??n ovan f??r att sedan kunna r??kna ut differensen. Differensen r??knas sedan ut nedan i clicksdiff- OBS. Ett extremt h??gt clicksdiff beror p?? att det v??xlar mellan tv?? deltagare som har helt olika timestamp, ex. 1970 - 2016 
  animega_sorted2 <- animega_sorted %>% 
    group_by(Date) %>%
    mutate(lagclicks=lag(Datum))  %>% 
    mutate(clicksdiff= ifelse((Datum-lagclicks) > 600, 0, Datum-lagclicks)) #If the time is longer than 10 min, then set the time to 0 
  
  # Create variables
  id <- as.numeric(gsub(".*?([0-9]+).*", "\\1", file_test))
  time_played_animega <- sum(animega_sorted2$clicksdiff, na.rm = T)
  days_played_animega <- sapply(animega_sorted2, function(x) length(unique(x)))["Date"]
  # Join the variables to create a data frame
  temp_df <- data.frame(id,time_played_animega,days_played_animega)
  # add new data to animega_df
  animega_df <- rbind(animega_df, temp_df)
}

# LOAD DATA ####
#Create list of files in dir 
files <- dir("data/appdata/Animega-is", pattern = "txt", full.names = T)
files_rtf <- dir("data/appdata/Animega-is/rtf", pattern = "rtf.txt", full.names = TRUE)

#create empty animega_df
animega_df <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("id", "time_played_animega", "days_played_animega"))))

#RTF-files ####
#created variable with ID numbers
all_id_rtf <- unique(parse_number(dir("data/appdata/Animega-is/rtf", pattern = "txt", full.names = F)))
#Run all id:s
#NB! will yeild warnings, these can be ignored. 
for (i in seq_along(all_id_rtf)) {

  createfile <- paste0("data/appdata/Animega-is/rtf/",all_id_rtf[i],"_create_rtf.txt")
  testfile <- paste0("data/appdata/Animega-is/rtf/",all_id_rtf[i],"_test_rtf.txt")
  
  animega_df <- rft_add_time_played_to_df(createfile, testfile)
}

# TXT-files ####
#created variable with ID numbers
all_id <- unique(parse_number(dir("data/appdata/Animega-is", pattern = "txt", full.names = F)))
#Run all id:s
#NB! Will yeald warning meassage which can be ignored: 
#In read.table(file = file, header = header, sep = sep, quote = quote,  :incomplete final line found by readTableHeader on 'data/appdata/Animega-is/341_test.txt'"
for (i in seq_along(all_id)) {
  
  createfile <- paste0("data/appdata/Animega-is/",all_id[i],"_create.txt")
  testfile <- paste0("data/appdata/Animega-is/",all_id[i],"_test.txt")

  #Run different functiona depending on if both test and creat file exists or not
  ifelse( find.file(testfile, dirs = NULL) == "" |find.file(createfile, dirs = NULL) == "", 
          ifelse(find.file(testfile, dirs = NULL) != "", 
                 animega_df <- TEST_add_time_played_to_df(testfile), 
          animega_df <- CREATE_add_time_played_to_df(createfile)), 
          animega_df <- add_time_played_to_df(createfile, testfile))
}

# #For checking specific individual ID separately ####
# 
# #read .txt file
# animega_create_txt_325 <- read.delim("data/appdata/Animega-is/325_create.txt", sep =",")
# colnames(animega_create_txt_325) <- animega_create_txt_325[1,]
# animega_create_txt_325 <- animega_create_txt_325[-1, ] 
# 
# animega_test_txt_325 <- read.delim("data/appdata/Animega-is/325_test.txt", sep =",")
# colnames(animega_test_txt_325) <- animega_test_txt_325[1,]
# animega_test_txt_325 <- animega_test_txt_325[-1, ] 
# 
# animega_test_txt_325_subset <- animega_test_txt_325 %>% select(Datum, Typ, Uppgift,SL)
# animega_create_txt_325_subset <- animega_create_txt_325 %>% select(Datum, Typ, Uppgift,SL)
# 
# animega_325 <- bind_rows(animega_test_txt_325_subset, animega_create_txt_325_subset)
# animega_325$Datum = as.POSIXct(animega_325$Datum,format='%Y-%m-%d %H:%M:%S')
# animega325_sorted <- arrange(animega_325, Datum)
# animega325_sorted <- mutate(animega325_sorted, Date = as.POSIXlt(as.Date(Datum)))
# 
# 
# # F??r att rakna ut hur l??ng tid det ??r mellan tv?? klick. G??r en column d??r varje v??rde laggar en fr??n ovan f??r att sedan kunna r??kna ut differensen. Differensen r??knas sedan ut nedan i clicksdiff- OBS. Ett extremt h??gt clicksdiff beror p?? att det v??xlar mellan tv?? deltagare som har helt olika timestamp, ex. 1970 - 2016 
# animega325_sorted2 <- animega325_sorted %>% 
#   group_by(Date) %>%
#   mutate(lagclicks=lag(Datum))  %>% 
#   # mutate(clicksdiff= (Datum-lagclicks)) %>%  
#   mutate(clicksdiff= ifelse((Datum-lagclicks) > 600, 0, Datum-lagclicks)) #If the time is longer than 10 min, then set the time to 0 
# 
# # id
# as.numeric(gsub(".*?([0-9]+).*", "\\1", "data/appdata/Animega-is/325_create.txt"))
# #time played Animega
# sum(animega325_sorted2$clicksdiff, na.rm = T)
# #days played Animega
# sapply(animega325_sorted2, function(x) length(unique(x)))
# 
# # read .rtf file (first manually converted to .txt in Finder) 
# animega_create_txt_305 <- import("data/appdata/Animega-is/305_create.txt")
# names(animega_create_txt305)[1] <- "Datum"
# names(animega_create_txt305)[10] <- "SL"
# animega_create_txt305$SL<-gsub("[\\]par","",as.character(animega_create_txt305$SL))

