# 
# Code by Lisa Palmqvist 2021-08-02
# read ALL app-data files to calculate total played time
# Second script
#

source("scripts/read_animega_appdata.R")

## LOAD PACKAGES ####
library(officer)
library(dplyr)
library(xml2)

#Function to convert word document to dataframe in R. NOTE file has to be in correct wd
get_tbls <- function(word_doc) {
  
  tmpd <- tempdir()
  tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
  
  file.copy(word_doc, tmpf)
  unzip(tmpf, exdir=sprintf("%s/docdata", tmpd))
  
  doc <- read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
  
  unlink(tmpf)
  unlink(sprintf("%s/docdata", tmpd), recursive=TRUE)
  
  ns <- xml_ns(doc)
  
  tbls <- xml_find_all(doc, ".//w:tbl", ns=ns)
  
  lapply(tbls, function(tbl) {
    
    cells <- xml_find_all(tbl, "./w:tr/w:tc", ns=ns)
    rows <- xml_find_all(tbl, "./w:tr", ns=ns)
    dat <- data.frame(matrix(xml_text(cells), 
                             ncol=(length(cells)/length(rows)), 
                             byrow=TRUE), 
                      stringsAsFactors=FALSE)
    colnames(dat) <- dat[1,]
    dat <- dat[-1,]
    rownames(dat) <- NULL
    dat
    
  })
  
}

#create empty df
#df_all_id <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("id", "time_played_ALL_tot"))))
ALL_df <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("id", "time_played_ALL_tot"))))

#get list of number of Animega files
files_ALL <- dir("data/appdata/ALL", pattern = "docx", full.names = F)


for (i in seq_along(files_ALL)) {
  df_tot <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("id", "time_played_ALL")))) 
  id_df <- data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("id", "time_played_ALL"))))
  ALLfile <- paste0("data/appdata/ALL/",files_ALL[i])
  id <- as.numeric(gsub(".*?([0-9]+).*", "\\1", files_ALL[i]))
  #get tables from files
  file <- get_tbls(ALLfile)
  file <- file[-c(1, 2)]; # without 1st,2nd and 3rd table

  #Run to extract all tables from one ID and collect time points for first session start and session end. 
  for (i in seq_along(file)) {
    if(length(file[[i]]) != 0) {
      #Extract table
      one_table <- file[[i]]
      #add row with Session start and time (as that is stored as colum names and not variables)
      one_table <- rbind(one_table, c(colnames(one_table)[1],"", colnames(one_table)[3]))
      #remove empty column
      one_table <- one_table %>% select(1, 3)
      #rename columnname as it contains ":"
      names(one_table)[names(one_table) == "Sessionsstart:"] <- "Sessionsstart"
      names(one_table)[2] <- "Time"  
      #select time point for session start and end
      one_table_2 <- dplyr::filter(one_table, Sessionsstart == "Sessionsslut:" | Sessionsstart == "Sessionsstart:")
      one_table_2$Time = as.POSIXct(one_table_2$Time,format='%Y-%m-%d %H:%M:%S')
      one_table_2 <- mutate(one_table_2, Date = as.POSIXlt(as.Date(Time)))
      
      # För att rakna ut hur lång tid det är mellan två klick. Gör en column där varje värde laggar en från ovan för att sedan kunna räkna ut differensen. Differensen räknas sedan ut nedan i clicksdiff- OBS. Ett extremt högt clicksdiff beror på att det växlar mellan två deltagare som har helt olika timestamp, ex. 1970 - 2016 
      one_table_3 <- one_table_2 %>% 
        mutate(lagclicks=lag(Time))  %>% 
        mutate(clicksdiff= ifelse((lagclicks-Time) > 6000000, 0, lagclicks-Time)) #If the time is longer than 10 min, then set the time to 0 
      
      # Create variables
      time_played_ALL <- sum(one_table_3$clicksdiff, na.rm = T)
      days_played_ALL <- one_table_3$Date[1]
      # Join the variables to create a data frame
      temp_df <- data.frame(id,time_played_ALL, days_played_ALL)
      # add new data to id_df
      id_df <- rbind(id_df, temp_df)
      time_played_ALL_tot <- sum(id_df$time_played_ALL, na.rm = T)
      days_played_ALL <- sapply(id_df, function(x) length(unique(x)))["days_played_ALL"]
      df_tot <- cbind(id,time_played_ALL_tot, days_played_ALL)
      #df_all_id <- rbind(df_all_id, df_tot)
      
      } 
  }
  ALL_df <- rbind(ALL_df, df_tot)
}

app_data_df <- full_join(ALL_df, animega_df, by = "id")
app_data_df[is.na(app_data_df)] <- 0
app_data_df$time_played_animega <- app_data_df$time_played_animega/60
app_data_df <- app_data_df %>% mutate(tot_train_time = time_played_ALL_tot+time_played_animega) 

