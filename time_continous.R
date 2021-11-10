library(readr)
library(reader)


time_continous <- import("data/time_continous.xlsx")

library(tidyr)
time_continous$Kod <- factor(time_continous$Kod)
data_long <- gather(time_continous, time, days, t1_dag:t4_dag, factor_key=TRUE)


