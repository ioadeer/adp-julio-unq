library(dplyr)
library(tidyverse)

filenames <- list.files(
  path = "2025-all-data/data/csv/processed/",
  pattern = "*.csv")

headers <- names(read_csv("2025-all-data/data/csv/processed/001.csv"))

data_participants_tbl <- do.call(rbind, 
                                 lapply(
                                   paste0("./2025-all-data/data/csv/processed/",
                                          filenames),
                                   col_names = headers,
                                   read_csv, 
                                   skip = 13, 
                                   n_max = 30,
                                   col_select = c("participant_n", 
                                                  "participant",
                                                  "trials.thisN",
                                                  "trials.thisIndex",
                                                  "typedWord",
                                                  "date"
                                                  ),
                                 ))

data_participants_tbl <- data_participants_tbl %>%
  mutate(subject = participant_n)    %>%
  mutate(subject_name = participant)    %>%
  mutate(trial = trials.thisN+1) %>%
  mutate(target_distance = case_when(
    trials.thisIndex == 0 ~ 1,
    trials.thisIndex == 1 ~ 2,
    trials.thisIndex == 2 ~ 3,
    trials.thisIndex == 3 ~ 4,
    trials.thisIndex == 4 ~ 5,
    trials.thisIndex == 5 ~ 6,
  ))  %>%
  mutate(perc_distance = as.numeric(gsub(",", ".", typedWord))) 

data_participants_tbl <-
  subset(data_participants_tbl, select = c(subject,subject_name,trial,target_distance,perc_distance, date))

write.table(data_participants_tbl, file="./2025-all-data/data/data-internet.csv", row.names = FALSE)
