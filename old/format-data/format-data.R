library(dplyr)
library(tidyverse)

filenames <- list.files(
                         path = "./data/por-participante/",
                         pattern = "*.csv")

headers <- names(read_csv("./data/por-participante/001.csv"))

data_participants_tbl <- do.call(rbind, 
                           lapply(
                             paste0("./data/por-participante/",filenames),
                             col_names = headers,
                             read_csv, 
                             skip = 10, 
                             n_max = 30,
                             col_select = c("participant","trials.thisN","trials.thisIndex","typedWord"),
                             ))

data_participants_tbl <- data_participants_tbl %>%
  mutate(participant = case_when(participant == "s002" ~ "002",
                                 participant != "s002" ~ participant)) %>%
  mutate(sujeto = paste0("S", participant))    %>%
  mutate(trial = trials.thisN+1) %>%
  mutate(distancia = case_when(
      trials.thisIndex == 0 ~ 1,
      trials.thisIndex == 1 ~ 2,
      trials.thisIndex == 2 ~ 3,
      trials.thisIndex == 3 ~ 4,
      trials.thisIndex == 4 ~ 5,
      trials.thisIndex == 5 ~ 6,
    ))  %>%
  mutate(respuesta = as.numeric(gsub(",", ".", typedWord))) 

data_participants_tbl <-
  subset(data_participants_tbl, select = c(sujeto,trial,distancia,respuesta))
  
write.table(data_participants_tbl, file="./data/datos.csv", row.names = FALSE)
