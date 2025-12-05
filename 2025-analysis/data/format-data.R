library(tidyverse)
library(dplyr)

data_sala.tbl <- read.csv(file='2025-analysis/data/data-sala.csv', sep = ' ')

data_sala.tbl <- data_sala.tbl %>%
  rename(subject = sujeto) %>%
  rename(target_distance = distancia) %>%
  rename(perc_distance = respuesta) %>%
  mutate(subject = sub("^S", "", subject)) %>%
  mutate(subject = as.numeric(subject)) %>%
  mutate(condition = "Controlled environment")

data_internet.tbl <- read.csv(file='./2025-analysis/data/data-internet.csv', sep = ' ')

data_internet.tbl <- data_internet.tbl %>%
  select(c(subject, trial, target_distance, perc_distance)) %>%
  mutate(subject = subject+ 30 ) %>% # El n de sujeto va a comenzar a ser contado desde el experi de sala 27
  mutate(condition = "Unknown environment") 

data_adp <- rbind(data_sala.tbl, data_internet.tbl)

write.csv(data_adp, file = "./2025-analysis/data/data-internet-sala.csv", row.names = FALSE)
