# este script esta hecho para formatear datos poblacionales de aula 27
# e internet

library(tidyverse)
library(dplyr)


# aula 27 -----------------------------------------------------------------

data.aula27 <- read.csv("./data/poblacional/aula-27-poblacional.csv")
colnames(data.aula27)

data.aula27 <- data.aula27 %>% 
  rename(
  subject = sujeto,
  age = edad,
  sex = sexo
  ) %>% 
  select(c("subject", "age", "sex")) %>%
  mutate(room_condition = "Controlled environment") %>%
  write_csv("data/poblacional/aula_27_procesado_con_outliers.csv")


# internet data -----------------------------------------------------------

data.internet <- read.csv("./data/poblacional/internet-poblacional.csv")
colnames(data.internet)

data.internet <- data.internet %>% 
  rename(
    name = Nombre.y.Apellido.,
    time = Marca.temporal,
    age = Edad.,
    sex = Género.,
    headphone_type = X.A.qué.tipología.corresponde.el.tipo.de.auriculares.utilizados.para.realizar.el.experimento..,
    headphone_brand = Si.conoce.la.marca.y.modelo.de.los.auriculares..por.favor.indíquelo.,
    musical_studies = X.Estudió.música...,
    hearing_issues = X.Presenta.algún.problema.de.audición.diagnosticado.
  ) %>% 
  select(c("time", "name","age", "sex", "headphone_type", "headphone_brand", "musical_studies", "hearing_issues")) %>%
  mutate(room_condition = "Unknown environment") %>%
  write_csv("data/poblacional/interntet_procesado_con_outliers.csv")
