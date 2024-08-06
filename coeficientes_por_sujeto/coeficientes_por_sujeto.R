# Aca voy a realizar extraccion y comparacion para coeficientes
# a k y R^2 por sujeto.
# Luego hacer figuras con histograma de cada uno de los valores.
# Analisis t-test entre condiciones y ver si es necesaria la correccion
# de bonferroni dado que tenemos solo 2 grupos

# Dependencias ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(quickpsy)
library(tidyverse)
library(lme4)
library(nlme)
library(lmerTest)
library(broom)
library(broom.mixed)
library(ggpubr)
library(Routliers)
library(ggbeeswarm)
library(ggthemes)
library(ggstatsplot)
library(gmodels)
library(pracma)
library(Routliers)
library(rstatix)




# load data ---------------------------------------------------------------

tabla.raw <- read.csv('ResultsData/Dresults.csv', header = TRUE, sep = ',', stringsAsFactors = TRUE)

# Preparacion de datos -----------  
# https://cran.r-project.org/web/packages/broom/vignettes/broom.html
#  https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html

tabla.ind <- tabla.raw %>%
  mutate(
    log_target_distance = log(target_distance),
    log_perc_dist = log(perc_dist),
  )

# modelo_sujeto  <- function(df) {
#   lmer(log_respuesta ~ log_distancia + (1|nsub) , data = df)
# }

modelo_sujeto  <- function(df) {
  lm(log_perc_dist ~ log_target_distance, data = df)
}



# Controlled environment (aula 27) ----------------------------------------

tabla.ind.controlled <- tabla.ind %>%
  filter(room_condition == 'Controlled environment')

regressions.controlled  <- tabla.ind.controlled  %>%
  nest(data =  -subject) %>%
  mutate(
    fit = map(data,modelo_sujeto),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

tidied.controlled <- regressions.controlled %>%
  unnest(tidied)

regressions.controlled %>%
  unnest(glanced)

tidied.controlled %>%
  unnest(augmented)

#
# CORRECCION Guardarse el R^2 tambien

r_sqrd.controlled <- regressions.controlled %>%
  unnest(glanced) %>%
  group_by(subject) %>%
  select(subject, r.squared)

#
coefs.controlled <- regressions.controlled %>%
  unnest(tidied) %>%
  group_by(subject) %>%
  spread(term,estimate) %>%
  select(subject,"(Intercept)", log_target_distance) %>%
  rename(intercept = "(Intercept)") %>%
  group_by(subject) %>%
  summarise(
    intercept = max(intercept, na.rm =T),
    coef = max(log_target_distance, na.rm =T),
  )

coefs.controlled <- merge(x=coefs.controlled, y=r_sqrd.controlled, by='subject')

coefs.controlled <- coefs.controlled %>%
  mutate(room_condition = "Controlled environment")

#
# CORRECCION 2 Guardarse residuos

# VISUAL

# Unknown environment (INTERNET) ----------------------------------------------------------

tabla.ind.unknown <- tabla.ind %>%
  filter(room_condition == 'Unknown environment')

regressions.unknown <- tabla.ind.unknown %>%
  nest(data =  -subject) %>%
  mutate(
    fit = map(data,modelo_sujeto),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

tidied.unknown <- regressions.unknown %>%
  unnest(tidied)

regressions.unknown %>%
  unnest(glanced)

regressions.unknown %>%
  unnest(augmented)

#
# CORRECCION Guardarse el R^2 tambien

r_sqrd.unknown <- regressions.unknown %>%
  unnest(glanced) %>%
  group_by(subject) %>%
  select(subject, r.squared)

#
coefs.unknown <- regressions.unknown %>%
  unnest(tidied) %>%
  group_by(subject) %>%
  spread(term,estimate) %>%
  select(subject,"(Intercept)", log_target_distance) %>%
  rename(intercept = "(Intercept)") %>%
  group_by(subject) %>%
  summarise(
    intercept = max(intercept, na.rm =T),
    coef = max(log_target_distance, na.rm =T),
  )

coefs.unknown <- merge(x=coefs.unknown, y=r_sqrd.unknown, by='subject')

coefs.unknown <- coefs.unknown %>%
  mutate(room_condition = "Unknown environment")


coefs.all <- rbind(coefs.unknown, coefs.controlled)



# save data ---------------------------------------------------------------


write.table(coefs.all, file="./data/coeficientes_por_sujeto_sin_outliers.csv", row.names = FALSE)



# Residuals ---------------------------------------------------------------

# CORRECCION 2 Guardarse residuos
residuos.controlled <- regressions.controlled %>%
  unnest(augmented) %>%
  select(-c(fit, tidied, data, glanced))

residuos.controlled['room_condition'] = 'Controlled environment'

residuos.unknown <- regressions.unknown %>%
  unnest(augmented) %>%
  select(-c(fit, tidied, data, glanced))

residuos.unknown['room_condition'] = 'Unknown environment'

residuos.all <- rbind(residuos.controlled, residuos.unknown)

write.table(residuos.all, file="data/coeficientes_fitted_model_residuals_sin_outliers.csv", row.names = FALSE)

# unnest(glanced) %>%
# group_by(nsub) %>%
# select(nsub, r.squared)
