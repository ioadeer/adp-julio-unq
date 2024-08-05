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

# Extraccion de coeficientes con broom  -----------  
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
  group_by(nsub) %>%
  summarise(
    intercept = max(intercept, na.rm =T),
    coef = max(log_distancia, na.rm =T),
  )

coefs.oscuras <- merge(x=coefs.oscuras, y=r_sqrd.oscuras, by='nsub')

coefs.oscuras <- coefs.oscuras %>%
  mutate(condicion_sala = "OSCURAS")

tabla.ind.oscuras <- tabla.ind %>%
  filter(condicion_sala == 'OSCURAS')

regressions.oscuras <- tabla.ind.oscuras %>%
  nest(data =  -nsub) %>%
  mutate(
    fit = map(data,modelo_sujeto),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

tidied.oscuras <- regressions.oscuras %>%
  unnest(tidied)

regressions.oscuras %>%
  unnest(glanced)

tidied.oscuras %>%
  unnest(augmented)

#
# CORRECCION Guardarse el R^2 tambien

r_sqrd.oscuras <- regressions.oscuras %>%
  unnest(glanced) %>%
  group_by(nsub) %>%
  select(nsub, r.squared)

#
# CORRECCION 2 Guardarse residuos
# Esta al final

#
coefs.oscuras <- regressions.oscuras %>%
  unnest(tidied) %>%
  group_by(nsub) %>%
  spread(term,estimate) %>%
  select(nsub,"(Intercept)", log_distancia) %>%
  rename(intercept = "(Intercept)") %>%
  group_by(nsub) %>%
  summarise(
    intercept = max(intercept, na.rm =T),
    coef = max(log_distancia, na.rm =T),
  )

coefs.oscuras <- merge(x=coefs.oscuras, y=r_sqrd.oscuras, by='nsub')

coefs.oscuras <- coefs.oscuras %>%
  mutate(condicion_sala = "OSCURAS")

# VISUAL

tabla.ind.visual <- tabla.ind %>%
  filter(condicion_sala == 'VISUAL')

regressions.visual <- tabla.ind.visual %>%
  nest(data =  -nsub) %>%
  mutate(
    fit = map(data,modelo_sujeto),
    tidied = map(fit, tidy),
    glanced = map(fit, glance),
    augmented = map(fit, augment)
  )

tidied.visual <- regressions.visual %>%
  unnest(tidied)

regressions.visual %>%
  unnest(glanced)

tidied.visual %>%
  unnest(augmented)

#
# CORRECCION Guardarse el R^2 tambien

r_sqrd.visual <- regressions.visual %>%
  unnest(glanced) %>%
  group_by(nsub) %>%
  select(nsub, r.squared)

#
coefs.visual <- regressions.visual %>%
  unnest(tidied) %>%
  group_by(nsub) %>%
  spread(term,estimate) %>%
  select(nsub,"(Intercept)", log_distancia) %>%
  rename(intercept = "(Intercept)") %>%
  group_by(nsub) %>%
  summarise(
    intercept = max(intercept, na.rm =T),
    coef = max(log_distancia, na.rm =T),
  )

coefs.visual <- merge(x=coefs.visual, y=r_sqrd.visual, by='nsub')

coefs.visual <- coefs.visual %>%
  mutate(condicion_sala = "VISUAL")


coefs.all <- rbind(coefs.oscuras, coefs.visual)

write.table(coefs.all, file="analisis_control/data/coeficientes_por_sujeto_control.csv", row.names = FALSE)


#
# CORRECCION 2 Guardarse residuos
residuos.oscuras <- regressions.oscuras %>%
  unnest(augmented) %>%
  select(-c(fit, tidied, data, glanced))

residuos.oscuras['condicion_sala'] = 'OSCURAS'

residuos.visual <- regressions.visual %>%
  unnest(augmented) %>%
  select(-c(fit, tidied, data, glanced))

residuos.visual['condicion_sala'] = 'VISUAL'

residuos.all <- rbind(residuos.oscuras, residuos.visual)

write.table(residuos.all, file="analisis_control/data/fitted_model_residuals.csv", row.names = FALSE)

# unnest(glanced) %>%
# group_by(nsub) %>%
# select(nsub, r.squared)
