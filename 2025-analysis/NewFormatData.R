# Experiment Controlled Environment vs Unknown Environment

# Dependencias ------------------------------------------------------------
library(broom)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggstatsplot)
library(gridExtra)
library(htmlwidgets)
library(tidyverse)
library(lme4)
library(nlme)
library(lmerTest)
library(modelr)
library(scales) 
library(plotly)
library(Routliers)
library(processx)
library(effects)




# Format data -------------------------------------------------------------

rm(list=ls())
tabla.raw <- read.csv('2025-analysis/data/data-internet-sala.csv', header = TRUE, sep = ',', stringsAsFactors = TRUE)

tabla.raw$abs_bias <-  abs(tabla.raw$perc_distance - tabla.raw$target_distance)
tabla.raw$log_bias <-  log10(tabla.raw$perc_distance/tabla.raw$target_distance)

# signed bias
tabla.raw$signed_bias <- (tabla.raw$perc_distance - tabla.raw$target_distance) / tabla.raw$target_distance
# unsigen bias
tabla.raw$unsigned_bias <- abs(tabla.raw$signed_bias)

#  unsigned log bias
tabla.raw$log_bias_unsigned <- abs(tabla.raw$log_bias)

f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

results_tbl <- tibble(aggregate(cbind(perc_distance,signed_bias,unsigned_bias,abs_bias, log_bias, log_bias_unsigned) ~ 
                                  subject*condition*target_distance,
                                data = tabla.raw,
                                FUN  = f_promedio,na.action = NULL))


results_tbl %>%
  # clean_names() %>%
  mutate(subject = factor(subject),
         condition = factor(condition),
         
         perc_dist_sd =  perc_distance[,"sd"],
         perc_dist_sem = perc_distance[,"sem"],
         perc_dist_var = perc_distance[,"var"],
         perc_dist_n =   perc_distance[,"n"],
         perc_dist =     perc_distance[,"mean"],
         
         rel_bias_signed_sd = signed_bias[,"sd"],
         rel_bias_signed_sem = signed_bias[,"sem"],
         rel_bias_signed_var = signed_bias[,"var"],
         rel_bias_signed_n = signed_bias[,"n"],
         rel_bias_signed = signed_bias[,"mean"],
         
         rel_bias_unsigned_sd = unsigned_bias[,"sd"],
         rel_bias_unsigned_sem = unsigned_bias[,"sem"],
         rel_bias_unsigned_var = unsigned_bias[,"var"],
         rel_bias_unsigned_n = unsigned_bias[,"n"],
         rel_bias_unsigned = unsigned_bias[,"mean"],
         
         abs_bias_sd = abs_bias[,"sd"],
         abs_bias_sem = abs_bias[,"sem"],
         abs_bias_var = abs_bias[,"var"],
         abs_bias_n = abs_bias[,"n"],
         abs_bias = abs_bias[,"mean"],
         
         log_bias_sd = log_bias[,"sd"],
         log_bias_sem = log_bias[,"sem"],
         log_bias_var = log_bias[,"var"],
         log_bias_n = log_bias[,"n"],
         log_bias_m = log_bias[,"mean"],
         
         
         log_bias_unsigned_sd = log_bias_unsigned[,"sd"],
         log_bias_unsigned_sem = log_bias_unsigned[,"sem"],
         log_bias_unsigned_var = log_bias_unsigned[,"var"],
         log_bias_unsigned_n = log_bias_unsigned[,"n"],
         log_bias_unsigned_m = log_bias_unsigned[,"mean"]) %>%
  
  select(-c(perc_distance,signed_bias,unsigned_bias,abs_bias,log_bias,log_bias_unsigned ))  %>%
  
  # descomentar para escirbir nuevos dataset (primerop sacar outliers)
  write_csv("2025-analysis/ResultsData/results_log_bias.csv")
