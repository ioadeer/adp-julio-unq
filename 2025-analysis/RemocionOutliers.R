# Dependencias ------------------------------------------------------------
library(broom)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(Routliers)

# load data ---------------------------------------------------------------

rm(list=ls())
results_tbl <- read.csv('2025-analysis/ResultsData/results_log_bias.csv', header = TRUE, sep = ',', stringsAsFactors = TRUE)

# make figure -------------------------------------------------------------
f1 <- ggplot(results_tbl , aes(x= target_distance, y= perc_dist, color = condition)) +
  facet_grid(subject~.) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  geom_point(color="red", size=1) +
  geom_line()+
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)", breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,8)) +
  scale_y_continuous(name="Perceived distance (m)",breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,8)) +
  theme_linedraw(base_size = 9)

#plot(f1)

#figures_folder = "./2025-analysis/figures"
#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_poblacional.png", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f1, width=10, height=100, units="cm", limitsize=FALSE, dpi=300)


# Experi aula 27 ----------------------------------------------------------

tabla.outlier <- results_tbl %>% 
  filter(condition == 'Controlled environment') %>% 
  group_by(subject, condition, target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_unsigned_m,na.rm=TRUE))  %>%
  ungroup()


res3 <- outliers_mad(x = tabla.outlier$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)

# Agrego esto para evitar valores Inf
tabla.outlier$mBiasUnsigned[is.infinite(tabla.outlier$mBiasUnsigned)] <- NA

plot_outliers_mad(res3,x=tabla.outlier$mBiasUnsigned,pos_display=TRUE)

tabla.outlier[res3$outliers_pos,] 
# Sacamos 11 valores, los 6 del sujeto 2, td_1 del sujeto 9,
# 1 2 5 y 6 del sujeto 21
#subject condition              target_distance mBiasUnsigned
#<int> <fct>                            <int>         <dbl>
#1       2 Controlled environment               1         0.8  
#2       2 Controlled environment               2         0.985
#3       2 Controlled environment               3         0.889
#4       2 Controlled environment               4         0.913
#5       2 Controlled environment               5         0.849
#6       2 Controlled environment               6         0.613
#7       9 Controlled environment               1       Inf    
#8      21 Controlled environment               1       Inf    
#9      21 Controlled environment               2       Inf    
#10      21 Controlled environment               5         0.639
#11      21 Controlled environment               6         0.658

# EJEMPLO
#idx = results_tbl$subject == "31" & results_tbl$room_condition == 'Small VE' & 
#  results_tbl$target_distance == 2.7  
#results_tbl = results_tbl[!idx,]

# Volamos los 6 del sujeto 2, sacamos el sujeto 2
idx = results_tbl$subject == "2" & results_tbl$condition == 'Controlled environment' 

results_tbl = results_tbl[!idx,]

# target distance 1 del sujeto 9
idx = results_tbl$subject == "9" & results_tbl$condition == 'Controlled environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "21" & results_tbl$condition == 'Controlled environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "21" & results_tbl$condition == 'Controlled environment'  & 
  results_tbl$target_distance == 2  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "21" & results_tbl$condition == 'Controlled environment'  &  
  results_tbl$target_distance == 5
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "21" & results_tbl$condition == 'Controlled environment'  &  
  results_tbl$target_distance == 6
results_tbl = results_tbl[!idx,]


# Experi internet ---------------------------------------------------------

tabla.outlier <- results_tbl %>% 
  filter(condition == 'Unknown environment') %>% 
  group_by(subject, condition, target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_unsigned_m,na.rm=TRUE))  %>%
  ungroup()

res3 <- outliers_mad(x = tabla.outlier$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)

# Agrego esto para evitar valores Inf
tabla.outlier$mBiasUnsigned[is.infinite(tabla.outlier$mBiasUnsigned)] <- NA
plot_outliers_mad(res3,x=tabla.outlier$mBiasUnsigned,pos_display=TRUE)
tabla.outlier[res3$outliers_pos,] 

# Sacamos 11 valores
#subject condition           target_distance mBiasUnsigned
#<int> <fct>                         <int>         <dbl>
#1      40 Unknown environment               3         8.55 
#2      41 Unknown environment               1         1    
#3      61 Unknown environment               2         1.38 
#4      74 Unknown environment               1         1.35 
#5      77 Unknown environment               1         1    
#6      87 Unknown environment               1         1.35 
#7      87 Unknown environment               2         2.23 
#8      87 Unknown environment               3         1.21 
#9      87 Unknown environment               4         1.91 
#10      87 Unknown environment               5         1.39 
#11      92 Unknown environment               2         1.01  
#12      99 Unknown environment               1         1.41 
#13      99 Unknown environment               2         1.16 
#14      99 Unknown environment               3         1.36 
#15      99 Unknown environment               5         0.976
#16     101 Unknown environment               1       Inf    -
#17     101 Unknown environment               4       Inf    
#18     104 Unknown environment               1         1.00 
#19     107 Unknown environment               1       Inf    

#idx = results_tbl$subject == "2" & results_tbl$room_condition == 'Congruent VE' & 
#  results_tbl$target_distance == 2.7  
#results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "40" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 3  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "61" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 2  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "74" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "77" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "87" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "87" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 2  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "87" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 3  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "87" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 4  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "87" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 5  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "92" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 2  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "99" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "99" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 2  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "99" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 3  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "99" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 5  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "101" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "101" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 4  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "104" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

idx = results_tbl$subject == "107" & results_tbl$condition == 'Unknown environment'  & 
  results_tbl$target_distance == 1  
results_tbl = results_tbl[!idx,]

write_csv(x =results_tbl ,file ="2025-analysis/ResultsData/DResults-no-outliers.csv")
