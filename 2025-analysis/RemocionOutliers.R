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

plot(f1)

figures_folder = "./2025-analysis/figures"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_poblacional.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f1, width=10, height=100, units="cm", limitsize=FALSE, dpi=300)


# remocion ----------------------------------------------------------------

tabla.outlier <- results_tbl %>% 
  filter(condition == 'Controlled environment') %>% 
  group_by(subject, condition, target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_unsigned_m,na.rm=TRUE))  %>%
  ungroup()


res3 <- outliers_mad(x = tabla.outlier$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)

plot_outliers_mad(res3,x=tabla.outlier$mBiasUnsigned,pos_display=TRUE)

tabla.outlier[res3$outliers_pos,] 

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

#idx = results_tbl$subject == "31" & results_tbl$room_condition == 'Small VE' & 
#  results_tbl$target_distance == 2.7  
#results_tbl = results_tbl[!idx,]


tabla.outlier <- results_tbl %>% 
  filter(room_condition == 'Congruent VE') %>% 
  group_by(subject, room_condition, target_distance) %>%
  summarise(mBiasUnsigned  = mean(log_bias_unsigned_m,na.rm=TRUE))  %>%
  ungroup()
res3 <- outliers_mad(x = tabla.outlier$mBiasUnsigned,threshold = 3 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.outlier$mBiasUnsigned,pos_display=TRUE)
tabla.outlier[res3$outliers_pos,] 

#idx = results_tbl$subject == "2" & results_tbl$room_condition == 'Congruent VE' & 
#  results_tbl$target_distance == 2.7  
#results_tbl = results_tbl[!idx,]

write_csv(x =results_tbl ,file ="./Experiment_1/Exp_1_ADP/ResultsData/Dresults_nuevos.csv")
