# Analisis de N50 de ADP
# Intra y entre

# dependencies ------------------------------------------------------------


library(tidyverse)
library(Routliers)
library(lme4)
library(nlme)
library(sjPlot)
library(MuMIn)
library(lmerTest)
library(jtools)
library(gdtools)
library(broom)
library(ggstatsplot)
library(modelsummary)
library(ggpubr)
library(flextable)
library(webshot)
library(officer)
library(effects)

rm(list=ls())

results_tbl <- read.csv("../ResultsData/Dresults.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

tabla.raw <- read.csv("../ResultsData/bambu.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)
tabla.raw$Distancia = tabla.raw$Dist_fis
tabla.raw = select(tabla.raw, Trial, Respuesta, Sujeto, Bloque, Distancia )
# tabla.bambu$SesgoRel <- (tabla.bambu$Respuesta - tabla.bambu$Distancia) / tabla.bambu$Distancia
# tabla.bambu$SesgoAbs <-  abs(tabla.bambu$SesgoRel)

tabla.raw$signedbias <- (tabla.raw$Respuesta - tabla.raw$Distancia) / tabla.raw$Distancia
tabla.raw$unsignedbias <-  abs(tabla.raw$signedbias)

tabla.ind = tabla.raw %>%
  group_by(Sujeto,Distancia,Bloque) %>%
  summarise(respuesta_ind = mean(Respuesta),
            sd_respuesta_ind = sd(Respuesta),
            sem_respuesta_ind = sd(Respuesta)/sqrt(n()))


tabla.ind = tabla.ind %>% rename(perc_dist = respuesta_ind)
tabla.ind = tabla.ind %>% rename(target_distance = Distancia)
tabla.ind = tabla.ind %>% rename(subject = Sujeto)
tabla.ind = tabla.ind %>% rename(room_condition = Bloque)
tabla.ind$condition = factor(tabla.ind$room_condition, levels = c("VIRTUAL",
                                                             "INDVIRTUAL",
                                                             "REAL",
                                                             "SIMULADO",
                                                             "ABIERTO",
                                                             "CERRADO"))
levels(tabla.ind$room_condition) = c("Non-individualized HRTF",
                                "Individualized HRTF",
                                "Real speakers",
                                "Simulated HRTF",
                                "Incongruous room",
                                "Closed headphones")

tabla.ind = tabla.ind %>% rename(sd_perc_dist = sd_respuesta_ind)
tabla.ind = tabla.ind %>% rename(sem_perc_dist = sem_respuesta_ind)

results_tbl_bambu = tabla.ind
results_tbl_bambu$logperc_dist = log10(results_tbl_bambu$perc_dist)
results_tbl_bambu = filter(results_tbl_bambu, room_condition == "Non-individualized HRTF")
rm("tabla.ind","tabla.bambu","tabla.raw")


results_tbl = merge(results_tbl, results_tbl_bambu, all=TRUE)




tabla.ind_sesgo = tabla.raw %>%
  group_by(Sujeto,Bloque) %>%
  summarise(signed_bias_ind = mean(signedbias),
            sd_signed_bias_ind = sd(signedbias),
            sem_signed_bias_ind = sd(signedbias)/sqrt(n()),
            unsigned_bias_ind = abs(mean(signedbias)),
            sd_unsigned_bias_ind = abs(sd(signedbias)),
            sem_unsigned_bias_ind = abs(sd(signedbias)/sqrt(n())))


tabla.ind_sesgo = tabla.ind_sesgo %>% rename(subject = Sujeto)
tabla.ind_sesgo = tabla.ind_sesgo %>% rename(condition = Bloque)
tabla.ind_sesgo$condition = factor(tabla.ind_sesgo$condition, levels = c("VIRTUAL",
                                                                         "INDVIRTUAL",
                                                                         "REAL",
                                                                         "SIMULADO",
                                                                         "ABIERTO",
                                                                         "CERRADO"))
levels(tabla.ind_sesgo$condition) = c("Non-individualized HRTF",
                                      "Individualized HRTF",
                                      "Real speakers",
                                      "Simulated HRTF",
                                      "Incongruous room",
                                      "Closed headphones")


results_tbl_bias = tabla.ind_sesgo

rm("tabla.ind_sesgo")





cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

# Primero INTRA SUJETOS
# Primer modelo
m.Dist1 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
                 data = results_tbl) 


extract_stats(ggcoefstats(m.Dist1))
r.squaredGLMM(m.Dist1)

anova(m.Dist1)
anov1 = anova(m.Dist1)

results_tbl$Modelfitted1<-predict(m.Dist1)

# Todos los sujetos
FittedlmPlot1 <-ggplot()+
  facet_grid(subject ~ room_condition, labeller=label_both)+
  geom_line(data = results_tbl, aes(x = target_distance, y = 10^Modelfitted1))+
  geom_point(data = results_tbl, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
  #  coord_cartesian(ylim = c(.03,.074))+ 
  xlab("Targent_distance")+ylab("Perceived_distance")
FittedlmPlot1


Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist1)

# Grafico poblacional
Final.Fixed<-as.data.frame(Final.Fixed)

Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition))+
  # coord_cartesian(xlim=c(0,4),ylim = c(0,.7))+ 
  geom_line(aes(color=room_condition), size=2)+
  geom_ribbon(aes(ymin=10^fit-10^se, ymax=10^fit+10^se,fill=room_condition),alpha=.2)+
  xlab("Target_distance")+
  ylab("Perceived_distance")+
  # scale_color_manual(values=c("blue", "red"))+
  # scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+
  theme(text=element_text(face="bold", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.title=element_blank(),
        legend.position = c(.2, .92))
Final.Fixed.Plot

mDist1stats <- extract_stats(ggcoefstats(m.Dist1))
mDist1stats$tidy_data
anova(m.Dist1)
r.squaredGLMM(m.Dist1)

eq1 <- substitute("Controlled environment:" ~~~ italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^mDist1stats$tidy_data$estimate[[1]],digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]], digits = 2)))
eq2 <- substitute("Unknown environment:"~~~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^(mDist1stats$tidy_data$estimate[[1]]+mDist1stats$tidy_data$estimate[[3]]), digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]]+mDist1stats$tidy_data$estimate[[5]], digits = 2)))
eq3 <- substitute("Real environment:"~~~italic(y) == k %.% italic(X)^italic(a),
                  list(k = round(10^(mDist1stats$tidy_data$estimate[[1]]+mDist1stats$tidy_data$estimate[[4]]), digits = 2),
                       a = round(mDist1stats$tidy_data$estimate[[2]]+mDist1stats$tidy_data$estimate[[6]], digits = 2)))
#eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b),
#                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))
eq1

results_tbl <- results_tbl %>%
  mutate(
    perc_dist_log_10 = log10(perc_dist)
  )

tabla.pob <- results_tbl %>% group_by(target_distance,room_condition) %>%
  summarise(Mperc_dist  = mean(perc_dist_log_10),
            SDperc_dist = sd(perc_dist_log_10)/sqrt(n()))  %>%
  ungroup()

f1 <- ggplot(tabla.pob, aes(x=target_distance, y =10^Mperc_dist, group = room_condition, color  = room_condition)) +
  geom_pointrange(aes(x = target_distance, y = 10^Mperc_dist, ymin = 10^(Mperc_dist-SDperc_dist), ymax = 10^(Mperc_dist+SDperc_dist)),alpha = 1,
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) +
  scale_fill_manual(values = cbPalette) +
  geom_line(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition, color=room_condition))+
  geom_text(x = 0.2, y = 8.0, label = as.character(as.expression(eq1)), 
            hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000",
            family="Times New Roman")+
  geom_text(x = 0.2, y = 7.0, label = as.character(as.expression(eq2)), 
            hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00",
            family="Times New Roman")+
  geom_text(x = 0.2, y = 6.0, label = as.character(as.expression(eq3)), 
            hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#009E73",
            family="Times New Roman")+
  #geom_text(x = 0.2, y = 6.0, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,10)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,10)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        text=element_text(family="Times New Roman", size=10)) 


f1


##Analisis Post hoc de slope


coefmodelpob = fixef(m.Dist1)
coefmodelind = ranef(m.Dist1)
a = levels(m.Dist1@frame$subject)
b = coefmodelind$subject[[2]]
table.slope = data.frame(a,b)
table.model.data = m.Dist1@frame
table.model.data = table.model.data %>% group_by(room_condition,subject)%>%
  summarise(Mperc_dist  = mean(`log10(target_distance)`)) %>%
  ungroup()

table.slope = table.slope %>% rename(subject = a)
table.slope = table.slope %>% rename(slope = b)
table.slope = merge(table.model.data, table.slope, all=TRUE)
table.slope <- table.slope %>%
  select(-c(Mperc_dist))

idx = table.slope$room_condition == "Controlled environment"
table.slope[idx,]$slope = table.slope[idx,]$slope + coefmodelpob[[2]] 

idx = table.slope$room_condition == "Unknown environment"
table.slope[idx,]$slope = table.slope[idx,]$slope + coefmodelpob[[2]] +coefmodelpob[[5]]

idx = table.slope$room_condition == "Non-individualized HRTF"
table.slope[idx,]$slope = table.slope[idx,]$slope + coefmodelpob[[2]] +coefmodelpob[[6]]

t.test(filter(table.slope, 
              room_condition=="Unknown environment")$slope,
       filter(table.slope, 
              room_condition=="Non-individualized HRTF")$slope, 
       paired = FALSE)



results_tblp <- table.slope %>% 
  group_by(room_condition) %>%
  summarise(mslope  = mean(slope,na.rm=TRUE),
            SDslope  = sd(slope,na.rm=TRUE)/sqrt(length(slope)))  %>%
  ungroup()

f2 =  ggplot(table.slope, aes(x = room_condition,y = slope,colour = room_condition)) +
  geom_point()+
  # geom_pointrange(aes(x=room_condition, y=mslope, ymin=mslope-SDslope, ymax=mslope+SDslope),
  #                 position = position_dodgenudge(direction = "split", width = 3.2), size = 1.2, shape = 0)+
  # geom_line(position = position_dodgenudge(direction = "split", width = 3),size = 1.2, alpha=.5)+
  # geom_line(data = results_tbl, mapping = aes(x = room_condition,y = slope, group = subject, colour = room_condition, fill = room_condition),alpha = 0.6)+
  # geom_point(data = results_tbl, mapping = aes(x = room_condition,y = slope, colour = room_condition, fill = room_condition), size = 2.4,alpha = 1)+
  # geom_violin(data = filter(results_tbl,type == "NORMAL"), mapping = aes(x = room_condition,y = slope, colour = room_condition, fill = room_condition),
  #                  position = position_nudge_center(x = .3,
  #                    y = 0,
  #                    center_x = 2,
  #                    direction = "split",
  #                    kept.origin = c("original", "none")),trim = FALSE, alpha = .2)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "room_condition", 
       y = "Slope with LM") +
  # facet_grid(. ~ type) +
  annotate("text", x = 1.5, y = 2,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 2, y = 1.9, yend = 1.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")
f2




# signed bias -------------------------------------------------------------

results_tbls <- results_tbl %>% 
  group_by(room_condition,subject) %>%
  summarise(mBiasSigned  = mean(rel_bias_signed,na.rm=TRUE),
            SdBiasSigned  = sd(rel_bias_signed,na.rm=TRUE)/sqrt(length(rel_bias_signed)),
            mBiasUnSigned  = mean(rel_bias_unsigned,na.rm=TRUE),
            SdBiasUnSigned  = sd(rel_bias_unsigned,na.rm=TRUE)/sqrt(length(rel_bias_unsigned)))  %>%
  ungroup()

results_tblp <- results_tbls %>% 
  group_by(room_condition) %>%
  summarise(MBiasSigned  = mean(mBiasSigned,na.rm=TRUE),
            SDBiasSigned  = sd(mBiasSigned,na.rm=TRUE)/sqrt(length(mBiasSigned)),
            MBiasUnSigned  = mean(mBiasUnSigned,na.rm=TRUE),
            SDBiasUnSigned  = sd(mBiasUnSigned,na.rm=TRUE)/sqrt(length(mBiasUnSigned)))  %>%
  ungroup()
# geom_flat_violin(aes(fill = Condicion),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
#   geom_point(aes(x = as.numeric(Tiempo)-.15, y = LeqAS, colour = Condicion),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+

f6 <-  ggplot(results_tblp, aes(x = room_condition,y = MBiasSigned, colour = room_condition, fill = room_condition)) +
  geom_pointrange(aes(x=room_condition, y=MBiasSigned, ymin=MBiasSigned-SDBiasSigned, ymax=MBiasSigned+SDBiasSigned), size = 0.5)+
  geom_line(aes(group = 1),size = 1.2, alpha=.5)+
  geom_point(data = results_tbls, mapping = aes(x = room_condition,y = mBiasSigned, colour = room_condition, fill = room_condition), alpha = 0.3)+
  geom_line(data = results_tbls, mapping = aes(x = room_condition,y = mBiasSigned, group = subject, colour = room_condition),alpha = 0.3)+
  geom_violin(data= results_tbls,aes(x = room_condition,y = mBiasSigned), trim=TRUE, alpha=0)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Relative signed \nbias") +
  # facet_grid(. ~ type) +
  #annotate("text", x = 1.5, y = 0.3,  label = "*", size = 4) +
  #annotate("segment", x = 1, xend = 2, y = 0.2, yend = 0.2, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

f6


m.RelativBias <- lm(mBiasSigned ~ room_condition, 
                    data = results_tbls)
extract_stats(ggcoefstats(m.RelativBias))
#1 (Intercept)                        -0.509
# room_conditionVisual information    0.151 

anov = anova(m.RelativBias)
anov
f6

# no es signficitavo #1 0.18301 0.183011  3.5967 0.06756 .
#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "13. Bias signed", ".png", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f6, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

# NO ES PAREADO
testSigendBias <- t.test(filter(results_tbls, 
                                room_condition=="Controlled environment" )$mBiasSigned,
                         filter(results_tbls, 
                                room_condition=="Unknown environment")$mBiasSigned, 
                         paired = FALSE)

testSigendBias

#95 percent confidence interval:
#  -0.31498920  0.01249011
#sample estimates:
#  mean of x  mean of y 
#-0.5090052 -0.3577557 


# unsigned bias -----------------------------------------------------------

f7 =  ggplot(results_tblp, aes(x = room_condition,y = MBiasUnSigned, colour = room_condition, fill = room_condition)) +
  geom_pointrange(aes(x=room_condition, y=MBiasUnSigned, ymin=MBiasUnSigned-SDBiasUnSigned, ymax=MBiasUnSigned+SDBiasUnSigned), size = 0.5)+
  geom_point(data = results_tbls, mapping = aes(x = room_condition,y = mBiasUnSigned, colour = room_condition, fill = room_condition), alpha = .3)+
  geom_line(aes(group = 1),size = 1.2, alpha=.5)+
  geom_line(data = results_tbls, mapping = aes(x = room_condition,y = mBiasUnSigned, group = subject, colour = room_condition),alpha = 0.3)+
  geom_violin(data= results_tbls,aes(x = room_condition,y = mBiasUnSigned), trim=TRUE, alpha=0)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Relative unsigned \nbias") +
  # facet_grid(. ~ type) +
  #annotate("text", x = 1.5, y = 1.1,  label = "**", size = 4) +
  #annotate("segment", x = 1, xend = 2, y = 1.0, yend = 1.0, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

f7

m.RelativUnsignedBias <- lm(mBiasUnSigned ~ room_condition, 
                            data = results_tbls)
extract_stats(ggcoefstats(m.RelativUnsignedBias))
anov = anova(m.RelativUnsignedBias)
anov
f7
#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "13. Bias signed", ".png", sep = '')
#ggsave(mi_nombre_de_archivo, plot=f6, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

t.test(filter(results_tbls, 
              room_condition=="Controlled environment" )$mBiasUnSigned,
       filter(results_tbls, 
              room_condition=="Unknown environment")$mBiasUnSigned, 
       paired = FALSE)






# main plot ---------------------------------------------------------------

#f1 f6 y f7
main_figure <- ggarrange(f1, 
                         ggarrange(f6, f7, widths = c(2,2),
                                   ncol = 2, labels = c("B", "C")),
                         nrow = 2, 
                         labels ="A",
                         heights = c(1, 0.75),
                         common.legend = TRUE)
#                    legend = "top")

main_figure

#plot<- ggarrange(ba,mi,fa, ncol=3, nrow=1, common.legend = TRUE,legend="bottom")

main_figure <- annotate_figure(main_figure, top = text_grob("Experiment controlled vs unknwon environment", 
                                      color = "black", face = "bold", size = 12))

# save plot ---------------------------------------------------------------

figures_folder = "./Figuras/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "All", ".png", sep = '')
ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=15, height=15)
plot(main_figure)
dev.off()
s