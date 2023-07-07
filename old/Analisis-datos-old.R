
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


# Load data  OLD---------------------------------------------------------------

# Datos de participantes de internet
tabla.internet = read.csv("./data/datos-de-internet-1.csv", header = TRUE, sep = '', stringsAsFactors = TRUE)
# sacar sujeto 5 7 29 37
tabla.internet <- tabla.raw %>% filter(!grepl('S005', sujeto))
tabla.internet <- tabla.raw %>% filter(!grepl('S007', sujeto))
tabla.internet <- tabla.raw %>% filter(!grepl('S029', sujeto))
tabla.internet <- tabla.raw %>% filter(!grepl('S037', sujeto))

tabla.internet <- tabla.internet %>%
  mutate(nsub = as.numeric(gsub("[^0-9]", "", sujeto))) %>%
  mutate(condicion = "INTERNET")

tabla.internet <- tabla.internet[, -which(names(tabla.internet) == "sujeto")]

# Datos de particpantes de aula 27

tabla.sala_27  = read.csv("./data/datos.csv", header = TRUE, sep = '', stringsAsFactors = TRUE)

# Le sumo 100 a los del aula 27 para no repetir nsub, numero de sujeto
tabla.sala_27 <- tabla.sala_27 %>%
  mutate(nsub = as.numeric(gsub("[^0-9]", "", sujeto))) %>%
  mutate(nsub = nsub+100) %>%
  mutate(condicion = "SALA")

tabla.sala_27 <- tabla.sala_27[, -which(names(tabla.sala_27) == "sujeto")]

# Junto sala 27 e internet

experi_julio_tibble = rbind(tabla.internet, tabla.sala_27)

write.table(experi_julio_tibble, file="./data/datos_internet_y_sala.csv", row.names = FALSE)

# Load data NEW -----------------------------------------------------------

tabla.raw = read.csv("./data/datos_internet_y_sala.csv", header = TRUE, sep = '', stringsAsFactors = TRUE)

tabla.raw$SesgoAbs <-  tabla.raw$respuesta - tabla.raw$distancia
tabla.raw$SesgoRel <- (tabla.raw$respuesta - tabla.raw$distancia) / tabla.raw$distancia

# Remocion de outliers

f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

tabla.ind <- tibble(aggregate(cbind(respuesta,SesgoRel) ~ nsub*condicion*distancia,
                              data = tabla.raw,
                              FUN  = f_promedio,na.action = NULL))

# - Nivel poblacional

tabla.pob <- tibble(aggregate(cbind(respuesta[,"mean"],SesgoRel[,"mean"]) ~ condicion*distancia,
                              data <- tabla.ind,
                              FUN  <- f_promedio,na.action = NULL))

tabla.pob = tabla.pob %>% rename(respuestapob = V1)
tabla.pob = tabla.pob %>% rename(sesgorelpob = V2)

# Log Log
tabla.ind <- tabla.ind %>% 
  mutate(log_respuesta_mean = log(respuesta[,"mean"])) %>%
  mutate(log_distancia = log(distancia))

# Remocion de outliers
# Original
#res3 <- outliers_mad(x = filter(tabla_ADP.ind.Blind.VR,Bloque == "verbal report" & BlindCat == "Blind")$mSesgoRel ,na.rm=TRUE)
#plot_outliers_mad(res3,x=tabla_ADP.ind.Blind.VR$mSesgoRel,pos_display=TRUE)
#tabla_ADP.ind.Blind.VR[res3$outliers_pos,]

# Remocion de outliers de sujetos de internet

tabla.outlier <- tabla.ind %>% 
  #filter(Bloque == "verbal report" & BlindCat == "Blind") %>% 
  group_by(nsub, condicion) %>%
  summarise(mSesgoRel  = mean(SesgoRel[,"mean"],na.rm=TRUE))  %>%
  ungroup()

res3 <- outliers_mad(x = filter(tabla.outlier,condicion == 'INTERNET')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion == 'INTERNET')$mSesgoRel,pos_display=TRUE)


res3$outliers_pos

res3

# Outliers de Internet
# 5  7  8 17 29 37

# Remocion de outliers de sujetos de SALA 27

res3 <- outliers_mad(x = filter(tabla.outlier,condicion == 'SALA')$mSesgoRel ,na.rm=TRUE)

plot_outliers_mad(res3,x=filter(tabla.outlier,condicion == 'SALA')$mSesgoRel,pos_display=TRUE)

res3$outliers_pos

res3
# Outliers de Internet
# 2 14
# Ojo como sume 100 a los nsub de sala internet eso queda como:
# nsub 102 y 114

# Removiendo outliers...

removal_list = c("5", "7","8","17","29","37","102","114")
experi_julio_tibble <- tabla.raw %>%
  filter(!nsub %in% removal_list)

write.table(experi_julio_tibble, file="./data/datos_internet_y_sala_sin_outliers.csv", row.names = FALSE)



# Load data Latest - sin outliers -----------------------------------------



tabla.raw = read.csv("./data/datos_internet_y_sala_sin_outliers.csv", header = TRUE, sep = '', stringsAsFactors = TRUE)

# Sacar trial 16 del nsub 15 porque respuesta esta mal 77 m
# y el resto de repsuestas son normales

tabla.raw <- tabla.raw %>%
  filter(!(trial == 16 & nsub == 15))

# removiendo respuesta rara 34 metros
tabla.raw <- tabla.raw %>%
  filter(!(trial == 17 & nsub == 34))

tabla.raw$SesgoAbs <-  tabla.raw$respuesta - tabla.raw$distancia
tabla.raw$SesgoRel <- (tabla.raw$respuesta - tabla.raw$distancia) / tabla.raw$distancia

f_promedio <- function(x) c(mean = mean(x),
                            sd   = sd(x),
                            var  = var(x),
                            sem  = sd(x)/sqrt(length(x)),
                            n    = length(x))

tabla.ind <- tibble(aggregate(cbind(respuesta,SesgoRel) ~ nsub*condicion*distancia,
                              data = tabla.raw,
                              FUN  = f_promedio,na.action = NULL))

# - Nivel poblacional

tabla.pob <- tibble(aggregate(cbind(respuesta[,"mean"],SesgoRel[,"mean"]) ~ condicion*distancia,
                              data <- tabla.ind,
                              FUN  <- f_promedio,na.action = NULL))

tabla.pob = tabla.pob %>% rename(respuestapob = V1)
tabla.pob = tabla.pob %>% rename(sesgorelpob = V2)

# Log Log
tabla.ind <- tabla.ind %>% 
  mutate(log_respuesta_mean = log(respuesta[,"mean"])) %>%
  mutate(log_distancia = log(distancia))


# Figuras -----------------------------------------------------------------


figures_folder = "./figuras-julio-2023/"

#Grafico individual con brutos

g1 <- ggplot(tabla.ind, aes(x = distancia, y = respuesta[,"mean"])) +
  geom_errorbar(data=tabla.ind, color="black",alpha = 1, width=1.3, size=1.1,
                mapping=aes(ymin = respuesta[,"mean"] - respuesta[,"sem"],
                            ymax = respuesta[,"mean"] + respuesta[,"sem"]))+
  geom_point(color="red", size=1) +
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)")+#, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")+#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  geom_jitter(data = tabla.raw, mapping = aes(x=distancia, y=respuesta), color="blue", alpha=.8, shape=4, size=2.2, stroke=.2,
              position = position_jitter(width=.1, height=.1)) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  facet_grid(condicion ~ nsub) + 
  theme_linedraw(base_size = 9)

plot(g1)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_individuales.png", sep = '')
#ggsave(mi_nombre_de_archivo, plot = g1, width=44, height=14, units="cm", limitsize=FALSE, dpi=200)
ggsave(mi_nombre_de_archivo, plot = g1, width=100, height=20, units="cm", limitsize=FALSE, dpi=200)

# Por condicion 
# internet
internet <- filter(tabla.ind, condicion == 'INTERNET')

g1_internet <- ggplot(internet, aes(x = distancia, y = respuesta[,"mean"])) +
  geom_errorbar(data=internet, color="black",alpha = 1, width=1.3, size=1.1,
                mapping=aes(ymin = respuesta[,"mean"] - respuesta[,"sem"],
                            ymax = respuesta[,"mean"] + respuesta[,"sem"]))+
  geom_point(color="red", size=1) +
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)")+#, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")+#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  geom_jitter(data = filter(tabla.raw, condicion == 'INTERNET'), mapping = aes(x=distancia, y=respuesta), color="blue", alpha=.8, shape=4, size=2.2, stroke=.2,
              position = position_jitter(width=.1, height=.1)) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  facet_grid(. ~ nsub) + 
  theme_linedraw(base_size = 9)

plot(g1_internet)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_individuales-internet.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = g1_internet, width=100, height=20, units="cm", limitsize=FALSE, dpi=200)

# sala
sala <- filter(tabla.ind, condicion == 'SALA')

g1_sala <- ggplot(sala, aes(x = distancia, y = respuesta[,"mean"])) +
  geom_errorbar(data=sala, color="black",alpha = 1, width=1.3, size=1.1,
                mapping=aes(ymin = respuesta[,"mean"] - respuesta[,"sem"],
                            ymax = respuesta[,"mean"] + respuesta[,"sem"]))+
  geom_point(color="red", size=1) +
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)")+#, breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  scale_y_continuous(name="Perceived distance (m)")+#,  breaks=c(0,2.4,3.6,4.8,6,7), labels=c(0,2.4,3.6,4.8,6,7), minor_breaks=NULL, limits = c(0,20)) +
  geom_jitter(data = filter(tabla.raw, condicion == 'SALA'), mapping = aes(x=distancia, y=respuesta), color="blue", alpha=.8, shape=4, size=2.2, stroke=.2,
              position = position_jitter(width=.1, height=.1)) + 
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  facet_grid(. ~ nsub) + 
  theme_linedraw(base_size = 9)

plot(g1_sala)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_individuales-sala.png", sep = '')
ggsave(mi_nombre_de_archivo, plot = g1_sala, width=100, height=20, units="cm", limitsize=FALSE, dpi=200)


# Grafico poblacional con bruto ####

g2 <- ggplot(filter(tabla.pob, condicion != "SALA_PARLANTES"), aes(x = distancia, y = respuestapob[,"mean"],color = condicion, group = condicion)) +
  geom_errorbar(data=filter(tabla.pob, condicion != "SALA_PARLANTES"), alpha = 1, width=0.5, size=0.5,
                mapping=aes(ymin = respuestapob[,"mean"] - respuestapob[,"sem"], 
                            ymax = respuestapob[,"mean"] + respuestapob[,"sem"]))+ 
  geom_point(size=1, stroke = 1) + geom_line(size = 3)+
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)", breaks=c(0,1,2,3,4,5,6,7,80), labels=c(0,1,2,3,4,5,6,7,8), minor_breaks=NULL, limits = c(0,8)) +
  scale_y_continuous(name="Mean perceived distance +/- SEM (m)",  breaks=c(0,1,2,3,4,5,6,7,8), labels=c(0,1,2,3,4,5,6,7,8), minor_breaks=NULL, limits = c(0,8)) +
  # geom_jitter(data = filter(tabla.ind, condicion != "SALA_PARLANTES"), mapping = aes(x=distancia, y=respuesta[,"mean"],group=nsub, color = condicion), alpha=.8, size=0.7, stroke=.2,
  #          position = position_jitter(width=.1, height=0))+
  # geom_line(data = filter(tabla.ind, condicion != "SALA_PARLANTES"), mapping = aes(x=distancia, y=respuesta[,"mean"], group=nsub, color = condicion), alpha=.8, size=0.1)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  # facet_grid(nsub~ .)+
  theme_pubr(base_size = 9, margin = TRUE, legend = "right")

plot(g2)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_poblacional.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=g2, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)


## Sesgo
tabla_sesgo <- tabla.ind %>% 
  group_by(condicion,nsub) %>%
  summarise(mDist_perc = mean(respuesta[,"mean"]),
            mSesgoRel  = mean(SesgoRel[,"mean"]))  %>%
  ungroup()

tabla_sesgo.pob <- tabla_sesgo  %>% 
  group_by(condicion) %>%
  summarise(MSesgoRel = mean(mSesgoRel),
            N = n()) %>%
  ungroup()


fig.sesgo <- ggplot(tabla_sesgo, aes(x = condicion,
                                     y = mSesgoRel,
                                     colour = condicion))+
  geom_point(size = 4,alpha = 1,
             position = position_jitterdodge(jitter.width = .1,
                                             jitter.height = 0,
                                             dodge.width = .1)) +
  # stat_summary(fun.data = "mean_se",
  #              geom = "bar",
  #              alpha = .4,
  #              size = 1,
  #              position = position_dodge(width = 1)) +
  stat_summary(fun.data = "mean_se",
               geom = "line") +
  stat_summary(fun.data = "mean_se",
               geom = "bar",
               alpha = .4,
               size=2,
               position = position_dodge(width = 1)) +
  labs(x = "Condition de sala",
       y = "Bias") +
  theme_pubr(base_size = 9, margin = TRUE)
# theme(legend.position = "none")
fig.sesgo
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "sesgo.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=fig.sesgo, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)
