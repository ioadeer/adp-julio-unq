library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
rm(list=ls())
tabla.datos = read.csv("./data/datos.csv", header = TRUE, sep = '', stringsAsFactors = TRUE)

tabla.ind = tabla.datos%>% 
  group_by(sujeto,distancia) %>%
  summarise(mrespuesta = mean(respuesta))  %>%
  ungroup()


tabla.pob <- tabla.ind %>% 
  group_by(distancia) %>%
  summarise(Mrespuesta = mean(mrespuesta),
            SDrespuesta = sd(mrespuesta))  %>%
  ungroup()



figures_folder = "figuras"
cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

cbPalette <- c("#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
               "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
               "#000F00", "#809693", "#0F0F0F", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
               "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9")

f2 <- ggplot(tabla.pob, aes(x=distancia, y =Mrespuesta)) + 
  geom_line(size = 1.5, color = "BLUE", linetype=4)+
  geom_line(data = tabla.ind, mapping = aes(x=distancia, y = mrespuesta, color = sujeto), size = 1) +
  geom_pointrange(aes(ymin=Mrespuesta-SDrespuesta,
                      ymax=Mrespuesta+SDrespuesta),
                  size = 1.5,fatten = .1,alpha = .6,color = "BLUE",
                  position=position_jitter(width=.01, height=0)) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  #geom_line(data = tabla.ind, mapping = aes(x=Distancia, y=Distancia_percibida[,"mean"],group = interaction(Sujeto,Bloque), color = Bloque ) , alpha=.4, size=0.4)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  scale_x_continuous(name="Distance source (m)", breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

plot(f2)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "f1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f2, width=20, height=20, units="cm", limitsize=FALSE, dpi=300)
