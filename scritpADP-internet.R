library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
rm(list=ls())
tabla.datos = read.csv("./data/datos-de-internet-1.csv", header = TRUE, sep = '', stringsAsFactors = TRUE)

#tabla.datos <- tabla.datos %>% filter(!grepl('S002', sujeto))

# sacar sujeto 5 7 29 37
tabla.datos <- tabla.datos %>% filter(!grepl('S005', sujeto))
tabla.datos <- tabla.datos %>% filter(!grepl('S007', sujeto))
tabla.datos <- tabla.datos %>% filter(!grepl('S029', sujeto))
tabla.datos <- tabla.datos %>% filter(!grepl('S037', sujeto))

#tabla.datos <- tabla.datos %>% filter(sujeto != 'S014' | trial != 5)

figures_folder = "figuras-internet"
cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

tabla.ind = tabla.datos%>% 
  group_by(sujeto,distancia) %>%
  summarise(mrespuesta = mean(respuesta))  %>%
  ungroup()


tabla.pob <- tabla.ind %>% 
  group_by(distancia) %>%
  summarise(Mrespuesta = mean(mrespuesta),
            SDrespuesta = sd(mrespuesta),
            SError = sd(mrespuesta)/sqrt(length(mrespuesta)))  %>%
  ungroup()

f2 <- ggplot(tabla.ind, aes(x=distancia, y =mrespuesta)) +
  geom_point(size = 2.5,color = "BLUE",alpha = .6)+
  geom_line(size = 1.5,color = "BLUE",alpha = .6)+
  geom_jitter(data = tabla.datos, mapping = aes(x=distancia, y = respuesta), size = 1, width=.15, height = .1)+
  # geom_pointrange(aes(ymin=Mrespuesta-SDrespuesta,
  #                     ymax=Mrespuesta+SDrespuesta),
  #                 size = 1.5,fatten = .1,alpha = .6,color = "BLUE",
  #                 position=position_jitter(width=.01, height=0)) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  scale_x_continuous(name="Distance source (m)", breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,12)) +
  facet_wrap(sujeto~.,nrow = 5)+ #scales="free_y")+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

plot(f2)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "f1-individual-internet", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f2, width=50, height=70, units="cm", limitsize=FALSE, dpi=300)


f2 <- ggplot(tabla.pob, aes(x=distancia, y =Mrespuesta)) + 
  geom_line(size = 1.5, color = "BLUE", linetype=1)+
  #geom_line(data = tabla.ind, mapping = aes(x=distancia, y = mrespuesta, color = sujeto), size = 1) +
  geom_pointrange(aes(ymin=Mrespuesta-SError,
                      ymax=Mrespuesta+SError),
                  size = 1.5,fatten = .1,alpha = .6,color = "BLUE",
                  position=position_jitter(width=.01, height=0)) +
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  #geom_line(data = tabla.ind, mapping = aes(x=Distancia, y=Distancia_percibida[,"mean"],group = interaction(Sujeto,Bloque), color = Bloque ) , alpha=.4, size=0.4)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  scale_x_continuous(name="Distance source (m)", breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,7)) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(1,2,3,4,5,6,7), labels=c(1,2,3,4,5,6,7), minor_breaks=NULL, limits = c(0,8)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())


plot(f2)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "f1-pob-internet-se", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f2, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)
