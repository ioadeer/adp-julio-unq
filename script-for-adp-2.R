library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)

tabla.ind_1_10 <- tabla.ind[0:60,]
tabla.datos_1_10 <- tabla.datos[0:300,]


f2_1_10 <- ggplot(tabla.ind_1_10, aes(x=distancia, y =mrespuesta)) +
  geom_point(size = 2.5,color = "BLUE",alpha = .6)+
  geom_line(size = 1.5,color = "BLUE",alpha = .6)+
  geom_jitter(data = tabla.datos_1_10, mapping = aes(x=distancia, y = respuesta), size = 1, width=.15, height = .1)+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  scale_x_continuous(name="Distance source (m)", breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  facet_wrap(sujeto~.,ncol = 5)+ #scales="free_y")+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

plot(f2_1_10)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "f1_1_10", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f2_1_10, width=20, height=10, units="cm", limitsize=FALSE, dpi=300)

tabla.ind_11_20 <- tabla.ind[66:120,]
tabla.datos_11_20 <- tabla.datos[315:600,]


f2_11_20 <- ggplot(tabla.ind_11_20, aes(x=distancia, y =mrespuesta)) +
  geom_point(size = 2.5,color = "BLUE",alpha = .6)+
  geom_line(size = 1.5,color = "BLUE",alpha = .6)+
  geom_jitter(data = tabla.datos_11_20, mapping = aes(x=distancia, y = respuesta), size = 1, width=.15, height = .1)+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  scale_x_continuous(name="Distance source (m)", breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  facet_wrap(sujeto~.,ncol = 5)+ #scales="free_y")+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

plot(f2_11_20)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "f2_11_20", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f2_11_20, width=20, height=10, units="cm", limitsize=FALSE, dpi=300)

tabla.ind_21_30 <- tabla.ind[126:180,]
tabla.datos_21_30 <- tabla.datos[615:900,]


f2_21_30 <- ggplot(tabla.ind_21_30, aes(x=distancia, y =mrespuesta)) +
  geom_point(size = 2.5,color = "BLUE",alpha = .6)+
  geom_line(size = 1.5,color = "BLUE",alpha = .6)+
  geom_jitter(data = tabla.datos_21_30, mapping = aes(x=distancia, y = respuesta), size = 1, width=.15, height = .1)+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  scale_x_continuous(name="Distance source (m)", breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  scale_y_continuous(name="Perceived distance (m)",  breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0,6)) +
  facet_wrap(sujeto~.,ncol = 5)+ #scales="free_y")+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

plot(f2_21_30)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "f2_21_30", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f2_21_30, width=20, height=10, units="cm", limitsize=FALSE, dpi=300)
