
install.packages("ggthemes")

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

# TODO 

# - curvas de distancia
# - sesgo
# - desviacion estandar intrasujeto (variabilidad entre trails) sacar promedio y sd de repuestas para mismo trials. ver la distinta variabilidad para la distintas posiciones.
# - Desviacion estandar en promedio
# - Variabilidad entre sujetos comparando condiciones aula vs internet
# - collapsed standar deviation between subject 

# Load data Latest - sin outliers -----------------------------------------

tabla.raw = read.csv("./data/datos_internet_y_sala_sin_outliers.csv", header = TRUE, sep = '', stringsAsFactors = TRUE)

# Sacar trial 16 del nsub 15 porque respuesta esta mal 77 m
# y el resto de repsuestas son normales

# volamos a este sujeto que respondio mucho cero y me caga 
# hacer una regresion log log
tabla.raw <- tabla.raw %>%
  filter(!nsub == 121)

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

#sala  con sem <- filter(tabla.ind, condicion == 'INTERNET')
g2 <- ggplot(tabla.pob, aes(x = distancia, y = respuestapob[,"mean"],color = condicion, group = condicion)) +
  geom_errorbar(data=tabla.pob, alpha = 1, width=0.5, size=0.5,
                mapping=aes(ymin = respuestapob[,"mean"] - respuestapob[,"sem"], 
                            ymax = respuestapob[,"mean"] + respuestapob[,"sem"]))+ 
  geom_point(size=1, stroke = 1) + geom_line(size = 3)+
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)", breaks=c(0,1,2,3,4,5,6,7,80), labels=c(0,1,2,3,4,5,6,7,8), minor_breaks=NULL, limits = c(0,10)) +
  scale_y_continuous(name="Mean perceived distance +/- SEM (m)",  breaks=c(0,1,2,3,4,5,6,7,8), labels=c(0,1,2,3,4,5,6,7,8), minor_breaks=NULL, limits = c(0,10)) +
  # geom_jitter(data = filter(tabla.ind, condicion != "SALA_PARLANTES"), mapping = aes(x=distancia, y=respuesta[,"mean"],group=nsub, color = condicion), alpha=.8, size=0.7, stroke=.2,
  #          position = position_jitter(width=.1, height=0))+
  # geom_line(data = filter(tabla.ind, condicion != "SALA_PARLANTES"), mapping = aes(x=distancia, y=respuesta[,"mean"], group=nsub, color = condicion), alpha=.8, size=0.1)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  # facet_grid(nsub~ .)+
  theme_pubr(base_size = 9, margin = TRUE, legend = "right")

#sala  con sd <- filter(tabla.ind, condicion == 'INTERNET')
g2 <- ggplot(tabla.pob, aes(x = distancia, y = respuestapob[,"mean"],color = condicion, group = condicion)) +
  geom_errorbar(data=tabla.pob, alpha = 1, width=0.5, size=0.5,
                mapping=aes(ymin = respuestapob[,"mean"] - respuestapob[,"sd"], 
                            ymax = respuestapob[,"mean"] + respuestapob[,"sd"]))+ 
  geom_point(size=1, stroke = 1) + geom_line(size = 3)+
  #geom_abline(intercept = 1, slope = 1,linetype="dashed") +       
  scale_x_continuous(name="Distance source (m)", breaks=c(0,1,2,3,4,5,6,7,80), labels=c(0,1,2,3,4,5,6,7,8), minor_breaks=NULL, limits = c(0,10)) +
  scale_y_continuous(name="Mean perceived distance +/- SD (m)",  breaks=c(0,1,2,3,4,5,6,7,8), labels=c(0,1,2,3,4,5,6,7,8), minor_breaks=NULL, limits = c(0,10)) +
  # geom_jitter(data = filter(tabla.ind, condicion != "SALA_PARLANTES"), mapping = aes(x=distancia, y=respuesta[,"mean"],group=nsub, color = condicion), alpha=.8, size=0.7, stroke=.2,
  #          position = position_jitter(width=.1, height=0))+
  # geom_line(data = filter(tabla.ind, condicion != "SALA_PARLANTES"), mapping = aes(x=distancia, y=respuesta[,"mean"], group=nsub, color = condicion), alpha=.8, size=0.1)+
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  # facet_grid(nsub~ .)+
  theme_pubr(base_size = 9, margin = TRUE, legend = "right")

plot(g2)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "distancia_poblacional.png", sep = '')
ggsave(mi_nombre_de_archivo, plot=g2, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)


# Hacer una desviacion estandar ##

g4 <- ggplot(tabla.pob, aes(x = distancia, y = respuestapob[,"sd"], color= condicion, group= condicion)) +
  geom_point(size=3, stroke = 1) + geom_line(size = 3)+
  geom_errorbar(data=tabla.pob, alpha = 1, width=0.2, size=1.1,
                mapping=aes(ymin = respuestapob[,"sd"] - respuestapob[,"sem"],
                            ymax = respuestapob[,"sd"] + respuestapob[,"sem"]))+
  scale_x_continuous(name="Distance source (m)", breaks=c(1,2,3,4,5,6), labels=c(1,2,3,4,5,6), minor_breaks=NULL, limits = c(0.75,6.25)) +
  scale_y_continuous(name="Standard deviation +/- SEM (m)",  breaks=c(0,1,2,3,4), labels=c(0,1,2,3,4), minor_breaks=NULL, limits = c(0,4)) 


plot(g4)
ggsave("Desviacion_estandard_masmenos_SEM.png", plot =g4, units="cm", limitsize=FALSE, dpi=200)

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


##
# Falta collapsed standard deviation [%] between subject



# Estadistica -------------------------------------------------------------


# Both tasks
tabla.ind <- tabla.ind %>% 
  mutate(log_respuesta_mean = log(respuesta[,"mean"])) %>%
  mutate(log_distancia = log(distancia))

# LOG LOG
m.distancia <- lmer(log_respuesta_mean ~ condicion * log_distancia + (1|nsub), 
                    data = tabla.ind)

summary(m.distancia)
ggcoefstats(m.distancia, output = "tidy") %>% select(-label)

anova(m.distancia)

sink("./resultado_lmer_summary.txt")
print(summary(m.distancia))
sink() 

sink("./resultado_lmer_anova.txt")
print(anova(m.distancia))
sink() 

## Hacer regresion lineal por sujeto y despues graficar
# Compresiones en boxplots

por_sujeto <- tabla.ind %>%
  group_by(nsub) %>%
  nest()

# asi tiene que ser
# country_model <- function(df) {
#   lm(lifeExp ~ year, data = df)
# }
modelo_sujeto <- function(df) {
  lm(log_respuesta_mean ~ log_distancia, data = df)
}

#modelos <- map(por_sujeto$data, modelo_sujeto)

library(modelr)
library(purrr)

por_sujeto <- por_sujeto %>%
  mutate(model = map(data, modelo_sujeto))

por_sujeto <- por_sujeto %>%
  mutate(resids = map2(data, model, add_residuals))

resids <- unnest(por_sujeto, resids)

resids_modelr <- resids %>% 
  ggplot(aes(log_distancia, resid)) +
  geom_line(aes(group = nsub), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

model_modelr <- resids %>% 
  ggplot(aes(log_distancia,log_respuesta_mean)) +
  geom_line(aes(group = nsub), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

graphs <- ggarrange(model_modelr,resids_modelr,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
plot(graphs)
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "adjust-and-resids_modelr.png", sep = '')
mi_nombre_de_archivo = "adjust-and-resids_modelr.png"
ggsave(mi_nombre_de_archivo, plot =graphs, dpi=200)

por_sujeto_test <- por_sujeto %>%
  mutate(sum = model[[1]][1]) %>%
  mutate(intercept = sum[[1]][1]) %>%
  mutate(slope = sum[[1]][2]) %>%

por_sujeto_test_unnested <- unnest(por_sujeto_test,data)

## Aca voy a hacer boxplot con pendientes

slope_boxplot <- ggboxplot(por_sujeto_test_unnested, x ="condicion", y="slope")

ggsave("boxplot_de_compresion.png", plot =slope_boxplot, dpi=200)

plot(slope_boxplot)

# Ajustes individuales

por_sujeto_test_unnested %>% 
  ggplot(aes(log_distancia, log_respuesta_mean)) +
  geom_point(aes(y =log_respuesta_mean)) +
  geom_abline(aes(intercept = intercept, slope = slope)) +
  geom_smooth(se = FALSE)

por_sujeto_test_unnested %>% 
  ggplot(aes(log_distancia, log_respuesta_mean)) +
  geom_point(aes(y =log_respuesta_mean)) +
  geom_abline(aes(intercept = intercept, slope = slope)) +
  geom_smooth(se = FALSE)

# Ajustes individuales

ajust_sub <- por_sujeto_test_unnested %>% 
  ggplot(aes(log_distancia, log_respuesta_mean)) +
  geom_abline(aes(intercept = intercept, slope = slope)) +
  facet_grid(condicion ~ nsub) +
  geom_smooth(se = FALSE)

plot(ajust_sub)

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ajuste_sub.png", sep = '')
#ggsave(mi_nombre_de_archivo, plot = g1, width=44, height=14, units="cm", limitsize=FALSE, dpi=200)
ggsave("ajustes_individuales.png", plot =ajust_sub, width=100, height=20, units="cm", limitsize=FALSE, dpi=200)

# Histograma de exponentes de respuesta

histograma_exponentes <- ggplot(por_sujeto_test_unnested, aes(x=slope)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  # geom_vline(data=mean(mDist_perc), aes(xintercept=grp.mean, color=condicion_sala),
  #             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Respuesta histogram plot",x="Exponentes", y = "Density")+
  theme_classic()

plot(histograma_exponentes)

#mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "histogramas_exponentes_sub.png", sep = '')
ggsave("histograma_exponentes.png", plot =histograma_exponentes, dpi=200)

por_sujeto_test_2 <- por_sujeto_test_unnested %>%
  mutate(predi = predict(model[[1]]))

por_sujeto_test_2 <- por_sujeto_test_2 %>%
  mutate(predi_linear = exp(predi))

predictions  <- por_sujeto_test_2 %>% 
  ggplot(aes(distancia, predi_linear)) +
  geom_point(aes(y =respuesta[,1])) +
  facet_grid(condicion ~ nsub) +
  geom_smooth(se = FALSE) +
  labs(x="", y="", title ='Prediccion hecha con log log pasada lineal')

plot(predictions)  

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "prediccion_lineal.png", sep = '')
ggsave("predictions.png", plot =predictions, width=100, height=20, units="cm",limitsize=FALSE,  dpi=200)

