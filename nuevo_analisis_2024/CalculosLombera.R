
# Dependencies ------------------------------------------------------------


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



# Load data ---------------------------------------------------------------

rm(list=ls())
figures_folder = "Figuras"
results_tbl <- read.csv("./ResultsData/Dresults.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")



# Ajustamos 5 modelos y confirmamos que el modelo 3 es el mas acertado. Sobre todo porq: La pendiente de la distancia varia en funcion del sujo,
#pero trata la variacion entre room_condition y la distancia como INDEPENDIENTES. Tambien hicimos un ANOVA entre los modelos y el 3 es el que mejor
# Ajusta segun AIC. Dejo todo en el dbug

# EXPERIMENTO 2 -----
m.Dist3 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
                 data = results_tbl) 

m.fixed <- lm(log10(perc_dist) ~ log10(target_distance)*room_condition,
              data = results_tbl)

extract_stats(ggcoefstats(m.Dist3))
r.squaredGLMM(m.Dist3)

anova(m.Dist3)
anov1 = anova(m.Dist3)

results_tbl$Modelfitted3<-predict(m.Dist3)

# Todos los sujetos
FittedlmPlot1 <-ggplot()+
  facet_grid(subject ~ room_condition, labeller=label_both)+
  geom_line(data = results_tbl, aes(x = target_distance, y = 10^Modelfitted3))+
  geom_point(data = results_tbl, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
  #  coord_cartesian(ylim = c(.03,.074))+ 
  xlab("Targent_distance")+ylab("Perceived_distance")
FittedlmPlot1


Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist3)

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

#Bias

cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

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

f6 =  ggplot(results_tblp, aes(x = room_condition,y = MBiasSigned, colour = room_condition, fill = room_condition)) +
  geom_pointrange(aes(x=room_condition, y=MBiasSigned, ymin=MBiasSigned-SDBiasSigned, ymax=MBiasSigned+SDBiasSigned), size = 1.2)+
  geom_line(aes(group = room_condition),size = 1.2, alpha=.5)+
  geom_point(data = results_tbls, mapping = aes(x = room_condition,y = mBiasSigned, colour = room_condition, fill = room_condition), alpha = .8)+
  geom_line(data = results_tbls, mapping = aes(x = room_condition,y = mBiasSigned, group = subject, colour = room_condition, fill = room_condition),alpha = 0.3)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope = 0,
              intercept = 0,
              alpha = 0.5,
              linetype = "dashed") +
  labs(x = "Condition", 
       y = "Relative signed \nbias [%]") +
  # facet_grid(. ~ type) +
  # annotate("text", x = 1.5, y = 2,  label = "***", size = 4) +
  # annotate("segment", x = 1, xend = 2, y = 1.9, yend = 1.9, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "none")

m.RelativBias <- lm(mBiasSigned ~ room_condition, 
                    data = results_tbls)
extract_stats(ggcoefstats(m.RelativBias))
anov = anova(m.RelativBias)
anov
f6
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "13. Bias signed", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f6, width=15, height=10, units="cm", limitsize=FALSE, dpi=600)

t.test(filter(results_tbls, 
              room_condition=="" )$mBiasSigned,
       filter(results_tbls, 
              room_condition=="VISUAL")$mBiasSigned, 
       paired = TRUE)



# EXPERIMENTO 3 -----
results_tbl <- read.csv("./Exp_3_ADP_vr/ResultsData/DresultsExp3.csv", header = TRUE, sep = ',', stringsAsFactors = TRUE)


m.Dist3 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject)+(0+room_condition|subject),
                 data = results_tbl) 
extract_stats(ggcoefstats(m.Dist3))
r.squaredGLMM(m.Dist3)

anova(m.Dist3)
anov1 = anova(m.Dist3)

results_tbl$Modelfitted3<-predict(m.Dist3)

# Todos los sujetos
FittedlmPlot1 <-ggplot()+
  facet_grid(subject ~ room_condition, labeller=label_both)+
  geom_line(data = results_tbl, aes(x = target_distance, y = 10^Modelfitted3))+
  geom_point(data = results_tbl, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
  #  coord_cartesian(ylim = c(.03,.074))+ 
  xlab("Targent_distance")+ylab("Perceived_distance")
FittedlmPlot1


Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist3)

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

Final.Fixed<-effect(c("target_distance*room_condition"), m.Dist1)

# You have to convert the output to a dataframe
Final.Fixed<-as.data.frame(Final.Fixed)

Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =fit, group=room_condition))+
  # coord_cartesian(xlim=c(0,4),ylim = c(0,.7))+ 
  geom_line(aes(color=room_condition), size=2)+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=room_condition),alpha=.2)+
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


# debug ----

# m.Dist1 <-  lmer(perc_dist ~ target_distance*room_condition+(1+target_distance|subject),
#                  data = results_tbl)
# extract_stats(ggcoefstats(m.Dist1))
# r.squaredGLMM(m.Dist1)
# 
# anova(m.Dist1)
# anov1 = anova(m.Dist1)
# 
# 
# m.Dist2 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)|subject),
#                  data = results_tbl)
# extract_stats(ggcoefstats(m.Dist2))
# r.squaredGLMM(m.Dist2)
# 
# anova(m.Dist2)
# anov1 = anova(m.Dist2)

# m.Dist4 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)+room_condition|subject),
#                  data = results_tbl) 
# extract_stats(ggcoefstats(m.Dist4))
# r.squaredGLMM(m.Dist4)
# 
# anova(m.Dist4)
# anov1 = anova(m.Dist4)
# 
# m.Dist5 <-  lmer(log10(perc_dist) ~ log10(target_distance)*room_condition+(1+log10(target_distance)*room_condition|subject),
#                  data = results_tbl) 
# extract_stats(ggcoefstats(m.Dist5))
# r.squaredGLMM(m.Dist5)
# 
# anova(m.Dist5)
# anov1 = anova(m.Dist5)

# results_tbl$Modelfitted1<-predict(m.Dist1)
# results_tbl$Modelfitted2<-predict(m.Dist2)
# results_tbl$Modelfitted3<-predict(m.Dist3)
# results_tbl$Modelfitted3<-predict(m.Dist4)
# FittedlmPlot1 <-ggplot()+
#   facet_grid(subject ~ room_condition, labeller=label_both)+
#   geom_line(data = results_tbl, aes(x = target_distance, y = 10^Modelfitted2))+
#   geom_point(data = results_tbl, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
#   #  coord_cartesian(ylim = c(.03,.074))+ 
#   xlab("Targent_distance")+ylab("Perceived_distance")
# FittedlmPlot1
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Figure1_model22", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=FittedlmPlot1, width=10, height=50, units="cm", limitsize=FALSE, dpi=600)
# 
# 
# library(effects)
# Final.Fixed<-effect(c("log10(target_distance)*room_condition"), m.Dist3)
# 
# # You have to convert the output to a dataframe
# Final.Fixed<-as.data.frame(Final.Fixed)
# 
# Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=room_condition))+
#   # coord_cartesian(xlim=c(0,4),ylim = c(0,.7))+ 
#   geom_line(aes(color=room_condition), size=2)+
#   geom_ribbon(aes(ymin=10^fit-10^se, ymax=10^fit+10^se,fill=room_condition),alpha=.2)+
#   xlab("Target_distance")+
#   ylab("Perceived_distance")+
#   # scale_color_manual(values=c("blue", "red"))+
#   # scale_fill_manual(values=c("blue", "red"))+
#   theme_bw()+
#   theme(text=element_text(face="bold", size=12),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.border = element_rect(fill = NA, colour = "NA"),
#         axis.line = element_line(size = 1, colour = "grey80"),
#         legend.title=element_blank(),
#         legend.position = c(.2, .92))
# Final.Fixed.Plot
# 
# Final.Fixed<-effect(c("target_distance*room_condition"), m.Dist1)
# 
# # You have to convert the output to a dataframe
# Final.Fixed<-as.data.frame(Final.Fixed)
# 
# Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =fit, group=room_condition))+
#   # coord_cartesian(xlim=c(0,4),ylim = c(0,.7))+ 
#   geom_line(aes(color=room_condition), size=2)+
#   geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill=room_condition),alpha=.2)+
#   xlab("Target_distance")+
#   ylab("Perceived_distance")+
#   # scale_color_manual(values=c("blue", "red"))+
#   # scale_fill_manual(values=c("blue", "red"))+
#   theme_bw()+
#   theme(text=element_text(face="bold", size=12),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         panel.border = element_rect(fill = NA, colour = "NA"),
#         axis.line = element_line(size = 1, colour = "grey80"),
#         legend.title=element_blank(),
#         legend.position = c(.2, .92))
# Final.Fixed.Plot





p_val_format <- function(x){
  z <- scales::pvalue_format()(x)
  z[!is.finite(x)] <- ""
  z
}
anov1$Predictors = c("Target distance","Condition","Target distance:Condition")
anov1 = data.frame(anov1)
anov1 <- flextable(anov1,col_keys = c("Predictors","NumDF","DenDF", "F.value","Pr..F.")) %>%
  hline_top(border = fp_border(color="black", width = .5), part = "all")%>%
  hline_bottom(border = fp_border(color="black", width = .5))%>%
  width(j = 1, width = 5, unit = "cm")%>%
  align(align = "center", part = "all")%>%
  align(j = 1, align = "left", part = "all")%>%
  colformat_double(digits = 1, na_str = "N/A")%>%
  set_formatter(values = list("Pr..F." = p_val_format) )%>% 
  font(fontname = "+font-family: Arial;")%>%
  font(fontname = "+font-family: Arial;", part = "header")%>%
  
  fontsize(size = 10.5, part = "header")%>% 
  fontsize(size = 10.5)%>%
  bold(j = "Pr..F.", bold = TRUE)%>%
  italic(italic = TRUE, part = "header")%>%
  line_spacing(space = 1, part = "body")%>%
  line_spacing(space = .5, part = "header")
anov1


save_as_image(anov1,"Anov_PAD_POB_NORMAL_LIN.png")

# tab_model(m.Dist1,file="plot.html")
tab_model(m.Dist1, wrap.labels = 80,
          auto.label = FALSE, show.stat = TRUE, string.p = "p.value", string.ci = "CI 95%", dv.labels = "Perceived distance",
          show.intercept = TRUE, show.aic = FALSE, show.zeroinf = TRUE, show.re.var = FALSE, show.reflvl = TRUE,
          CSS = list(css.table = '+font-family: Arial;'),
          pred.labels = c("Intercept","Target distance", "Condition (Floor level)","Target distance * Condition (Floor level)"),
          file = "plot.html")


webshot("plot.html","Summary_PAD_POB_NORMAL_LIN.png", vwidth = 600, vheight = 100)



m.Dist1 <-  lme(perc_dist ~ target_distance*room_condition, random = ~target_distance|subject,
                method = "ML", control =list(msMaxIter = 1000, msMaxEval = 1000),
                data = results_tbl)
eq1 <- substitute("Ear level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                  list(a = round(m.Dist1$coefficients$fixed[[2]],digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[1]], digits = 2)))
eq2 <- substitute("Floor level:"~~~italic(y) == a %.% italic(X)+italic((b)), 
                  list(a = round(m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], digits = 2),
                       b = round(m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], digits = 2)))
eq3 <- substitute("r.squared:"~~~italic(R)^italic(2) == italic(b), 
                  list(b = round(r.squaredGLMM(m.Dist1)[2], digits = 2)))

tabla.pob = results_tbl %>% group_by(target_distance,room_condition) %>%
  summarise(Mperc_dist  = mean(perc_dist),
            SDperc_dist = sd(perc_dist)/sqrt(n()))  %>%
  ungroup()

f1 <- ggplot(tabla.pob, aes(x=target_distance, y =Mperc_dist, group = room_condition, color  = room_condition)) + 
  geom_pointrange(aes(x = target_distance, y = Mperc_dist, ymin = Mperc_dist-SDperc_dist, ymax = Mperc_dist+SDperc_dist),alpha = 1, 
                  position = position_jitterdodge(jitter.width = .1,
                                                  jitter.height = 0,
                                                  dodge.width = .1 ))+
  geom_abline(intercept = 0, slope = 1, linetype=2) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_abline(slope =m.Dist1$coefficients$fixed[[2]], 
              intercept =m.Dist1$coefficients$fixed[[1]], 
              alpha = 0.5,
              color = "#000000") +
  geom_abline(slope =m.Dist1$coefficients$fixed[[2]]+m.Dist1$coefficients$fixed[[4]], 
              intercept =m.Dist1$coefficients$fixed[[1]]+m.Dist1$coefficients$fixed[[3]], 
              alpha = 0.5,
              color = "#E69F00") +
  geom_text(x = 0.2, y = 7.1, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
  geom_text(x = 0.2, y = 6.7, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#E69F00")+
  geom_text(x = 0.2, y = 6.3, label = as.character(as.expression(eq3)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#009E73")+
  scale_x_continuous(name="Distance source (m)", limits = c(0,8)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,8)) +
  theme_pubr(base_size = 12, margin = TRUE)+
  theme(legend.position = "top",
        legend.title = element_blank())

f1
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "5. Lme Lineal-Normal", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=f1, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

