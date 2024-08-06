# en este archivo voya  ahcer analisis de los coeficientes que saque de
# los datos del experimento. la obtencion de coeficientes por participantes
# esta en el archivo coeficientes_por_sujeto.R


# load data ---------------------------------------------------------------

data.clean <-  read.csv('data/coeficientes_por_sujeto_sin_outliers.csv', header = TRUE, sep = ' ', stringsAsFactors = TRUE)


# coefficient a (non linear compression) ----------------------------------

data.clean.controlled <- data.clean %>%
  filter(room_condition == "Controlled environment")

eqn1 <- sprintf(
  "M = %.3g ± %.2g \nMdn = %.3g (%.2g - %.2g)\nN =  %.2g",
  mean(data.clean.controlled$coef),
  sd(data.clean.controlled$coef),
  median(data.clean.controlled$coef),
  quantile(data.clean.controlled$coef)[4],
  quantile(data.clean.controlled$coef)[2],
  sum(data.clean.controlled$coef)
)

hist_a_sc <- ggplot(data.clean.controlled, aes(coef)) +
  geom_histogram(binwidth= 0.1, color="black",fill="white") +
  scale_x_continuous(breaks= seq(0.3,2.1, by=0.5)) +
  scale_y_continuous(breaks= seq(0,8, by = 1))+
  xlim(0.3,2.1) +
  ylim(0,8) +
  geom_vline(xintercept = round(mean(data.clean.controlled$coef),2),        # Add line for mean
             linetype="dashed",
  ) +
  annotate("text",                        # Add text for mean
           #x = 1.5, # para fig sola
           x = 1.4, # para fig compuesta
           y = 4.0,
           label = eqn1,
           size = 1.5,
           hjust = 0) +
  labs(x="a", y = "Count")+
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank()) +
  theme_minimal()

#hist_a_sc

# sala grande

data.clean.unknown <- data.clean %>%
  filter(room_condition == "Unknown environment")

eqn1 <- sprintf(
  "M = %.3g ± %.2g \nMdn = %.3g (%.2g - %.2g)\nN =  %.2g",
  mean(data.clean.unknown$coef),
  sd(data.clean.unknown$coef),
  median(data.clean.unknown$coef),
  quantile(data.clean.unknown$coef)[4],
  quantile(data.clean.unknown$coef)[2],
  sum(data.clean.unknown$coef)
)


# scale_x_continuous(breaks= seq(0.3,2.1, by=0.5)) +
#   scale_y_continuous(breaks= seq(0,5, by = 1))+
#   xlim(0.3,2) +
#   ylim(0,5) +

hist_a_sg <- ggplot(data.clean.unknown, aes(coef)) +
  geom_histogram(binwidth= 0.1, color="black",fill="white") +
  geom_vline(xintercept = round(mean(data.clean.unknown$coef),2),        # Add line for mean
             linetype="dashed",
  ) +
  scale_x_continuous(breaks= seq(0.3,2.1, by=0.5)) +
  scale_y_continuous(breaks= seq(0,8, by = 1))+
  xlim(0.3,2.1) +
  ylim(0,8) +
  annotate("text",                        # Add text for mean
           #x = 1.5, # para fig sola
           x = 1.4, # para fig compuesta
           y = 4.0,
           label = eqn1,
           size = 1.5,
           hjust = 0) +
  labs(x="a", y = "Count")+
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank()) +
  theme_minimal()

#hist_a_sg

# a_comp <- ggarrange(
#   hist_a_sc,
#   hist_a_sg,
#   nrow = 2
# )

#plot(a_comp)

# histograma a apaisado
data.clean.boxplot_a <- data.clean %>%
  mutate(
    room_condition = case_when(
      room_condition == "Controlled environment" ~ "Controlled",
      room_condition == "Unknown environment" ~ "Unknown",
    )
  )

bxp <- ggboxplot(data.clean.boxplot_a, x = "room_condition", y = "coef",
                 orientation = "horizontal",
                 color = "room_condition", palette = "jco",
                 add = "jitter", ylab="coef a",
                 title="T-test coeficientes a") +
  theme_minimal() +
  theme(
    legend.position="none",
    #axis.line = element_blank(),
    axis.title.x = element_blank(),
    #axis.title = element_text(hjust = 0),
    #axis.ticks = element_blank(),
    axis.title.y = element_blank())


stat.test <- data.clean.boxplot_a  %>% 
  t_test(coef~room_condition) %>%
  add_significance()

stat.test <- stat.test %>% add_y_position(fun ="mean", step.increase = 3)

bxp <- bxp + stat_pvalue_manual(
  stat.test, label = "p", tip.length = 0.01,
  coord.flip = TRUE
) +
  coord_flip()

#bxp

all <- ggarrange(
  bxp,
  ggarrange(
    hist_a_sc, 
    hist_a_sg,
    nrow = 2
  ),
  ncol = 2
)

plot(all)


# save plot a coeff -------------------------------------------------------

figures_folder = "./coeficientes_por_sujeto/figures/"
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "coefficient_a", ".png", sep = '')
#ggsave(device = "png", mi_nombre_de_archivo, plot=main_figure, width=15, height=15, units="cm", limitsize=FALSE, dpi=600)

## asi me lo guarda bien
png(mi_nombre_de_archivo, res=600, units="cm", width=20, height=15)
plot(all)
dev.off()
