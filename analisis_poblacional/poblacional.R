# Dependencias ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)

# load data ---------------------------------------------------------------
data.internet <- read.csv("./analisis_poblacional/data/interntet_procesado_con_outliers.csv")

data.internet_sum <- data.internet %>%
  pivot_longer(
    cols = c("headphone_type"),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  select(c("Measure", "Value")) %>% 
  group_by(Value) %>%
  tally() %>%
  mutate(Ntot = sum(n))

write.csv(data.internet_sum, file ="./data/internet-sum.csv")

data.internet <- read.csv("./analisis_poblacional/data/internet-sum.csv")

pie <- ggplot(data.internet, aes(x = "", y=n, pattern = factor(Value),
                                    fill = factor(Value))) +
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  labs(fill="Tipo de auricular",
       x=NULL,
       y=NULL,
       title="",
       caption="") +
  geom_text(aes(label = paste0(round(n/Ntot*100), "%")), position = position_stack(vjust=0.5), size = 3) +
  coord_polar(theta = "y", start=0) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_brewer(palette="Pastel1")

pie

pie + coord_polar(theta = "y", start=0)
