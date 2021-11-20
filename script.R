library(tidyverse)


options(scipen = 999)
theme_set(theme_classic())

data <- readxl::read_xlsx("Predicción Electoral.xlsx")

data %>%
  gather("candidato", "perc", 1:7) -> data

colnames(data)[c(3, 4)] <- c("tendpol", "ses")

data$tendpol <- factor(data$tendpol,
                       unique(data$tendpol)[c(3, 1, 2, 4)])

data$ses <- factor(data$ses,
                   unique(data$ses)[c(5, 4, 1, 2, 3)])

data$Edad <- as.numeric(data$Edad)


data %>%
  group_by(candidato) %>%
  summarise(avg = mean(perc/100),
            sd = sd(perc/100),
            n = n()) %>%
  ggplot() +
  aes(reorder(candidato, avg), avg,
      fill= candidato,
      label = scales::percent(avg, accuracy = .1)) +
  guides(fill = "none") +
  geom_errorbar(aes(ymin = avg-.01, ymax = avg + sd/sqrt(n) ) ) +
  geom_col() +
  geom_text(hjust = 1.1, fontface = "bold", size = 10) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        title = element_text(size = 25)) +
  labs(x="",
       y="",
       title = "Porcentaje promedio estimado por candidato",
       subtitle = "n = 134")


data %>%
  na.omit() %>%
  group_by(tendpol, candidato) %>%
  summarise(avg = mean(perc/100)) %>%
  ggplot()+
  aes(tendpol, reorder(candidato, avg), fill = avg) +
  geom_tile() +
  scale_fill_viridis_c(labels = scales::percent) +
  # scale_fill_gradient(labels = scales::percent) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        title = element_text(size = 25),
        legend.text = element_text(size = 15)) +
  labs(x="",
       y="",
       fill = "",
       title = "Porcentaje promedio estimado por candidato por tendencia política")


data %>%
  na.omit() %>%
  group_by(ses, candidato) %>%
  summarise(avg = mean(perc/100)) %>%
  ggplot()+
  aes(ses, reorder(candidato, avg), fill = avg) +
  geom_tile() +
  scale_fill_viridis_c(labels = scales::percent) +
  # scale_fill_gradient(labels = scales::percent) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        title = element_text(size = 25),
        legend.text = element_text(size = 15)) +
  labs(x="",
       y="",
       fill = "",
       title = "Porcentaje promedio estimado por candidato por nivel socioeconómico")


data %>%
  na.omit() %>%
  ggplot() +
  aes(Edad, perc/100, color = candidato) +
  guides(color = "none") +
  geom_jitter() +
  facet_wrap(.~candidato) +
  geom_smooth(method = "gam") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 25),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 20)) +
  labs(x="",
       y="", 
       title = "% estimado por edad")




data$Comuna <- toupper(data$Comuna)
data$Comuna2 <- iconv(data$Comuna, to="ASCII//TRANSLIT", from = "UTF-8")


data %>%
  na.omit() %>%
  group_by(Comuna2) %>%
  count() %>%
  ggplot() +
  aes(reorder(Comuna2, -n), n, fill = Comuna2) +
  guides(fill = "none") +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  labs(x="",
       y="N",
       title = "Cantidad de gente por comuna")

data %>%
  na.omit() %>%
  group_by(tendpol) %>%
  count() %>%
  ggplot() +
  aes(tendpol, n, fill = tendpol) +
  guides(fill = "none") +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  labs(x="",
       y="N",
       title = "Cantidad de gente por tendencia")

data %>%
  na.omit() %>%
  group_by(ses) %>%
  count() %>%
  ggplot() +
  aes(ses, n, fill = ses) +
  guides(fill = "none") +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  labs(x="",
       y="N",
       title = "Cantidad de gente por nivel socioecon")









