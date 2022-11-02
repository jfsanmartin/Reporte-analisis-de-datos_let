library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(dslabs)
library(showtext)
library(ggthemes)
library(scales)
library(ggtext) # para editar anotaciones y etiquetas
library(gghighlight) # para destacar valores en un gráfico

datos_protestas <- read_csv("Datos/datos-protestas.csv")

font_add_google(name = "Lato", family = "Lato")
showtext_auto()

graph1 <- datos_protestas %>% 
  group_by(Mes, Pais) %>% 
  summarize(cantidad = length(Escala))

graph11 <- graph1 |> 
  filter(Pais %in% c("Chile", "Argentina", "Peru", "Bolivia", "Ecuador",
                     "Colombia", "Venezuela", "Uruguay", "Paraguay", "Brazil")) |> 
  ggplot(aes(Mes, cantidad, color = Pais)) +
  geom_line(size = 2) +
  #scale_color_colorblind() +
  scale_color_manual(values = hue_pal()(10)) +
  labs(title = "Cantidad de manifestaciones por mes",
       x = NULL,
       y = "Cantidad de manifestaciones",
       color = "país") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b\n%Y") +
  coord_cartesian(ylim=c(0,1000))

graph11 + 
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(size = 24),
    axis.text = element_text(size = 15), 
    axis.title.y = element_text(size = 15),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 15),
    panel.grid.major.y = element_line(color = "gray40"),
    panel.grid.minor.y = element_line(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  ) 

