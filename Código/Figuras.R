library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(dslabs)
library(showtext)
library(ggthemes)
library(scales)
library(ggtext)
library(gghighlight)


datos_protestas <- read_csv("Datos/datos-protestas.csv")

# Grafico 1 ----

font_add_google(name = "Lato", family = "Lato")
showtext_auto()

graph1 <- datos_protestas %>% 
  group_by(Mes, Pais) %>% 
  summarize(cantidad = length(Tipo_de_evento))

hex <- hue_pal()(10)
hex[1] <- "#52fff1"
hex[3] <- "#00ab1f"
hex[4] <- "#ff4545"
hex[5] <- "#fff305"
hex[7] <- "#0000FF33"
hex[8] <- "#9999FF33"
hex[9] <- "#FF33E633"
hex[2] <- "#FFB38033"
hex[6] <- "#00B3B333"

graph11 <- graph1 |> 
  filter(Pais %in% c("Chile", "Argentina", "Peru", "Bolivia", "Ecuador",
                     "Colombia", "Venezuela", "Uruguay", "Paraguay", "Brazil")) |> 
  ggplot(aes(Mes, cantidad, color = Pais)) +
  geom_line(size = 1.5) +
  #scale_color_colorblind() +
  scale_color_manual(values = hex) +
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
    axis.text = element_text(size = 20), 
    axis.title.y = element_text(size = 20),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    panel.grid.major.y = element_line(color = "gray40"),
    panel.grid.minor.y = element_line(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  ) 
ggsave("Figuras/grafico_1.png")

# Gráfico 2 ----

font_add_google(name = "Lato", family = "Lato")
showtext_auto()

graph1 <- datos_protestas %>% 
  group_by(Mes, Pais) %>% 
  summarize(cantidad = length(Tipo_de_evento))

hex <- hue_pal()(10)
hex[1] <- "#00ab1f"
hex[2] <- "#ff4545"
hex[3] <- "#fff305"
hex[4] <- "#FF62BC"


graph11 <- graph1 |> 
  filter(Pais %in% c("Chile",
                     "Colombia", "Venezuela", "Brazil")) |> 
  ggplot(aes(Mes, cantidad, color = Pais)) +
  geom_line(size = 1.5) +
  #scale_color_colorblind() +
  scale_color_manual(values = hex) +
  labs(title = "Cantidad de manifestaciones por mes",
       x = NULL,
       y = "Cantidad de manifestaciones",
       color = "país") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b\n%Y") +
  coord_cartesian(ylim=c(0,1250))

graph11 + 
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(size = 24),
    axis.text = element_text(size = 20), 
    axis.title.y = element_text(size = 20),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    panel.grid.major.y = element_line(color = "gray40"),
    panel.grid.minor.y = element_line(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  ) 
ggsave("Figuras/grafico_2.png")

# Grafico 3 ----

muerte1 <- datos_protestas |> 
  filter(Muertos > 0) |> 
  group_by(Pais) |> 
  summarize(Muertos = sum(Muertos)) |> 
  arrange(desc(Muertos))

muerte1 |> 
  ggplot(aes(reorder(Pais, -Muertos), Muertos)) +
  geom_col(fill = "#00d1ae") +
  geom_text(aes(label = Muertos),
            vjust = 1.5,
            size = 7,
            color = "white",
            fontface = "bold") +
  labs(title = "Cantidad de muertos en manifestaciones por país",
       subtitle = "entre 2018 y 2021",
       x = NULL,
       y = "Muertes") +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(size = 24),
    axis.text = element_text(size = 20), 
    axis.title.y = element_text(size = 20),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    #plot.background = element_rect(fill = "#f3fafd"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  ) 

ggsave("Figuras/grafico_3.png")


# Grafico 4 ----

muerte2 <- datos_protestas |> 
  filter(Pais %in% c("Chile",
                     "Colombia", "Venezuela", "Brazil")) |>
  filter(Muertos > 0) |> 
  group_by(Mes, Pais) |> 
  filter(Muertos == max(Muertos)) |> 
  summarize(fyl = paste(Pais, "\n",substr(Mes, start = 1, stop = 7)),
            Muertos = Muertos) |> 
  arrange(desc(Muertos))

muerte2 <- head(muerte2, 10)

muerte2 |> 
  ggplot(aes(reorder(fyl, -Muertos), Muertos)) +
  geom_col(fill = "#d1a000") +
  geom_text(aes(label = Muertos),
            vjust = 1.5,
            size = 7,
            color = "white",
            fontface = "bold") +
  labs(title = "Cantidad de muertos en manifestaciones agrupados por mes y país",
       subtitle = "entre 2018 y 2021",
       x = NULL,
       y = "Cantidad de muertos") +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(size = 24),
    axis.text = element_text(size = 20), 
    axis.title.y = element_text(size = 20),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    #plot.background = element_rect(fill = "#f3fafd"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  ) 

ggsave("Figuras/grafico_4.png")

# Grafico 5_1234 ----

## Chile ----
font_add_google(name = "Lato", family = "Lato")
showtext_auto()

chiled <- datos_protestas %>% 
  filter(Pais %in% "Chile") |> 
  filter(Tipo_de_evento == "Disturbios") |> 
  group_by(Mes, Pais) %>% 
  summarize(Disturbios = length(Tipo_de_evento))

chilep <- datos_protestas %>% 
  filter(Pais %in% "Chile") |> 
  filter(Tipo_de_evento == "Protestas") |> 
  group_by(Mes, Pais) %>% 
  summarize(Protestas = length(Tipo_de_evento))

chile <-  chiled |> 
  left_join(chilep)



chilegraph <- ggplot(chile, aes(Mes)) + 
  geom_line(aes(y = Disturbios, colour = "Disturbios"), size = 1.5) + 
  geom_line(aes(y = Protestas, colour = "Protestas"), size = 1.5) +
  labs(title = "Cantidad de manifestaciones por mes en Chile",
       x = NULL,
       y = "Cantidad",
       color = "tipo:") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b\n%Y") +
  coord_cartesian(ylim=c(0,500))

chilegraph <- chilegraph + 
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(size = 24),
    axis.text = element_text(size = 20), 
    axis.title.y = element_text(size = 20),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    panel.grid.major.y = element_line(color = "gray40"),
    panel.grid.minor.y = element_line(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  ) 
ggsave("Figuras/grafico_5_1.png")

## Brasil ----
brazild <- datos_protestas %>% 
  filter(Pais %in% "Brazil") |> 
  filter(Tipo_de_evento == "Disturbios") |> 
  group_by(Mes, Pais) %>% 
  summarize(Disturbios = length(Tipo_de_evento))

brazilp <- datos_protestas %>% 
  filter(Pais %in% "Brazil") |> 
  filter(Tipo_de_evento == "Protestas") |> 
  group_by(Mes, Pais) %>% 
  summarize(Protestas = length(Tipo_de_evento))

brazil <-  brazild |> 
  left_join(brazilp)



brazilgraph <- ggplot(brazil, aes(Mes)) + 
  geom_line(aes(y = Disturbios, colour = "Disturbios"), size = 1.5) + 
  geom_line(aes(y = Protestas, colour = "Protestas"), size = 1.5) +
  labs(title = "Cantidad de manifestaciones por mes en Brazil",
       x = NULL,
       y = "Cantidad",
       color = "tipo:") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b\n%Y") +
  coord_cartesian(ylim=c(0,1000))

brazilgraph <- brazilgraph + 
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(size = 24),
    axis.text = element_text(size = 20), 
    axis.title.y = element_text(size = 20),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    panel.grid.major.y = element_line(color = "gray40"),
    panel.grid.minor.y = element_line(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  )
ggsave("Figuras/grafico_5_2.png")

## Colombia ----

colombiad <- datos_protestas %>% 
  filter(Pais %in% "Colombia") |> 
  filter(Tipo_de_evento == "Disturbios") |> 
  group_by(Mes, Pais) %>% 
  summarize(Disturbios = length(Tipo_de_evento))

colombiap <- datos_protestas %>% 
  filter(Pais %in% "Colombia") |> 
  filter(Tipo_de_evento == "Protestas") |> 
  group_by(Mes, Pais) %>% 
  summarize(Protestas = length(Tipo_de_evento))

colombia <-  colombiad |> 
  left_join(colombiap)



colombiagraph <- ggplot(colombia, aes(Mes)) + 
  geom_line(aes(y = Disturbios, colour = "Disturbios"), size = 1.5) + 
  geom_line(aes(y = Protestas, colour = "Protestas"), size = 1.5) +
  labs(title = "Cantidad de manifestaciones por mes en Colombia",
       x = NULL,
       y = "Cantidad",
       color = "tipo:") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b\n%Y") +
  coord_cartesian(ylim=c(0,700))

colombiagraph <- colombiagraph + 
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(size = 24),
    axis.text = element_text(size = 20), 
    axis.title.y = element_text(size = 20),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    panel.grid.major.y = element_line(color = "gray40"),
    panel.grid.minor.y = element_line(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  ) 
ggsave("Figuras/grafico_5_3.png")

## Venezuela ----

venezuelad <- datos_protestas %>% 
  filter(Pais %in% "Venezuela") |> 
  filter(Tipo_de_evento == "Disturbios") |> 
  group_by(Mes, Pais) %>% 
  summarize(Disturbios = length(Tipo_de_evento))

venezuelap <- datos_protestas %>% 
  filter(Pais %in% "Venezuela") |> 
  filter(Tipo_de_evento == "Protestas") |> 
  group_by(Mes, Pais) %>% 
  summarize(Protestas = length(Tipo_de_evento))

venezuela <-  venezuelad |> 
  left_join(venezuelap)



venezuelagraph <- ggplot(venezuela, aes(Mes)) + 
  geom_line(aes(y = Disturbios, colour = "Disturbios"), size = 1.5) + 
  geom_line(aes(y = Protestas, colour = "Protestas"), size = 1.5) +
  labs(title = "Cantidad de manifestaciones por mes en Venezuela",
       x = NULL,
       y = "Cantidad",
       color = "tipo:") +
  scale_x_date(date_breaks = "4 month", date_labels = "%b\n%Y") +
  coord_cartesian(ylim=c(0,500))

venezuelagraph <- venezuelagraph + 
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(size = 24),
    axis.text = element_text(size = 20), 
    axis.title.y = element_text(size = 20),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    panel.grid.major.y = element_line(color = "gray40"),
    panel.grid.minor.y = element_line(color = "gray30"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    #plot.background = element_rect(fill = "#f3fafd"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_line(size = 1)
  ) 

ggsave("Figuras/grafico_5_4.png")


