# Paquetes ----

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


# Importar los datos ----
Sys.setlocale("LC_TIME", "English")

df <- read_csv("datos/datos-sin-procesar/2007-01-01-2022-01-01-South_America.csv")

datos_protestas <- df |> 
  select(country, event_date, year, event_type, fatalities) |> 
  filter(event_type %in% c("Protests", "Riots")) |> 
  filter(country %in% c("Chile", "Argentina", "Peru", "Bolivia", "Ecuador", "Colombia", "Venezuela", "Uruguay", "Paraguay", "Brazil")) |> 
  rename(Pais = country, Mes = event_date, Anio = year, Tipo_de_evento = event_type,
         Muertos = fatalities) |> 
  mutate(Mes = as.Date(paste("01", substr(Mes, 4, nchar(Mes))), "%d %B %Y")) |> 
  mutate(Tipo_de_evento = case_when(
    str_detect(Tipo_de_evento, "Riots") ~ "Disturbios",
    str_detect(Tipo_de_evento, "Protests") ~ "Protestas"
  ))


write_csv(datos_protestas, "datos/datos-protestas.csv")




# Protests: Public demonstrations in which the participants are not violent
# Riots: Violent events where demonstrators or mobs engage in destructive acts against 
#  property and/or disorganized acts of violence against people
