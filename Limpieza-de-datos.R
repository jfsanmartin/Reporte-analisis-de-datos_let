# Paquetes ----

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)


# Importar los datos ----

df <- read_csv("datos/datos-sin-procesar/2007-01-01-2022-01-01-South_America.csv")

datos_protestas <- df |> 
  select(country, event_date, year, event_type, source_scale, fatalities, iso3) |> 
  filter(event_type %in% c("Protests", "Riots")) |> 
  rename(Pais = country, Mes = event_date, Anio = year, Tipo_de_evento = event_type,
         Escala = source_scale, Muertos = fatalities, ISO3 = iso3) |> 
  mutate(Mes = case_when(
    str_detect(Mes, "January") ~ "Enero",
    str_detect(Mes, "February") ~ "Febrero",
    str_detect(Mes, "March") ~ "Marzo",
    str_detect(Mes, "April") ~ "Abril",
    str_detect(Mes, "May") ~ "Mayo",
    str_detect(Mes, "June") ~ "Junio",
    str_detect(Mes, "July") ~ "Julio",
    str_detect(Mes, "August") ~ "Agosto",
    str_detect(Mes, "September") ~ "Septiembre",
    str_detect(Mes, "October") ~ "Octubre",
    str_detect(Mes, "November") ~ "Noviembre",
    str_detect(Mes, "December") ~ "Diciembre",)) |> 
  mutate(Tipo_de_evento = case_when(
    str_detect(Tipo_de_evento, "Riots") ~ "Disturbios",
    str_detect(Tipo_de_evento, "Protests") ~ "Protestas"
  ))


write_csv(datos_protestas, "datos/datos-protestas.csv")




# Protests: Public demonstrations in which the participants are not violent
# Riots: Violent events where demonstrators or mobs engage in destructive acts against 
#  property and/or disorganized acts of violence against people
