


library(shinybusy)
library(shiny)
library(echarts4r)
library(dplyr)
library(zoo)
library(shinymaterial)
library(grapher)
library(leaflet)
library(quantmod)
library(shinyWidgets)
library(reactable)
library(mapboxer)
library(ggnewscale)
library(ggmap)


options(scipen = 999)

style <- "mapbox://styles/mapbox/dark-v11"
key <- "AIzaSyC-L1JqEFQyWuwMiflGyA-8HRMi8K31noM"
token_mapa <- "pk.eyJ1Ijoiam9yZ2VoZGV6MTk5OCIsImEiOiJja2o2dnZzdmowemRsMzNueW5zNmJ6ZmdoIn0.s3BJeDpXW5GMy2Kln139Eg"


register_google(key)

locs <- vroom::vroom("data/locs_fin.csv")
datos <- vroom::vroom("data/datos.csv")
puestos_f <- vroom::vroom("data/puestos_fun.csv")
sanciones <- vroom::vroom("data/sanciones.csv")
estados <- sf::read_sf("data/México_Estados/México_Estados.shp")
id <- "mexico"
id_cor <- "mx_cor"

datos <- 
  datos |> 
  mutate(
    clasif_ing = case_when(
      ingreso <= 10000 ~ "Bajo",
      ingreso > 10000 & ingreso <= 30000 ~ "Medio",
      ingreso > 30000 & ingreso <= 50000 ~ "Alto",
      ingreso > 50000 ~ "Muy alto")
  ) 
  


locs <- 
  locs |> 
  left_join(
    datos |> 
      group_by(zona) |> 
      summarise(ingreso =  mean(ingreso, na.rm = T)),
    by = c("zona" = "zona")) |> 
  mutate(
    color = case_when(
      ingreso <= 10000 ~ "Bajo",
      ingreso > 10000 & ingreso <= 30000 ~ "Medio",
      ingreso > 30000 & ingreso <= 50000 ~ "Alto",
      ingreso > 50000 ~ "Muy alto"))

locs <-
  locs |> 
  left_join(
    datos[!duplicated(datos$zona),] |> 
      select(zona, estado),
    by = c("zona"= "zona" ) 
  ) |> 
  mutate(
    Estado = toupper(estado)
  )



base_color <-
  estados |>
  mutate(
    ESTADO = tolower(ESTADO)
  ) |>
  left_join(
    datos |>
      group_by(estado) |>
      summarise(
        ingreso_medio = mean(ingreso_mensual, na.rm = TRUE)
      ) |>
      filter(
        estado != ","
      ),
    by = c("ESTADO" = "estado")
  )

elecciones <- unique(base_color$ESTADO)

opts <- list(
  `actions-box` = TRUE,
  `live-search`=TRUE)

opts_cor <- toupper(na.omit(unique(datos$estado)))


source("functs.R")





