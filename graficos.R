




source("global.R")



datos |> 
  mutate(estado = toupper(estado)) |> 
  mutate(
    escala_ing = case_when(
      ingreso <= 10000 ~ "Bajo",
      ingreso > 10000 & ingreso <= 30000 ~ "Medio",
      ingreso > 30000 & ingreso <= 70000 ~ "Alto",
      ingreso > 70000 ~ "Muy alto"),
    conflicto = if_else(conflicto == TRUE, "Con conflicto de interés",
                        "Sin conflicto de interés"
                        )
    ) |> 
  group_by(estado, escala_ing) |>
  summarise(
    n = n()
  ) |> 
  group_by(estado) |>
  summarise(
    `Porcentaje` = n/sum(n),
    Clasificación = escala_ing
  )



datos |> 
  filter(fecha > "2021-04-01") |> 
  group_by(fecha) |> 
  summarise(
    n = n()
  ) |> 
  e_charts(fecha) |> 
  e_line(n, symbol = "none")


datos_full |> 
  select(
    nombre_comp, fecha,`Fecha sanción`,ingreso_mensual,
    correo,`Dependencia contratación`, Dependencia,
    `Puesto contratación`, Puesto, `Autoridad sancionadora`,
    `Tipo de falta`, Sanción, `Plazo sanción`, `Multa-moneda`,
    Multa, estado
  ) |> 
  tidyr::replace_na(
    list(
      `Dependencia contratación` = "Sin información",
      Dependencia = "Sin información",
      `Puesto contratación` = "Sin información"
    )
  )
  
  
  
  
  
  
  
  

