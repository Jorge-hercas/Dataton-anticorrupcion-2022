



theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text( color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f7f7f7", color = NA), 
      panel.background = element_rect(fill = "#f7f7f7", color = NA), 
      legend.background = element_rect(fill = "#f7f7f7", color = NA),
      panel.border = element_blank(),
      ...
    )
}



set_color <- function(base_color, .ESTADO = elecciones ) {
  if (length(.ESTADO) == 1){
    base_color |> 
      filter(ESTADO %in% .ESTADO) |> 
      mutate(color = "#ad3a32" )
  }else{
    base_color |> 
      filter(ESTADO %in% .ESTADO) |> 
      tidyr::replace_na(list(ingreso_medio=0)) |> 
      mutate(color = scales::col_numeric("Reds", ingreso_medio, na.color = "gray")(ingreso_medio))
  }
}











