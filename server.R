

function(input, output, session) {
  
  
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path("report/report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(Estado = input$estado_cor,
                     ingreso = input$tipo_ing,
                     fecha = input$date,
                     conf = input$conflicto
                     
      )
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  conflicto <- reactive(
    if (input$conflicto == T){
      c(T)
    }else{
      c(T,F)
    }
  )
  
  output$mapa1 <- renderMapboxer({
    est_selec <- base_color |> 
      set_color()
    mapboxer(
      attributionControl = FALSE,
      style = style, token = token_mapa,
      bounds = sf::st_bbox(est_selec)) |> 
      add_source(as_mapbox_source(est_selec), id) |> 
      add_fill_layer(
        fill_opacity = 0.75,
        fill_sort_key = 1,
        fill_outline_color = "gray",
        fill_color = c("get", "color"),
        source = id
        #,popup = "<p><strong>Valor total:</strong> {{valor_mon}}</p>"
      ) |> 
      add_text_control(
        text = "<h2><strong>Ingreso promedio de funcionarios por estado</strong></h2>",
        pos = "top-left",
        css_text = "background: transparent; color: white"
        
      )
  })
  
  
  est_selec <- reactive({
    
    req(conflicto())
    
    estados |>
      mutate(
        ESTADO = tolower(ESTADO)
      ) |>
      left_join(
        datos |>
          filter(clasif_ing %in% input$tipo_ing
                 & conflicto  %in% conflicto()
                 & fecha >= input$date[1]
                 & fecha <= input$date[2]
                 ) |> 
          group_by(estado) |>
          summarise(
            ingreso_medio = mean(ingreso_mensual, na.rm = TRUE)
          ) |>
          filter(
            estado != ","
          ),
        by = c("ESTADO" = "estado")
      ) |> 
      tidyr::replace_na(list(ingreso_medio = 0)) |> 
      set_color(input$variable_fin)
    
  })
  
  
  observeEvent(list(input$variable_fin,
                    input$tipo_ing,
                    input$date,
                    input$conflicto
                    ), {
    
    
    req(est_selec())
    
    
    mapboxer_proxy("mapa1") |> 
      set_data(est_selec(), id) |> 
      fit_bounds(sf::st_bbox(est_selec() )) |> 
      update_mapboxer()
  }) 
  
  
  output$tabla_ej<- renderReactable({
    
    req( conflicto())
    x <-
    datos |> 
      mutate(
        Estado = toupper(estado),
        conflicto = if_else(conflicto == TRUE,1,0)
      ) |> 
      filter(clasif_ing %in% input$tipo_ing
             & conflicto  %in% conflicto()
             & fecha >= input$date[1]
             & fecha <= input$date[2]
             & estado %in% input$variable_fin
      ) |> 
      group_by(Estado) |> 
      summarise(
        `Ingreso promedio` = round(mean(ingreso_mensual, na.rm =T)),
        `Numero de funcionarios` = n(),
        `Con conflicto de interés` = sum(conflicto, na.rm = TRUE)
      ) |> 
      arrange(desc(`Ingreso promedio`)) |> 
      na.omit() 
    x |> 
      mutate(Estado = Hmisc::capitalize(tolower(Estado))) |> 
      reactable(theme = reactableTheme(backgroundColor = "transparent"),
                style = list(fontSize = 10),
                compact = T,
                defaultPageSize = 15,
                highlight = T,
                defaultColDef = colDef(format = colFormat(separators = T)),
                columns = list(
                  `Ingreso promedio` = colDef(
                    cell = reactablefmtr::icon_sets(x, icon_size = 12, icons = "dollar", colors = RColorBrewer::brewer.pal(name = "Greens",9),
                                                    number_fmt = scales::comma
                    )
                  )
                )
      )
    
  })
  
  
  
  output$barras_ingreso <- renderEcharts4r({
    
    req( conflicto())
    datos |> 
      filter(clasif_ing %in% input$tipo_ing
             & conflicto  %in% conflicto()
             & fecha >= input$date[1]
             & fecha <= input$date[2]
             & estado %in% input$variable_fin
      ) |> 
      group_by(clasif_ing) |> 
      summarise(
        total = n()
      ) |> 
      arrange(desc(total)) |> 
      e_charts(clasif_ing, dispose = F) |> 
      e_bar(total,
            name = "Tipo ingreso",
            label = list(
              textStyle = list(fontFamily = "Roboto Condensed", 
                               color = "gray",
                               fontSize = 10)
              )
            ) |> 
      e_tooltip(trigger = "axis",
                textStyle = list(fontFamily = "Roboto Condensed", 
                                 color = "gray",
                                 fontSize = 10)
                ) |> 
      e_theme("auritus") |> 
      e_color(background = "transparent",
              color = "#3d5375"
      ) |> 
      e_title(
        "Funcionarios por tipo de ingreso",
        left = "center",
        textStyle = list(
          color = "gray",
          fontFamily = "Roboto Condensed",
          fontSize = 12
        )
      ) |> 
      e_text_style(
        fontFamily = "Roboto Condensed", 
        color = "gray",
        fontSize = 10
      ) |> 
      e_legend(right = 1,bottom = 1, orient = "vertical",textStyle = list(fontFamily = "Roboto Condensed", 
                                                                          color = "gray",
                                                                          fontSize = 12))
    
    
  })
  
  output$dona <- renderEcharts4r({
    
    req( conflicto())
    datos |> 
      filter(clasif_ing %in% input$tipo_ing
             & conflicto  %in% conflicto()
             & fecha >= input$date[1]
             & fecha <= input$date[2]
             & estado %in% input$variable_fin
      ) |> 
      group_by(conflicto) |> 
      summarise(
        total = n()
      ) |> 
      mutate(
        percent = total/sum(total),
        conflicto = case_when(
          conflicto == F ~ "Sin conflicto",
          T ~ "Con conflicto"
        )
      ) |> 
      e_charts(conflicto, dispose = F) |> 
      e_pie(percent, radius = c("50%", "70%"), label = list(show = F)) |> 
      e_tooltip(formatter = e_tooltip_choro_formatter("percent"),
                textStyle = list(fontFamily = "Roboto Condensed", 
                                 color = "gray",
                                 fontSize = 10)
                ) |> 
      e_theme("auritus") |> 
      e_color(background = "transparent",
              color = c("#3d5375","#75423d")
      ) |> 
      e_title(
        "Funcionarios con conflicto de interés",
        left = "center",
        textStyle = list(
          color = "gray",
          fontFamily = "Roboto Condensed",
          fontSize = 12
        )
      ) |> 
      e_text_style(
        fontFamily = "Roboto Condensed", 
        color = "gray",
        fontSize = 10
      ) |> 
      e_legend(right = 1,bottom = 1, orient = "vertical",textStyle = list(fontFamily = "Roboto Condensed", 
                                                                          color = "gray",
                                                                          fontSize = 12))
    
    
  })
  
  
  output$tserie <- renderEcharts4r({
    
    
    datos |> 
      filter(clasif_ing %in% input$tipo_ing
             & conflicto  %in% conflicto()
             & fecha >= input$date[1]
             & fecha <= input$date[2]
             & estado %in% input$variable_fin
      ) |> 
      group_by(fecha) |> 
      filter(
        fecha >= "2021-02-01"
      ) |> 
      summarise(
        n = n()
      ) |> 
      e_charts(fecha) |> 
      e_line(n, symbol = "none", name = "Declaraciones") |> 
      e_tooltip(trigger = "axis",
                textStyle = list(fontFamily = "Roboto Condensed", 
                                 color = "gray",
                                 fontSize = 10)
      ) |> 
      e_theme("auritus") |> 
      e_color(background = "transparent",
              color = "#3d5375"
      ) |> 
      e_title(
        "Declaraciones fiscales por fecha",
        left = "center",
        textStyle = list(
          color = "gray",
          fontFamily = "Roboto Condensed",
          fontSize = 12
        )
      ) |> 
      e_text_style(
        fontFamily = "Roboto Condensed", 
        color = "gray",
        fontSize = 10
      ) |> 
      e_legend(right = 1,bottom = 1, orient = "vertical",textStyle = list(fontFamily = "Roboto Condensed", 
                                                                          color = "gray",
                                                                          fontSize = 12))
    
    
  })
  
  
  output$other_map1 <- renderMapboxer({
    val <-
    locs |> 
      mutate(
        color = scales::col_factor("Oranges", color)(color)
      ) 
    
    mapboxer(
      attributionControl = FALSE,
      style = "mapbox://styles/mapbox/light-v11", token = token_mapa,
      bounds = c(min(val$lng),
                 min(val$lat),
                 max(val$lng),
                 max(val$lat)) ) |> 
      add_source(as_mapbox_source(val), id_cor) |> 
      add_circle_layer(
        circle_color = c("get", "color"),
        source = id_cor
      )
    
  })
  
  
  observeEvent(input$estado_cor,{
    
    x <- locs |> 
      filter(Estado %in% input$estado_cor)
    
    
    
    
    mapboxer_proxy("other_map1") |> 
      set_data(x, id_cor) |> 
      fit_bounds(  
        c(min(x$lng),
          min(x$lat),
          max(x$lng),
          max(x$lat))
      ) |> 
      update_mapboxer()
    
    
    
  })
  
  
  
  
  
  
  output$tabla_cor <- renderReactable({
    
    datos_react <- datos |> 
      mutate(
        Estado = toupper(estado)
      ) |> 
      filter(
        Estado %in% input$estado_cor
        & ingreso_mensual <= input$slider
      )
    
    x <- 
      datos_react[datos_react$ingreso_mensual %in% boxplot.stats(datos_react$ingreso_mensual)$out,] |> 
      mutate(
        prob_ing = ingreso_mensual/max(ingreso_mensual),
        conflicto = if_else(conflicto == F, 0, 1),
        calif = (prob_ing + conflicto)*100
      ) |> 
      select(-one_of("zona", "tipo_ingreso", "clasif_ing",
                     "fecha", "id", "ingreso", "moneda",
                     "correo", "Estado"
      )) |> 
      mutate(
        estado = Hmisc::capitalize(estado),
        nombre = Hmisc::capitalize(tolower(nombre)),
        apellido = Hmisc::capitalize(tolower(apellido)),
        apellido2 = Hmisc::capitalize(tolower(apellido2)),
        ingreso_mensual = round(ingreso_mensual),
        prob_ing = round(prob_ing, digits = 3),
        calif = round(calif, digits = 3)
      ) 
    
    x |> 
      reactable(
        theme = reactableTheme(backgroundColor = "transparent"),
        defaultColDef = colDef(format = colFormat(separators = T)),
        style = list(fontSize = 10),
        compact = T,
        defaultPageSize = 8,
        highlight = T,
        columnGroups = list(
          colGroup(name = "Funcionario", columns = c("nombre", "apellido", "apellido2"))
        ),
        columns = list(
          nombre = colDef(name = "Nombre"),
          apellido = colDef(name = "Apellido"),
          apellido2 = colDef(name = "Seg. apellido"),
          conflicto = colDef(name = "Conflicto de interés"),
          estado = colDef(name = "Estado"),
          ingreso_mensual = colDef(name = "Ingreso mensual" ),
          prob_ing = colDef(name = "Prob. por ingreso"),
          calif = colDef(name = "Calificación")
        )
      )
    
    
    
  })
  
  
  output$generales_tabla_cor <- renderReactable({
    
    datos_react <- datos |> 
      mutate(
        Estado = toupper(estado)
      ) |> 
      filter(
        Estado %in% input$estado_cor
        & ingreso_mensual <= input$slider
      )
    
    
    x <- 
      datos_react[datos_react$ingreso_mensual %in% boxplot.stats(datos_react$ingreso_mensual)$out,] |> 
      mutate(
        prob_ing = ingreso_mensual/max(ingreso_mensual),
        conflicto = if_else(conflicto == F, 0, 1),
        calif = (prob_ing + conflicto)*100
      ) |> 
      select(-one_of("zona", "tipo_ingreso", "clasif_ing",
                     "fecha", "id", "ingreso", "moneda",
                     "correo", "Estado"
      )) |> 
      mutate(
        estado = Hmisc::capitalize(estado),
        nombre = Hmisc::capitalize(tolower(nombre)),
        apellido = Hmisc::capitalize(tolower(apellido)),
        apellido2 = Hmisc::capitalize(tolower(apellido2)),
        ingreso_mensual = round(ingreso_mensual),
        prob_ing = round(prob_ing, digits = 3),
        calif = round(calif, digits = 3)
      ) 
    
    x |> 
      filter(
        calif > 10 | calif < 0
      ) |> 
      group_by(
        estado
      ) |> 
      summarise(
        Total = n()
      ) |> 
      reactable(
        theme = reactableTheme(backgroundColor = "transparent"),
        defaultColDef = colDef(format = colFormat(separators = T)),
        style = list(fontSize = 10),
        compact = T,
        defaultPageSize = 8,
        highlight = T,
        columns = list(
          estado = colDef(name = "Estado")
        )
      )
    
    
    
    
  })
  
  
  
  output$grafico_cor <- renderEcharts4r({
    
    
    datos_react <- datos |> 
      mutate(
        Estado = toupper(estado)
      ) |> 
      filter(
        Estado %in% input$estado_cor
        & ingreso_mensual <= input$slider
      )
    
    
    x <- 
      datos_react[datos_react$ingreso_mensual %in% boxplot.stats(datos_react$ingreso_mensual)$out,] |> 
      mutate(
        prob_ing = ingreso_mensual/max(ingreso_mensual),
        conflicto = if_else(conflicto == F, 0, 1),
        calif = (prob_ing + conflicto)*100
      ) |> 
      select(-one_of("zona", "tipo_ingreso", "clasif_ing",
                     "fecha", "id", "ingreso", "moneda",
                     "correo", "Estado"
      )) |> 
      mutate(
        calif = round(calif, digits = 3)
      ) 
    
    x |> 
      filter(
        calif > 10 | calif < 0
      ) |> 
      group_by(
        estado
      ) |> 
      summarise(
        Total = n()
      ) |> 
      mutate(
        porcentaje = Total/ sum(Total)
      ) |> 
      e_charts(estado, dispose = F) |> 
      e_pie(porcentaje, radius = c("50%", "70%"), label = list(show = F)) |> 
      e_tooltip(formatter = e_tooltip_choro_formatter("percent"),
                textStyle = list(fontFamily = "Roboto Condensed", 
                                 color = "gray",
                                 fontSize = 10)
      ) |> 
      e_theme("auritus") |> 
      e_color(background = "transparent",
              color = c("#3d5375","#75423d")
      ) |> 
      e_title(
        "Funcionarios con mayor riesgo (%)",
        left = "center",
        textStyle = list(
          color = "gray",
          fontFamily = "Roboto Condensed",
          fontSize = 12
        )
      ) |> 
      e_text_style(
        fontFamily = "Roboto Condensed", 
        color = "gray",
        fontSize = 10
      ) |> 
      e_legend(F)
    
    
    
    
    
  })
  
  
}






