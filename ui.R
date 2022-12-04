



  bootstrapPage(
    
    
    htmlTemplate(filename = "www/index.html",
                 explorador = fluidPage(
                   column(
                     width = 4,
                     material_card(title = strong("Filtros disponibles:"),
                                   pickerInput(label = "Estado:", inputId = "variable_fin", choices = unique(base_color$ESTADO),
                                               multiple = T,
                                               options = opts,
                                               selected = unique(base_color$ESTADO)
                                               
                                   ),
                                   pickerInput(label = "Clasificación de ingreso:", inputId = "tipo_ing", choices = unique(datos$clasif_ing),
                                               multiple = T,
                                               options = opts,
                                               selected = unique(datos$clasif_ing)
                                               
                                   ),
                                   airDatepickerInput(inputId = "date",
                                                      label = "Escoge un rango de fechas a visualizar:",
                                                      range = T,
                                                      value = c(min(unique(datos$fecha)),
                                                                max(unique(datos$fecha))
                                                                )
                                                      ),
                                   prettySwitch(
                                     inputId = "conflicto",
                                     label = "¿Solo funcionarios con conflicto de interés?", 
                                     value = F,
                                     status = "success",
                                     fill = T
                                   ),
                                   reactableOutput("tabla_ej")
                                   
                     )
                   ),
                   column(width = 6,
                          mapboxerOutput("mapa1", width = 650, height = 820)
                   ),
                   column(width = 2,
                          tags$div(
                            style = "margin-left: 105px; margin-right: -160px", offset = 0,
                            material_card(
                              title = strong("Información estadística"),
                              br(),
                              echarts4rOutput("barras_ingreso", height = 250),
                              echarts4rOutput("dona", height = 250),
                              echarts4rOutput("tserie", height = 280)
                            ))
                          
                   )
                   
                 ),
                 
                 panel_gen = fluidPage(
                   chooseSliderSkin("Flat"),
                   setSliderColor("gray", 1),
                   column(width = 3,
                          material_card(title = strong("Algunos filtros:"),
                                        pickerInput(label = "Estado:", inputId = "estado_cor", choices = opts_cor,
                                                    multiple = T,
                                                    options = opts,
                                                    selected = opts_cor
                                                    
                                        ),
                                        sliderInput(
                                          inputId = "slider",
                                          label = "Ingresos máximos a ver:",
                                          min = min(datos$ingreso_mensual),
                                          max = max(datos$ingreso_mensual),
                                          value = max(datos$ingreso_mensual)
                                        ),
                                        br(),
                                        strong("Casos de riesgo de corrupción por estado"),
                                        reactableOutput("generales_tabla_cor"),
                                        echarts4rOutput("grafico_cor", height = 250)
                                        )
                   ),
                   column(width = 9,
                          column(
                            width = 12,
                            mapboxerOutput("other_map1", width = 760, height = 415)
                          ),
                          column(width = 12,
                                 material_card(title = "",
                                               reactableOutput("tabla_cor")
                                               
                                               )
                          )
                   )
                 ),
                 
                 reporte = downloadButton("report", "Generate report"),
                 latex = print(katexR::katex("Calificacion = (\\frac{I_e}{max (I_e)}+C_i)*100"))
    )
    
    
  )
