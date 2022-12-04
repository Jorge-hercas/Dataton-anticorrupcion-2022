


tabla <- tibble(
  id = c(),
  Fecha = c(),
  Dependencia = c(),
  Nombre = c(),
  Apellido = c(),
  `Segundo apellido` = c(),
  Puesto = c(),
  `Autoridad sancionadora` = c(),
  `Tipo de falta` = c(),
  `Sanción` = c(),
  Multa = c(),
  `Multa-moneda` = c(),
  `Plazo sanción` = c()
)



for (i in 1:length(list.files("/Users/jorge_hca/Downloads/s3s"))){
  
  x <- length(list.files(paste0("/Users/jorge_hca/Downloads/s3s/",list.files("/Users/jorge_hca/Downloads/s3s")[i] )))
  
  for (j in 1:x){
    
    y <- paste0("/Users/jorge_hca/Downloads/s3s/",list.files("/Users/jorge_hca/Downloads/s3s")[i],"/",list.files(paste0("/Users/jorge_hca/Downloads/s3s/",list.files("/Users/jorge_hca/Downloads/s3s")[i]))[j])
    
    data <- jsonlite::read_json(y)
    
    for (k in 1:length(data)){
      
      tryCatch({
        
        
        tabla <- 
          tibble(
            id = data[[k]]$id,
            Fecha = data[[k]]$fechaCaptura,
            Dependencia = data[[k]]$institucionDependencia$nombre,
            Nombre = data[[k]]$servidorPublicoSancionado$nombres,
            Apellido = data[[k]]$servidorPublicoSancionado$primerApellido,
            `Segundo apellido` = data[[k]]$servidorPublicoSancionado$segundoApellido,
            Puesto = data[[k]]$servidorPublicoSancionado$puesto,
            `Autoridad sancionadora` = data[[k]]$autoridadSancionadora,
            `Tipo de falta` = data[[k]]$tipoFalta$valor,
            `Sanción` = data[[k]]$tipoSancion[[1]]$valor,
            Multa = data[[k]]$multa$monto,
            `Multa-moneda` = data[[k]]$multa$moneda$valor,
            `Plazo sanción` = data[[k]]$inhabilitacion$plazo
          ) |> 
          bind_rows(tabla)
        
      }, error=function(e){
        
        
        
      })
      
      
      
    }
    
    
  } 
  
}



vroom::vroom_write(tabla, file = "/Users/jorge_hca/Downloads/sanciones.csv")





