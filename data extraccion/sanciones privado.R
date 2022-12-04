




tabla <- tibble(
  id = c(),
  Fecha = c(),
  Dependencia = c(),
  `Nombre completo` = c(),
  `Autoridad sancionadora` = c(),
  `Tipo de falta` = c(),
  `Sanción` = c(),
  Multa = c(),
  `Multa-moneda` = c(),
)



for (i in 1:length(list.files("/Users/jorge_hca/Downloads/s3p"))){
  
  x <- length(list.files(paste0("/Users/jorge_hca/Downloads/s3p/",list.files("/Users/jorge_hca/Downloads/s3p")[i] )))
  
  for (j in 1:x){
    
    y <- paste0("/Users/jorge_hca/Downloads/s3p/",list.files("/Users/jorge_hca/Downloads/s3p")[i],"/",list.files(paste0("/Users/jorge_hca/Downloads/s3p/",list.files("/Users/jorge_hca/Downloads/s3p")[i]))[j])
    
    data <- jsonlite::read_json(y)
    
    for (k in 1:length(data)){
      
      tryCatch({
        
        
        tabla <- 
          tibble(
            id = data[[k]]$id,
            Fecha = data[[k]]$fechaCaptura,
            Dependencia = data[[k]]$institucionDependencia$nombre,
            `Nombre completo` = data[[k]]$particularSancionado$nombreRazonSocial,
            `Autoridad sancionadora` = data[[k]]$autoridadSancionadora,
            `Tipo de falta` = data[[k]]$tipoFalta,
            `Sanción` = data[[k]]$tipoSancion[[1]]$valor,
            Multa = data[[k]]$multa$monto,
            `Multa-moneda` = data[[k]]$multa$moneda$valor
          ) |> 
          bind_rows(tabla)
        
      }, error=function(e){
        
        
        
      })
      
      
      
    }
    
    
  } 
  
}



vroom::vroom_write(tabla, file = "data/sanciones_privado.csv")





