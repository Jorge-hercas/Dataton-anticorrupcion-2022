
tabla <- tibble(
  id = c(),
  Nombre = c(),
  Apellido = c(),
  `Segundo apellido` = c(),
  Dependencia = c(),
  Puesto = c()
)







for (i in 1:length(list.files("/Users/jorge_hca/Downloads/s2"))){
  
  x <- length(list.files(paste0("/Users/jorge_hca/Downloads/s2/",list.files("/Users/jorge_hca/Downloads/s2")[i] )))
  
  for (j in 1:x){
    
    y <- paste0("/Users/jorge_hca/Downloads/s2/",list.files("/Users/jorge_hca/Downloads/s2")[i],"/",list.files(paste0("/Users/jorge_hca/Downloads/s2/",list.files("/Users/jorge_hca/Downloads/s2")[i]))[j])
    
    data <- jsonlite::read_json(y)
    
    for (k in 1:length(data)){
      
      tryCatch({
        
        
        tabla <- 
          tibble(
            id = data[[k]]$id,
            Nombre = data[[k]]$nombres,
            Apellido = data[[k]]$primerApellido,
            `Segundo apellido` = data[[k]]$segundoApellido,
            Dependencia = data[[k]]$institucionDependencia$nombre,
            Puesto = data[[k]]$puesto$nombre
          ) |> 
          bind_rows(tabla)
        
      }, error=function(e){
        
        
        
      })
      
      
      
    }
    
    
  } 
  
}





vroom::vroom_write(tabla, file = "/Users/jorge_hca/Downloads/puestos_fun.csv")











