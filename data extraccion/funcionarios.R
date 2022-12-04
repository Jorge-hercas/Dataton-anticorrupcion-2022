




data <- jsonlite::read_json("/Users/jorge_hca/Downloads/s1/BajaCaliforniaSur/data-0000000001.json")


tabla <- 
  data.frame(
    id = c(),
    fecha = c(),
    ingreso =c(),
    moneda =c(),
    nombre =c(),
    apellido =c(),
    apellido2 =c(),
    correo = c(),
    conflicto = c(),
    tipo_ingreso = c(),
    zona = c()
  )






for (i in 1:length(list.files("/Users/jorge_hca/Downloads/s1"))){
  
  
  x <- length(list.files(paste0("/Users/jorge_hca/Downloads/s1/",list.files("/Users/jorge_hca/Downloads/s1")[i] )))
  
  for (j in 1:x){
    
    y <- paste0("/Users/jorge_hca/Downloads/s1/",list.files("/Users/jorge_hca/Downloads/s1")[i],"/",list.files(paste0("/Users/jorge_hca/Downloads/s1/",list.files("/Users/jorge_hca/Downloads/s1")[i]))[j])
    
    data <- jsonlite::read_json(y)
    
    for (k in 1:length(data)){
      
      
      
      tryCatch({
        if (is.null(data[[k]]$declaracion$situacionPatrimonial$ingresos$remuneracionAnualCargoPublico$valor) == TRUE){
          
          tabla <- 
            data.frame(
              id = data[[k]]$id,
              fecha = as.Date(data[[k]]$metadata$actualizacion),
              ingreso = data[[k]]$declaracion$situacionPatrimonial$ingresos$remuneracionMensualCargoPublico$valor,
              moneda =data[[k]]$declaracion$situacionPatrimonial$ingresos$remuneracionMensualCargoPublico$moneda,
              nombre =data[[k]]$declaracion$situacionPatrimonial$datosGenerales$nombre,
              apellido =data[[k]]$declaracion$situacionPatrimonial$datosGenerales$primerApellido,
              apellido2 =data[[k]]$declaracion$situacionPatrimonial$datosGenerales$segundoApellido,
              correo = data[[k]]$declaracion$situacionPatrimonial$datosGenerales$correoElectronico$institucional,
              conflicto = data[[k]]$metadata$actualizacionConflictoInteres,
              tipo_ingreso = "MENSUAL",
              zona = paste(sep = ", ",
                           data[[k]]$declaracion$situacionPatrimonial$datosEmpleoCargoComision$domicilioMexico$calle,
                           data[[k]]$declaracion$situacionPatrimonial$datosEmpleoCargoComision$domicilioMexico$coloniaLocalidad,
                           data[[k]]$declaracion$situacionPatrimonial$datosEmpleoCargoComision$domicilioMexico$municipioAlcaldia$valor,
                           data[[k]]$declaracion$situacionPatrimonial$datosEmpleoCargoComision$domicilioMexico$entidadFederativa$valor
                           
              )
            ) |> 
            bind_rows(tabla)
          
          
        }else{
          
          tabla <- 
            data.frame(
              id = data[[k]]$id,
              fecha = as.Date(data[[k]]$metadata$actualizacion),
              ingreso = data[[k]]$declaracion$situacionPatrimonial$ingresos$remuneracionAnualCargoPublico$valor,
              moneda =data[[k]]$declaracion$situacionPatrimonial$ingresos$remuneracionAnualCargoPublico$moneda,
              nombre =data[[k]]$declaracion$situacionPatrimonial$datosGenerales$nombre,
              apellido =data[[k]]$declaracion$situacionPatrimonial$datosGenerales$primerApellido,
              apellido2 =data[[k]]$declaracion$situacionPatrimonial$datosGenerales$segundoApellido,
              correo = data[[k]]$declaracion$situacionPatrimonial$datosGenerales$correoElectronico$institucional,
              conflicto = data[[k]]$metadata$actualizacionConflictoInteres,
              tipo_ingreso = "ANUAL",
              zona = paste(sep = ", ",
                           data[[k]]$declaracion$situacionPatrimonial$datosEmpleoCargoComision$domicilioMexico$calle,
                           data[[k]]$declaracion$situacionPatrimonial$datosEmpleoCargoComision$domicilioMexico$coloniaLocalidad,
                           data[[k]]$declaracion$situacionPatrimonial$datosEmpleoCargoComision$domicilioMexico$municipioAlcaldia$valor,
                           data[[k]]$declaracion$situacionPatrimonial$datosEmpleoCargoComision$domicilioMexico$entidadFederativa$valor
                           
              )
            ) |> 
            bind_rows(tabla)
          
        }
      }, error=function(e){
        
       
        
      })
      
     
      
    }
    
  } 
  
  
}



vroom::vroom_write(tabla, file = "/Users/jorge_hca/Downloads/datos.csv")
tabla <- vroom::vroom("/Users/jorge_hca/Downloads/datos.csv")


library(ggmap)
key <- "AIzaSyC-L1JqEFQyWuwMiflGyA-8HRMi8K31noM"
register_google(key)



zonas <- 
unique(tabla$zona) |> 
  as.data.frame() |>
  setNames("zona") 


zonas_fin <- 
  data.frame(
    zona = c(),
    lat = c(),
    lng = c()
  )



for (i in 29857:length(zonas$zona)){
  
  val <- 
    zonas$zona[i] |> 
    as.data.frame() |> 
    setNames("zona") |> 
    mutate(
      lat =geocode(zona)$lat,
      lng = geocode(zona)$lon
    )
  
  
  zonas_fin <- 
    zonas_fin |> 
    bind_rows(val)
  
  
  
} 


