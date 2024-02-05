#-----------------------Ejercicio 5---------------------------------------------
library(readxl)
library(dplyr)

tabla_vida <- read_excel("tavid2000-2150.xls",
                         col_types = c("numeric", "numeric", "numeric", 
                         "numeric", "numeric", "numeric", "numeric"))

#Se filtra la base de datos para obtener los datos de un hombre nacido en 1994 
#con edades mayor o igual a 30

datos <- subset(tabla_vida, sex == 1 & ynac == 1994 & edad >=30, select = c(edad,year,qx))

#Se crea una función que obtiene las probabilidades de sobrevivencia

px_function <- Vectorize(function(x) 1- x)
px <- px_function(datos$qx)

#Se añaden las probabilidades de sobrevivencia a la base datos
datos$px <- px


#Se crea una función que obtiene n_p_30

n_p_30_function <- function(px) {

  n_p_30 <- c(0)
  
  for (i in 1:length(px)) {
    resultado <-1
    for(j in 1: i){
      resultado <- resultado*px[j]
    }
    
    n_p_30[i] <- resultado
  }
  
  return(n_p_30)
}

datos$n_p_30 <- n_p_30_function(px)



