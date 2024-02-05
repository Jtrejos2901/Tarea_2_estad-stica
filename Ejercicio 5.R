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
n_p_30 <- c(0)

n_p_30_function <- function(px) {

  for (i in 1:length(px)) {
    resultado <-1
    for(j in 1: i){
      resultado <- resultado*px[j]
    }
    
    n_p_30[i] <- resultado
  }
  
  return(n_p_30)
}

n_p_30 <- n_p_30_function(px)

datos$n_p_30 <- n_p_30 


suma_asegurada_1 <- 10^6
suma_asegurada_2 <- 2*10^6

#se calculan los pagos esperados para cada año
qx<- datos$qx
pago_esperado <- c(0)
pago_esperado[1] <- suma_asegurada_2*qx[1]

#caso fallecimiento antes de los 60 años
for (i in 2: 29 ) {
    pago_esperado[i] <- suma_asegurada_2*n_p_30[i-1]*qx[i]
}

#caso sobrevive a los 60 años

pago_esperado[30] <-suma_asegurada_1*n_p_30[30]

#caso fallecimiento después de los 60 años

for (i in 1: (length(px)-30)) {
  pago_esperado[30+i] <- suma_asegurada_1*n_p_30[30+i-1]*qx[30+i]
}

datos$"Pago Esperado" <- pago_esperado
 
 
     
