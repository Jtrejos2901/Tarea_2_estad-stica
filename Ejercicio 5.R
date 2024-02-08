#-----------------------Ejercicio 5---------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)

#Se carga la base de datos
tabla_vida <- read_excel("tavid2000-2150.xls",
                         col_types = "numeric")

#Se filtra la base de datos para obtener los datos de un hombre nacido en 1994 
#con edades mayor o igual a 30

datos <- subset(tabla_vida, sex == 1 & ynac == 1994 & edad >=30, select = c(edad,qx, year))

#Se obtienen las probabilidades de sobrevivencia

px <- 1- datos$qx

#Se añaden las probabilidades de sobrevivencia a la base datos
datos$px <- px

qx<- datos$qx
suma_asegurada_1 <- 10^6
suma_asegurada_2 <- 2*10^6

#--------------------MCMC---------------------------------|   

#Se simulan diversas trayectorias de vida de la persona
set.seed(2901)
iteraciones=10^4
n=length(px)
pago <- rep(0, 86)

for (i in 1:iteraciones) {
  U <- runif(n)  # Se toman como probabilidades de muerte
  t <- 1
  cont <- 1
  
  #Determinación del año de fallecimiento
  while (t == 1) {
    if (U[cont] < px[cont]) {
      cont <- cont + 1
    } else {
      t <- 0
    }
  }
  año_fallecimiento <- cont - 1
  
  #Asignar los pagos correspondientes al año de fallecimiento
  if (año_fallecimiento < 30) {
    pago[año_fallecimiento + 1] <- pago[año_fallecimiento + 1] + suma_asegurada_2 
  } else if (año_fallecimiento ==30) {
    pago[31] <- pago[31]+ suma_asegurada_1
  }else {
    pago[31] <- pago[31] + suma_asegurada_1 
    pago[año_fallecimiento + 1] <- pago[año_fallecimiento + 1] + suma_asegurada_1
  }
}

resultado <- data.frame("Años pago"= datos$year, "Pago" = pago)

ggplot(data = resultado, aes(x = Años.pago, y = Pago)) +  
  geom_bar(stat = "identity", fill = "blue") +  
  labs(title = "Histograma de Pagos Esperados por año", x = "Años", y = "Frecuencia")+
  theme_minimal()

#----- Método determinista-----------------------------------------------------|

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


#se calculan los pagos esperados para cada año
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

resultado_determinista <- data.frame("Años pago"= datos$year, "Pago" = pago_esperado)

ggplot(data = resultado_determinista, aes(x = Años.pago, y = Pago)) +  
  geom_bar(stat = "identity", fill = "blue") +  
  labs(title = "Histograma de Pagos Esperados por año", x = "Años", y = "Frecuencia")+
  theme_minimal()
