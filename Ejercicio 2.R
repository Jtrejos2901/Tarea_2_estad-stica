#----------------------------Ejercicio 2---------------------------------------

#---Estimación de función de distribución mediante Muestreo por Importancia----

set.seed(2901)
n <- 10^4 #tamaño de la muestra

X <- rnorm(n, 0.5, sqrt(0.5))

f <- dnorm(X, 0.5, sqrt(0.5))

valor_estimado_1 <- mean(f) #La integración por Montecarlo Converge lento

plot(density(X))

#Necesitamos densidad auxiliar que contenga a la densidad de la normal(0.5,0.5)

#Se emplea como densidad auxiliar una exponencial truncada en 6 con lambda = 1

A <- rexp(n)+6 #datos aleatorios con distribución exponencial truncada


w <- dnorm(A, 0.5, sqrt(0.5)) / dexp(A-6)

valor_estimado <- mean(w)

valor_real <- pnorm(-5, 0.5, sqrt(0.5))
 
error_absoluto <- abs(valor_estimado-valor_real)

resumen <- data.frame("Estimación" = valor_estimado, "Valor real" = valor_real, "Error absoluto" = error_absoluto)


