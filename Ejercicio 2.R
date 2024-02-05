#----------------------------Ejercicio 2---------------------------------------

#---Estimación de función de distribución mediante Muestreo por Importancia----


set.seed(2901)
n <- 10^4 #tamaño de la muestra

X <- rnorm(n, 0.5, sqrt(0.5))

f <- dnorm(X, 0.5, sqrt(0.5))

valor_estimado_1 <- mean(f) #La integración por Montecarlo Converge lento


#Se emplea como densidad auxiliar una exponencial truncada en 5 con lambda = 1

A <- rexp(n)+5 #datos aleatorios con distribución exponencial truncada


w <- dnorm(A, 0.5, sqrt(0.5)) / dexp(A-5)

valor_estimado <- mean(w)

valor_real <- pnorm(-5, 0.5, sqrt(0.5))

