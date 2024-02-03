#Usando el Algoritmo de Metropolis-Hastings construya una muestra de 𝑍=𝑋1−𝑋2 
#donde 𝑋1~𝑁(𝜇,𝜎2) y 𝑋2~𝑁(𝜇/2,𝜎2/4), considere para este ejercicio 
#𝜇=𝜎2=4.

fnormal <- function(x,mu1,mu2,sigma1, sigma2) { 
  fx= exp(-((x-mu1)^2/(2*(sigma1)))) - exp(-((x-mu2)^2/(2*(sigma2)))) 
  return(fx) 
}

fZ <- function(x){return(fnormal(x,4,2,4,1))} 

# Valores para el rango de la gráfica
x_values <- seq(0, 16, length.out = 1000)

# Gráfico de la distribución de Z y las medias de X1 y X2
plot(x_values, fZ(x_values), type = "l", col = "blue", lwd = 2, 
     xlab = "Z", ylab = "Densidad", main = "Distribución de Z = X1 - X2")

# Líneas verticales para las medias de X1 y X2
abline(v = c(4, 2), col = c("red", "green"), lty = c(2, 2), lwd = 2)

# Etiquetas para las medias
text(4, 0.20, "Media X1", pos = 1, col = "red")
text(2, 0.10, "Media X2", pos = 1, col = "green")
