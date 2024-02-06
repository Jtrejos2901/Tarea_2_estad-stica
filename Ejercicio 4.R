#Sea 𝑓(𝑥)=𝑠𝑒𝑛(𝑥+cos(10𝑥)3)𝑝𝑎𝑟𝑎 𝑥𝜖 [−2,2], (11 pts)
#a.Utilizando el algoritmo de recalentamiento simulado estime el mínimo global 
#en [−2,2], con valor inicial en 1.5

fx <- function(x){sin(x + (cos(10*x)/3))} 
curve(fx,col="violet",lwd=2,from=-2,to = 2,n=1000,ylab="f(x)")
title("Gráfico de la función")

recalentamiento_simulado <- function(f,alpha=0.5,s0=0,niter,mini=-Inf,maxi=Inf){ 
  s_n <- s0 
  estados <- rep(0,niter) 
  iter_count <- 0 
  
  for(k in 1:niter){ 
    estados[k]<-s_n 
    T <- (1-alpha)^k
    s_new <- rnorm(1,s_n,1)
    
    if(s_new<mini){
      s_new <- mini
    } 
    
    if(s_new>maxi){
      s_new <- maxi
    } 
    
    dif <- f(s_new)-f(s_n)
    
    if(dif< 0){ 
      s_n <- s_new 
    } else { 
        random <- runif(1,0,1) 
        
        if(random <exp(-dif/T)){ 
          s_n <- s_new 
        } 
        
    } 
    
    iter_count <- iter_count +1
    
  }
  
  return(list(r=s_n,e=estados)) 
}

Resultado <- recalentamiento_simulado(fx,0.1,1.5,1000,-2,2) 
Resultado$r

curve(fx,col="violet",lwd=2,from=-2,to = 2,n=1000,ylab="f(x)")
title("Gráfico y mínimo global de la función")
abline(v=Resultado$r,col="green")
text(Resultado$r, 0, "Mínimo", pos = 1, col = "green")

#b.Grafique el resultado de los estados donde estuvo la cadena de la estimación 
#del punto a.

plot(Resultado$e, xlab = "Iteraciones", ylab = "Valor (x)", 
     main = "Estados de la cadena")