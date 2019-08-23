library(tidyverse)
# Defino el cache/memoria de la funcion
max_n <- 50
max_m <- 50
max_k <- sum( (max_m+1):(max_n+max_m) )
memU <- array(dim = c(max_k, max_m, max_n))

# A falta del caracter 'Delta', usamos 'A'
A <- function(k, m, n) {
  # Devolver valores limites
  if (k < 0) { return(0) } # Regla A
  if (n == 0) { return(k==m*(m+1)/2) } # Regla B
  if (m == 0 | k == 0) { return(k==0 & m == 0) } # Regla C
  
  # De estar precomputado, devolver el valor
  if (!is.na(memU[k,m,n])) { return(memU[k,m,n]) }
  
  # Si no, calcularlo, cachearlo y devolverlo
  valor <- A(k-(m+n), m-1, n) + A(k, m, n-1)
  memU[k,m,n] <<- valor
  return(valor)
}

# En R, `dX` es la densidad de la distribucion X
dU <- function(x, m, n) {
  # Devuelve Pr(U = xi | n, m) para cada xi en x
  posibles <- map_dbl(x, A, m=m, n=n)
  totales <- choose(m+n, m)
  return(posibles / totales)
}

tablaU <- function(m, n) {
  tibble(
    # Dados n, m, el soporte de U esta dado por
    rango = (m*(m+1)/2):(m*n + m*(m+1)/2),
    probs = dU(rango, m, n))
}

tablaU(2,2)

tablaU(10, 15) %>%
  ggplot(aes(rango, probs)) +
  geom_col(width = 0.5)