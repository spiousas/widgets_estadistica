library(tidyverse)

# Vamos a asumir que tenemos un experimento con grupos independientes (between subject) y que la distribución
# de lo que estamos midiendo es normal. Por eso vamos a utiliza un t-test no pareado (test paramétrico).
set.seed(124)
alpha <- 0.05
d <- .3

# Los parámetros de ambas
sigma <- 10 # Asumamos igual varianza
mean1 <- 0
mean2 <- sigma * d

n <- seq(3, 100, 1)
reps <- 10000
p <- matrix(0, length(n), reps)

for (i in seq(1, length(n), 1)) {
  for (j in seq(1, reps, 1)) {
    sample1 <- rnorm(n[i], mean = mean1, sd = sigma)
    sample2 <- rnorm(n[i], mean = mean2, sd = sigma)
      
    # Calculo el pvalue
    p[i, j] <- t.test(sample1, sample2)$p.value
  
  }
}

decision <- p<0.05

