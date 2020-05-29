library(tidyverse)

# Vamos a asumir que tenemos un experimento con grupos independientes (between subject) y que la distribución
# de lo que estamos midiendo es normal. Por eso vamos a utiliza un t-test no pareado (test paramétrico).
set.seed(124)
alpha <- 0.05
d <- .3

# Los parámetros de ambas
sigma <- 1 # Asumamos igual varianza
mean1 <- 0
mean2 <- sigma * d

nmin <- 5
nmax <- 200
ns <- seq(nmin, nmax, 5)

nrep <- 1000
reps <- seq(1, nrep, 1)

p_exp <- array(0,dim=c(length(ns), nrep))
d_exp <- array(0,dim=c(length(ns), nrep))

p_exp <- expand.grid(rep = reps, n = ns)
p_exp$value <- 0
d_exp <- expand.grid(rep = reps, n = ns)
d_exp$value <- 0

for (n_i in ns) {
  cat(paste0("n = ", n_i, "\n"))
  for (rep_j in reps) {
    sample1 <- rnorm(n_i, mean = mean1, sd = sigma)
    sample2 <- rnorm(n_i, mean = mean2, sd = sigma)
      
    # Calculo el pvalue
    p_exp$value[(p_exp$n == n_i) & (p_exp$rep == rep_j)] <- t.test(sample1, sample2)$p.value
    
    # Calculo el effect size
    d_exp$value[(d_exp$n == n_i) & (d_exp$rep == rep_j)] <- 
                (mean(sample2) - mean(sample1))/(sqrt((sd(sample1)^2 + sd(sample2)^2)/2))
    
    
  }
}

decision_exp <- p_exp
decision_exp$value <- p_exp$value<0.05

d_exp.summ <- d_exp %>%
  group_by(n) %>%
  summarise(mean_d = mean(value),
            sd_d = sd(value))

decision_exp.summ <- decision_exp %>%
  group_by(n) %>%
  summarise(true = sum(value)/n())

plot_df <- merge(d_exp.summ, decision_exp.summ, by = "n") %>%
  gather(measure, value, mean_d:true)

cbPalette <- c( "#56B4E9", "#E69F00","#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
theme_minimal()
fig.dvsn <- ggplot(plot_df, aes(x = n, y = value, colour = measure)) +
  geom_hline(yintercept = d, linetype = "dashed", colour = "black") + 
  geom_point() + 
  geom_line() +
  scale_color_manual(values = cbPalette, labels = c("D promedio", "D sd", "Power")) +
  labs(colour = "Medida",
       x = "n",
       y = "",
       title = "Magnitudes vs. n") +
  theme_minimal()
fig.dvsn

