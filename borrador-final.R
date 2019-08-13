sum(dbinom(0:2, 15, 0.05))
sum(dbinom(0:2, 15, 0.1))
sum(dbinom(0:2, 15, 0.2))

library(tidyverse)
prob_rechazo <- function(p, n, RR) { sum(dbinom(RR, n, p)) }

df <- tibble(
  p = seq(0, 1, 0.01),
  E_phi = map_dbl(p, prob_rechazo, n = 15, RR = 0:2))
ggplot(df, aes(p, E_phi)) +
  geom_line() +
  geom_vline(xintercept = 0.1, color = 'gray') +
  geom_hline(yintercept = 0.8159, color = 'gray')

df <- tibble(
  n = seq.int(100),
  alfa = map_dbl(n, ~prob_rechazo(0.1, ., 0:2)))
ggplot(df, aes(n, alfa)) +
  geom_line() +
  geom_vline(xintercept = 60.5, color = 'gray') +
  geom_hline(yintercept = 0.05, color = 'gray')

map_dbl(59:62, ~prob_rechazo(0.1, ., 0:2))

df <- tibble(
  n = seq.int(100),
  alfa = map_dbl(n, ~prob_rechazo(0.1, ., 0:2)))
ggplot(df, aes(n, E_phi)) +
  geom_line() +
  geom_vline(xintercept = 60.5, color = 'gray') +
  geom_hline(yintercept = 0.05, color = 'gray')


S_tita <- function(tita, X) { sum(X>tita) }
max_x <- 1000
n <- 15
X <- sort(runif(n, 0, max_x))
df <- tibble(
  x = c(0, X),
  xend = c(X, max_x),
  y = n:0,
  yend = y
)
p <- (ggplot(df, aes(x=x, y=y, xend=xend, yend=yend)) +
#        geom_vline(aes(xintercept=x), linetype=2, color="grey") +
        geom_point() +  # Solid points to left
        geom_point(aes(x=xend, y=y), shape=1) +  # Open points to right
        geom_segment())  # Horizontal line segments
p