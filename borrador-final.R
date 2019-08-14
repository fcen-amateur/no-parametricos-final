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
max_x <- 100
n <- 15
X <- seq(5, max_x, length.out = n) + runif(n, -2, 2)
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

library(latex2exp)
ggplot(df, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_point() +  # Solid points to left
  geom_point(aes(x=xend, y=y), shape=1) +  # Open points to right
  geom_segment() +
  geom_segment(
    data = df2, size = 3, color = 'gray') +
  #xlab(TeX("$\\theta$")) +
  ylab(TeX("$S(\\theta)$")) +
  scale_x_continuous(
    name = TeX("$\\theta$"),
    breaks = df$x,
    labels = ticks,
    )
  
ticks <- map(sprintf('$X^{(%d)}$', 0:15), TeX, output = 'text') %>% parse(text = .)
parseexpression(`$X^{(14)}$`)
map(sprintf('$X^{(%d)}$', 0:15), TeX, output = 'text')
help(sprintf)
plot(TeX('$X^{(4)}$'))
plotmath()
library(stringr)
map(str_c("$X^{(", 1:15, ")}"), TeX)
tibble(
  x = 0:n,
  y = - dbinom(x, n, 0.5)) %>%
  ggplot(aes(x, y)) +
  coord_flip() +
  geom_col()

theta <- "\u03B8"
xord <- function(i) bquote(X^(.(i)))
max_x <- 100
n <- 9
d <- 10
MA <- d*seq.int(n) + max_x/(3*(n-1)) * runif(n, -.1, .1)
lineas <- tibble(
  x = c(0, MA),
  xend = c(MA, d*(n+1)),
  y = n:0,
  yend = y)
barras <- tibble(
  x=0,
  xend=- d*n*dbinom(0:n, n, 0.5),
  y=0:n,
  yend=y)
ggplot(lineas, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_vline(xintercept = 0, color = 'gray') +
  geom_hline(yintercept = 0, color = 'gray') +
  geom_point() +  # Solid points to left
  geom_point(aes(x=xend, y=y), shape=1) +  # Open points to right
  geom_segment() +
  geom_segment(
    data = barras, size = 3, color = 'gray') +
  scale_x_continuous(
    name = expression(theta),
    breaks = MA,
    labels = map(1:n, xord)) +
  scale_y_continuous(
    name = expression(S(theta)),
    breaks = c(0:3, (n-2):n),
    labels = c(0:3, (n-2):n)
  ) +
  theme(line = element_blank(),
        panel.grid.major.x = element_line(color = "white"))
