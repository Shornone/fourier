library(dplyr)
library(gganimate)
library(ggart)
library(ggforce)
library(ggplot2)
library(tweenr)

set.seed(101)

moxbuller1 = function(n) {   
  u = runif(n)   
  v = runif(n) 
  x = cos(2*pi*u)*sqrt(-1*exp(v))
  y = sin(1/2*pi*v)*sqrt(-1*sin(u))
  df = data.frame(u=u, v=v,x=x, y=y)
  return(df) 
}

moxbuller2 = function(n) {   
  u = rnorm(n)   
  v = rnorm(n) 
  x = cos(2*pi*u)*sqrt(-2*log(v))
  y = sin(2*pi*v)*sqrt(-2*log(u))
  df = data.frame(u=u, v=v,x=x, y=y)
  return(df) 
}

moxbuller3 = function(n) {   
  u = runif(n)   
  v = runif(n)
  x = cos(-2 * pi * 1 / u) * (1*log(v^0.5))
  y = sin(-2 * pi * v) * (1*(cos(u^0.5))^1)
  df = data.frame(u = u, v = v,x = x, y = y)
  return(df) 
}

df1 <- moxbuller3(50000)
# df2 <- moxbuller3(10)
# df3 <- moxbuller3(10)
# 
# df <- list(df1, df2, df3)
# 
# tf <- tween_states(df, tweenlength = 1, statelength = 0,
#                    ease = "linear",
#                    nframes = 10)

p <- ggplot(df1) +
  #geom_polygon(aes(x, y), colour = "white", size = 0.2, alpha = 0.1)
  geom_point(aes(x, y), colour = "white", size = 0.2, alpha = 0.075) +
  geom_curve(aes(x = u, xend = x, y = v, yend = y), colour = "white", size = 0.1, alpha = 0.02,
             curvature = 10) +
  #geom_line(aes(x, y), colour = "black", size = 0.35, alpha = 0.25) +
  coord_equal() + coord_polar() +
  theme_blankcanvas() + theme(plot.background = element_rect(fill = "black"))
p
# animation::ani.options(interval = 0.15)
# 
# gg_animate(p, "fourier2.gif", title_frame = FALSE, ani.width = 600, 
#            ani.height = 600)

ggsave("plots/plot003.png", width = 18, height = 18, units = c("cm"))

