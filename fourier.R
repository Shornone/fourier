library(dplyr)
library(gganimate)
library(ggart)
library(ggplot2)
library(tweenr)

set.seed(7)

moxbuller1 = function(n) {   
  u = runif(n)   
  v = runif(n) 
  x = cos(2*pi*u)*sqrt(-2*log(v))
  y = sin(2*pi*v)*sqrt(-2*log(u))
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
  x = cos(4 * pi * u) * (-2*log(v))
  y = sin(4 * pi * v) * (4*(cos(u))^1)
  df = data.frame(u = u, v = v,x = x, y = y)
  return(df) 
}

df1 <- moxbuller3(5000)
# df2 <- moxbuller2(5000)
# df3 <- moxbuller3(1000)
# 
# df <- list(df1, df2, df1)
# 
# tf <- tween_states(df, tweenlength = 1, statelength = 0,
#                    ease = "linear",
#                    nframes = 100)

p <- ggplot(df1) +
  geom_point(aes(x, y), colour = "black", size = 0.2, alpha = 0.05) +
  geom_curve(aes(x = u, xend = x, y = v, yend = y), colour = "black", size = 0.1, alpha = 0.015,
             curvature = 0.5) +
  #geom_line(aes(x, y), colour = "black", size = 0.35, alpha = 0.25) +
  coord_equal() +
  theme_blankcanvas()
#p
# animation::ani.options(interval = 0.15)
# 
# gg_animate(p, "fourier2.gif", title_frame = FALSE, ani.width = 600, 
#            ani.height = 600)

ggsave("plots/plot002.png", width = 18, height = 18, units = c("cm"))
