library(dplyr)
library(gganimate)
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
  x = cos(2*pi*u)*sqrt(-2*log(v))
  y = sin(2*pi*v)*sqrt(-2*log(u))
  df = data.frame(u=u, v=v,x=x, y=y)
  return(df) 
}

df1 <- moxbuller1(50000)
# df2 <- moxbuller2(5000)
# df3 <- moxbuller3(1000)
# 
# df <- list(df1, df2, df1)
# 
# tf <- tween_states(df, tweenlength = 1, statelength = 0,
#                    ease = "linear",
#                    nframes = 100)

p <- ggplot(df1) +
  geom_point(aes(x, y), colour = "#ffffcc", size = 0.2, alpha = 0.05) +
  geom_curve(aes(x = u, xend = x, y = v, yend = y), colour = "#ffffcc", size = 0.1, alpha = 0.015,
             curvature = 0.5, angle = 45) +
  #geom_line(aes(x, y), colour = "black", size = 0.35, alpha = 0.25) +
  coord_equal() + xlab(NULL) + ylab(NULL) +
  labs(x=NULL, y=NULL, title=NULL) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(panel.background=element_rect(fill = "#081d58",colour = NA)) +
  theme(plot.background=element_rect(fill = "#081d58",colour = NA)) +
  theme(panel.grid=element_blank()) +
  theme(panel.border=element_blank()) +
  theme(plot.margin=unit(c(0,0,0,0), "null")) +
  theme(panel.margin=unit(c(0,0,0,0), "null")) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.title=element_blank()) +
  theme(axis.line=element_blank()) +
  theme(legend.position="none") +
  theme(axis.ticks.length=unit(0, "null")) +
  theme(legend.margin=unit(0, "null")) + coord_polar()
p
# animation::ani.options(interval = 0.15)
# 
# gg_animate(p, "fourier2.gif", title_frame = FALSE, ani.width = 600, 
#            ani.height = 600)

ggsave("plots/plot001.png", width = 18, height = 18, units = c("cm"))
