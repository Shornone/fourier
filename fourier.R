library(ggart)
library(tidyverse)

set.seed(101)

n <- 50000

df <- data.frame(x = runif(n) , y = runif(n)) %>%
  mutate(xend = sin(-3 * pi * x) * sin(y),
         yend = sin(-2 * pi * y) * cos(x))

p <- ggplot(df) +
  geom_point(aes(x, y), size = 0.15, alpha = 0.075) +
  geom_segment(aes(x, y, xend = xend, yend = yend), size = 0.05, alpha = 0.075) +
  coord_equal() +
  coord_polar() +
  theme_blankcanvas()

ggsave("plots/plot006.png", p, width = 18, height = 18, units = "cm")
