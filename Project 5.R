X = seq(-2, 2, 0.01)
Y = 1 + X + -2 * (X - 1)^2 * (X >= 1)
df <- data.frame(X, Y)
install.packages("ggplot2")
library(tidyverse)
ggplot(df, aes(x = X, y = Y)) + 
  geom_vline(xintercept = 0) + 
  geom_vline(xintercept = 1, col = "mediumseagreen") + 
  geom_hline(yintercept = 0) + 
  geom_line(size = 1.5)

install.packages("tidyverse")
