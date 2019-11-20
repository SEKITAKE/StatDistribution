library(tidyverse)

rm(list=ls())
d <- read.csv("Table6.15.csv")
d$Y <- 1:nrow(d)


gp <- ggplot(d,aes(x=x,y=x,group=gp,color=gp)) + list(
  geom_point(size=3),
  theme_bw(),
  NULL
)