library(tidyverse)
library(rlist)

d <- read.csv("Table6.9.csv") %>%
  mutate(
    A= as.factor(A),
    B= as.factor(B)
  )

y <- d$Value %>% as.matrix

X <- matrix( c(
  1,0,0,0,0,0,
  1,0,0,0,0,0,
  1,0,0,1,0,0,
  1,0,0,1,0,0,
  1,1,0,0,0,0,
  1,1,0,0,0,0,
  1,1,0,1,1,0,
  1,1,0,1,1,0,
  1,0,1,0,0,0,
  1,0,1,0,0,0,
  1,0,1,1,0,1,
  1,0,1,1,0,1,
  NULL
  ),
  nrow= 12,byrow=T
)


processor <- function(select){
  X <- X[,select]
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  b <- solve(XtX)%*%t(X)%*%y #式6.3より
  btXty <- t(b)%*%t(X)%*%y
  D <- t(y)%*%y - btXty
  return(data.frame(btXty=btXty,DwithSigma=D,flexibility=12-length(select)))
}



fullmodel <- 1:6
abmodel <- 1:4
amodel <- 1:3
bmodel <- c(1,4)
nullmodel <- c(1)

model_list <- list(fullmodel,abmodel,amodel,bmodel,nullmodel)

df <- lapply(model_list,processor) %>% list.stack()
rownames(df) <- c("Full","ab","a","b","Null")


Anum <- d$A %>% unique %>% length
Bnum <- d$B %>% unique %>% length

df[1,5]