library(tidyverse)

rm(list=ls())
d <- read.csv("Table6.6.csv")
N <- nrow(d)*ncol(d)
J <- ncol(d) #水準数
K <- nrow(d) #繰り返し数
N <- J*K #全数
dd <- d^2 #それぞれの標本の二乗値

Yj <- apply(d,2,sum) #各水準の（期待値の）合計
Y.. <- sum(Yj) #全水準の（期待値の）合計


D1 <- sum(dd)-sum(Yj^2)/K #sigmaは打ち消される運命なのでここでは略
D0 <- sum(dd) - Y..^2/N
D0D1 <- sum(Yj^2)/K - Y..^2/N 

dframe <- data.frame( #分散分析表
  Flexibilty= c(1       ,J-1           ,N-J				,NA),
  sumsqrt=    c(Y..^2/N ,D1-D0         ,sum(dd)				,NA),
  musqrt=     c(NA      ,(D1-D0)/(J-1) ,D1/((K-1)*3)			,NA),
  F=          c(NA      ,NA            ,((D0-D1)/(J-1))/((D1)/(N-J))	,NA)
)
rownames(dframe) <- c("mu"    ,"process"     ,"deviation"  ,"sum")
Fval <- ((D0-D1)/(J-1))/((D1)/(N-J))
pvalue <- 1 - pf(Fval,J-1,N-J)


##############################
base_matrix <- matrix(c(
  1,0,0,
  0,1,0,
  0,0,1
  ),
  ncol=3
)

X <- array(
  base_matrix  %>% as.vector() %>% rep(10),
  dim=c(3,3,10)
) #XはJ×Jの行列で、それぞれの要素は１０個の１のみもしくは１０個の０のみからなるベクトル

b <- apply(d,2,mean) %>%
  as.matrix

y <- gather(d,key,val) %>%
  select(val) %>%
  as.matrix

Xb <- rep(base_matrix%*%b,each=10) #３次元(２次元の行行列&各要素には１次元のベクトル)×１次元の内積ができなかったので、別に作成
tbX <- rep(t(b)%*%t(base_matrix),each=10) #３次元(２次元の行行列&各要素には１次元のベクトル)×１次元の内積ができなかったので、別に作成
dev <- y-Xb #実測値の平均偏差
sigma2 <- t(dev) %*% dev /(N-J) #平均偏差を二乗して、、、、あれ？？分散？
sigma2_tmp <- (t(y)%*%y - tbX%*%y)/(N-J)
print( sigma2 - sigma2_tmp)

#############################


