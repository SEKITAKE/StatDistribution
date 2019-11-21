library(tidyverse)
library(rlist)
library(MuMIn)
#library(car)
options(na.action="na.fail")
rm(list=ls()) #一度リセットする


# ------------------------------------
# 一元配置分散分析
# ------------------------------------
d1 <- read.csv("Table6.6.csv")

pr1.1 <- d1 %>%
  gather(group,val)

pr1.2 <- aov(val~group,data=pr1.1) #分散分析表
pr1.3 <- anova(pr1.2) #分散分析

pr1.4 <- lm(val~group,data=pr1.1) %>% anova #結果はpr1.3と一致

# ------------------------------------
# 二元配置分散分析
# ------------------------------------

d2 <- read.csv("Table6.9.csv")

pr2.1 <- aov(Value~A+B+A*B,data= d2) #分散分析表
pr2.2 <- anova(pr2.1) #分散分析
pr2.3 <- lm(Value~A+B+A*B,data= d2)

# どの項目が重要かという判断で、検定ベースの判断はあまり良くなく、AICといいったモデル選択手段を使ったほうがいいんじゃない？
AICBase <- dredge(pr2.1,rank="AIC")
#Fullモデルが選択されたが、他のモデルもそこそこにAICが近かった、上位３位を見ると変数Aが含まれているので変数Aは重要そうだけど、変数Bについてはまだわからない、データを増やせってことですかね

# -------------------------------------
# 今日分散分析
# -------------------------------------

d3 <- read.csv("Table6.12.csv")


pr3.1 <- aov(y~x+gp,data= d3) #分散分析表
pr3.2 <- anova(pr3.1) #分散分析
pr3.3 <- lm(y~x+gp,data= d3) %>% anova

# ちなみにTypeI平方和で計算すると、説明変数の順序によって出力が変わる
pr3.4 <- aov(y~gp+x,data= d3) 
pr3.5 <- anova(pr3.4)
