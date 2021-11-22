library(tidyverse)

library (shiny)

options(scipen = 1000)

library(readxl)
merge <- read_excel("D:/ATUALIZA_PASTA_d/Brasil_POL/merge22.xlsx")


modDir <- lm(direita ~ Country + strong + abortion + desconfia, data = merge)
summary(modDir)
vif(modDir)

modstrong <- lm(strong ~ Country + direita + abortion + desconfia, data = merge)
summary(modstrong)
vif(modstrong)

modabort<- lm(abortion ~ Country + direita + strong + desconfia, data = merge)
summary(modabort)
vif(modabort)

modde<- lm(desconfia ~ Country + direita + strong + abortion, data = merge)
summary(modde)
vif(modde)
