### MANIPULAÇÃO ####

# POL #
library(readxl)
pol <- read_excel("F00007614-WV6_Data_Poland_Excel_v20201117.xlsx")


table(pol$`V95: Self positioning in political scale`)

library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
options(scipen = 1000)

# https://smolski.github.io/livroavancado/analisf.html # fonte


BASE <- subset(pol, select = c(`V228A: How often in country's elections: Votes are counted fairly`,
                               `V228F: How often in country's elections: Election officials are fair`,
                               `V116: Confidence: The Political Parties`, 
                               `V96: Income equality`,
                               `V98: Government responsibility`, `V2: Country/region`,
                               `V127: Political system: Having a strong leader`,
                               `V204: Justifiable: Abortion`,
                               `V95: Self positioning in political scale`)) %>% na.omit()

BASE$Q1 <- BASE$`V228A: How often in country's elections: Votes are counted fairly`
BASE$Q2 <- BASE$`V228F: How often in country's elections: Election officials are fair`
BASE$Q3 <- BASE$`V116: Confidence: The Political Parties`
BASE$Q4 <- BASE$`V96: Income equality`
BASE$Q5 <- BASE$`V98: Government responsibility`
BASE$Country <- BASE$`V2: Country/region`
BASE$strong <- BASE$`V127: Political system: Having a strong leader`
BASE$abortion <- BASE$`V204: Justifiable: Abortion`
BASE$autoideologia <- BASE$`V95: Self positioning in political scale`

#novaseleção
base <- subset(BASE, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion, autoideologia)) 



#primeiro remover categorias missing

summary(base$autoideologia)
table(base$autoideologia)
base$autoideologia[base$autoideologia == -2] <- NA
base$autoideologia[base$autoideologia == -1] <- NA
summary(base$autoideologia)
table(base$autoideologia)

summary(base$abortion)
table(base$abortion)
base$abortion[base$abortion == -2] <- NA # gerar missing
base$abortion[base$abortion == -1] <- NA # gerar missing
summary(base$abortion) # para verificar
table(base$abortion) 


summary(base$strong)
table(base$strong)
base$strong[base$strong == -2] <- NA # gerar missing
base$strong[base$strong == -1] <- NA # gerar missing
summary(base$strong) # para verificar
table(base$strong) 

summary(base$Q4)
table(base$Q4)
base$Q4[base$Q4 == -2] <- NA # gerar missing
base$Q4[base$Q4 == -1] <- NA # gerar missing
summary(base$Q4) # para verificar
table(base$Q4) # para verificar # obs o maior é MAIS "direita'

summary(base$Q5)
table(base$Q5)
base$Q5[base$Q5 == -2] <- NA # gerar missing
base$Q5[base$Q5 == -1] <- NA # gerar missing
summary(base$Q5) # para verificar
table(base$Q5) # para verificar # obs o maior é MAIS "DIREITA

summary(base$Q1)
base$Q1[base$Q1 == -2] <- NA # gerar missing
base$Q1[base$Q1 == -1] <- NA # gerar missing
summary(base$Q1) # para verificar
table(base$Q1) # para verificar # obs o maior é mais autoritário

summary(base$Q2)
base$Q2[base$Q2 == -2] <- NA # gerar missing
base$Q2[base$Q2 == -1] <- NA # gerar missing
summary(base$Q2) # para verificar
table(base$Q2) # para verificar # obs o maior é MENOS autoritário

summary(base$Q3)
base$Q3[base$Q3 == -2] <- NA # gerar missing
base$Q3[base$Q3 == -1] <- NA # gerar missing
summary(base$Q3) # para verificar
table(base$Q3) # para verificar # obs o maior é o MENOS autoritário

temp1 <- subset(base, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion, autoideologia))  %>% na.omit()
base <- subset(base, select = c(Q1, Q2, Q3, Q4, Q5))  %>% na.omit()


#analise descritiva
summary(base)


#iniciando a análise fatorial
matcor <- cor(base)
print(matcor, digits = 2)


require(corrplot)

corrplot(matcor, method="circle")

require(psych)
cortest.bartlett(base)


KMO(base)

# analise componentes principais
#ACP-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit<-princomp(base,cor=TRUE)
fit



summary(fit)

screeplot(fit)



plot(fit,type="lines")


PCAfit<-principal(base, nfactors=2,
                  n.obs=1278,rotate="none", scores=TRUE)
PCAfit

PCAfitvarimax<-principal(base, nfactors=2,
                         n.obs=1278,rotate="varimax",scores=TRUE)
PCAfitvarimax


# autovalores
#pca
PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)

#sepas
pol_12 <- subset(temp1, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion,autoideologia))  %>% na.omit()

pol_12$Country <- as.character(pol_12$Country)
table(pol_12$Country)

pol_12$Country<- dplyr::recode(pol_12$Country, '616' = "POL")
table(pol_12$Country)

summary(pol_12)

pol_12$avalia_demo <- pol_12$Q1 + pol_12$Q2 + pol_12$Q3
hist(pol_12$avalia_demo)
summary(pol_12$avalia_demo)
pol_12$econ <- pol_12$Q4 + pol_12$Q5
summary(pol_12$econ)
hist(pol_12$econ)


#BRA #
BRA <- read_excel("F00007581-WV6_Data_Brazil_Excel_v20201117(1).xlsx") 



BASE2 <- subset(BRA, select = c(`V228A: How often in country's elections: Votes are counted fairly`,
                                `V228F: How often in country's elections: Election officials are fair`,
                                `V116: Confidence: The Political Parties`, 
                                `V96: Income equality`,
                                `V98: Government responsibility`, `V2: Country/region`,
                                `V127: Political system: Having a strong leader`,
                                `V204: Justifiable: Abortion`,
                                `V95: Self positioning in political scale`)) %>% na.omit()

BASE2$Q1 <- BASE2$`V228A: How often in country's elections: Votes are counted fairly`
BASE2$Q2 <- BASE2$`V228F: How often in country's elections: Election officials are fair`
BASE2$Q3 <- BASE2$`V116: Confidence: The Political Parties`
BASE2$Q4 <- BASE2$`V96: Income equality`
BASE2$Q5 <- BASE2$`V98: Government responsibility`
BASE2$Country <- BASE2$`V2: Country/region`
BASE2$strong <- BASE2$`V127: Political system: Having a strong leader`
BASE2$abortion <- BASE2$`V204: Justifiable: Abortion`
BASE2$autoideologia <- BASE2$`V95: Self positioning in political scale`


#novaseleção
base2 <- subset(BASE2, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion, autoideologia)) 



#primeiro remover categorias missing

summary(base2$autoideologia)
table(base2$autoideologia)
base2$autoideologia[base2$autoideologia == -2] <- NA
base2$autoideologia[base2$autoideologia == -1] <- NA
summary(base2$autoideologia)
table(base2$autoideologia)

summary(base2$abortion)
table(base2$abortion)
base2$abortion[base2$abortion == -2] <- NA # gerar missing
base2$abortion[base2$abortion == -1] <- NA # gerar missing
summary(base2$abortion) # para verificar
table(base2$abortion) # para verificar # obs o maior é MAIS "direita'

summary(base2$strong)
table(base2$strong)
base2$strong[base2$strong == -2] <- NA # gerar missing
base2$strong[base2$strong == -1] <- NA # gerar missing
summary(base2$strong) # para verificar
table(base2$strong) # para verificar # obs o maior é MAIS "direita'

summary(base2$Q4)
table(base2$Q4)
base2$Q4[base2$Q4 == -2] <- NA # gerar missing
base2$Q4[base2$Q4 == -1] <- NA # gerar missing
summary(base2$Q4) # para verificar
table(base2$Q4) # para verificar # obs o maior é MAIS "direita'

summary(base2$Q5)
table(base2$Q5)
base2$Q5[base2$Q5 == -2] <- NA # gerar missing
base2$Q5[base2$Q5 == -1] <- NA # gerar missing
summary(base2$Q5) # para verificar
table(base2$Q5) # para verificar # obs o maior é MAIS "DIREITA

summary(base2$Q1)
base2$Q1[base2$Q1 == -2] <- NA # gerar missing
base2$Q1[base2$Q1 == -1] <- NA # gerar missing
summary(base2$Q1) # para verificar
table(base2$Q1) # para verificar # obs o maior é mais autoritário

summary(base2$Q2)
base2$Q2[base2$Q2 == -2] <- NA # gerar missing
base2$Q2[base2$Q2 == -1] <- NA # gerar missing
summary(base2$Q2) # para verificar
table(base2$Q2) # para verificar # obs o maior é MENOS autoritário

summary(base2$Q3)
base2$Q3[base2$Q3 == -2] <- NA # gerar missing
base2$Q3[base2$Q3 == -1] <- NA # gerar missing
summary(base2$Q3) # para verificar
table(base2$Q3) # para verificar # obs o maior é o MENOS autoritário

temp2 <- subset(base2, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion, autoideologia))  %>% na.omit()
base2<- subset(base2, select = c(Q1, Q2, Q3, Q4, Q5))  %>% na.omit()

#analise descritiva
summary(base2)


#iniciando a análise fatorial
matcor2 <- cor(base2)
print(matcor, digits = 2)



corrplot(matcor2, method="circle")


cortest.bartlett(base2)


KMO(base2)

# analise componentes principais
#ACP-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit2<-princomp(base2,cor=TRUE)
fit2

summary(fit2)


screeplot(fit2)



plot(fit2,type="lines")


PCAfit2<-principal(base2, nfactors=2,
                   n.obs=1278,rotate="none", scores=TRUE)
PCAfit2

PCAfitvarimax2<-principal(base2, nfactors=2,
                          n.obs=1278,rotate="varimax",scores=TRUE)
PCAfitvarimax2







# autovalores
#pca
PCAfitvarimax2$values

PCAfitvarimax2$loadings

biplot(PCAfitvarimax2)

#sepas
bra_14 <- subset(temp2, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion, autoideologia))  %>% na.omit()


bra_14$Country <- as.character(bra_14$Country)
table(bra_14$Country)

bra_14$Country<- dplyr::recode(bra_14$Country, '76' = "BRA")
table(bra_14$Country)

summary(bra_14)
bra_14$avalia_demo <- bra_14$Q1 + bra_14$Q2 + bra_14$Q3
summary(bra_14$avalia_demo)
hist(bra_14$avalia_demo)
bra_14$econ <- bra_14$Q4 + bra_14$Q5
summary(bra_14$econ)
hist(bra_14$econ)



#merge #
rm(base, BASE, BASE2, base2, fit,fit2,matcor, 
   matcor2, PCAfit, PCAfit2, PCAfitvarimax, 
   PCAfitvarimax2, BRA, pol, temp1, temp2) # nao precisa mais

merge <- full_join(bra_14, pol_12)


table(merge$Country)
# normalizar as variáveis de merge

# Quanto maior avalia_democracia menos confia
# quanto  maior econ mais direita
summary(merge)
merge$strong <- as.character(merge$strong)
merge <- within(merge, {
  strong <- car::Recode(strong, '1 = "4"; 2 = "3"; 3 = 2; 4 = "1"', as.factor=TRUE)
})
table(merge$strong) #para conferir

merge$strong <- as.numeric(merge$strong)

library(caret)
merge2 <- preProcess(merge[,c(7:8)], method = c("range"))
norm1 <- predict(merge2,merge[,c(7:8)])
summary(norm1)

merge3 <- preProcess(merge[,c(10:11)], method = c("range"))
norm2 <- predict(merge3,merge[,c(10:11)])
summary(norm2)


merge$direita <- norm2$econ*100
merge$desconfia <- norm2$avalia_demo*100
merge$strong <- norm1$strong*100
merge$abortion <- norm1$abortion*100
summary(merge)


rm(norm1, bra_14, pol_12, merge2, merge3, norm2)


merge <- subset(merge, select = c(Country, desconfia, direita, strong, abortion, autoideologia))
summary(merge)
Brasil <- merge %>%
  filter(Country == "BRA")
Poland <- merge %>%
  filter(Country == "POL")


#URU

URU <- read_excel("F00007632-WV6_Data_Uruguay_Excel_v20201117.xlsx")

BASE <- subset(URU, select = c(`V228A: How often in country's elections: Votes are counted fairly`,
                               `V228F: How often in country's elections: Election officials are fair`,
                               `V116: Confidence: The Political Parties`, 
                               `V96: Income equality`,
                               `V98: Government responsibility`, `V2: Country/region`,
                               `V127: Political system: Having a strong leader`,
                               `V204: Justifiable: Abortion`,
                               `V95: Self positioning in political scale`)) %>% na.omit()

BASE$Q1 <- BASE$`V228A: How often in country's elections: Votes are counted fairly`
BASE$Q2 <- BASE$`V228F: How often in country's elections: Election officials are fair`
BASE$Q3 <- BASE$`V116: Confidence: The Political Parties`
BASE$Q4 <- BASE$`V96: Income equality`
BASE$Q5 <- BASE$`V98: Government responsibility`
BASE$Country <- BASE$`V2: Country/region`
BASE$strong <- BASE$`V127: Political system: Having a strong leader`
BASE$abortion <- BASE$`V204: Justifiable: Abortion`
BASE$autoideologia <- BASE$`V95: Self positioning in political scale`

#novaseleção
base <- subset(BASE, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion, autoideologia)) 



#primeiro remover categorias missing

summary(base$autoideologia)
table(base$autoideologia)
base$autoideologia[base$autoideologia == -2] <- NA
base$autoideologia[base$autoideologia == -1] <- NA
summary(base$autoideologia)
table(base$autoideologia)

summary(base$abortion)
table(base$abortion)
base$abortion[base$abortion == -2] <- NA # gerar missing
base$abortion[base$abortion == -1] <- NA # gerar missing
summary(base$abortion) # para verificar
table(base$abortion) 


summary(base$strong)
table(base$strong)
base$strong[base$strong == -2] <- NA # gerar missing
base$strong[base$strong == -1] <- NA # gerar missing
summary(base$strong) # para verificar
table(base$strong) 

summary(base$Q4)
table(base$Q4)
base$Q4[base$Q4 == -2] <- NA # gerar missing
base$Q4[base$Q4 == -1] <- NA # gerar missing
summary(base$Q4) # para verificar
table(base$Q4) # para verificar # obs o maior é MAIS "direita'

summary(base$Q5)
table(base$Q5)
base$Q5[base$Q5 == -2] <- NA # gerar missing
base$Q5[base$Q5 == -1] <- NA # gerar missing
summary(base$Q5) # para verificar
table(base$Q5) # para verificar # obs o maior é MAIS "DIREITA

summary(base$Q1)
base$Q1[base$Q1 == -2] <- NA # gerar missing
base$Q1[base$Q1 == -1] <- NA # gerar missing
summary(base$Q1) # para verificar
table(base$Q1) # para verificar # obs o maior é mais autoritário

summary(base$Q2)
base$Q2[base$Q2 == -2] <- NA # gerar missing
base$Q2[base$Q2 == -1] <- NA # gerar missing
summary(base$Q2) # para verificar
table(base$Q2) # para verificar # obs o maior é MENOS autoritário

summary(base$Q3)
base$Q3[base$Q3 == -2] <- NA # gerar missing
base$Q3[base$Q3 == -1] <- NA # gerar missing
summary(base$Q3) # para verificar
table(base$Q3) # para verificar # obs o maior é o MENOS autoritário

temp1 <- subset(base, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion, autoideologia))  %>% na.omit()
base <- subset(base, select = c(Q1, Q2, Q3, Q4, Q5))  %>% na.omit()


#analise descritiva
summary(base)


#iniciando a análise fatorial
matcor <- cor(base)
print(matcor, digits = 2)


require(corrplot)

corrplot(matcor, method="circle")

require(psych)
cortest.bartlett(base)


KMO(base)

# analise componentes principais
#ACP-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit<-princomp(base,cor=TRUE)
fit



summary(fit)

screeplot(fit)



plot(fit,type="lines")


PCAfit<-principal(base, nfactors=2,
                  n.obs=1278,rotate="none", scores=TRUE)
PCAfit

PCAfitvarimax<-principal(base, nfactors=2,
                         n.obs=1278,rotate="varimax",scores=TRUE)
PCAfitvarimax


# autovalores
#pca
PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)

#sepas
uru_s <- subset(temp1, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion, autoideologia))  %>% na.omit()

uru_s$Country <- as.character(uru_s$Country)
table(uru_s$Country)

uru_s$Country<- dplyr::recode(uru_s$Country, '858' = "URU")
table(uru_s$Country)

summary(uru_s)

uru_s$avalia_demo <- uru_s$Q1 + uru_s$Q2 + uru_s$Q3
hist(uru_s$avalia_demo)
summary(uru_s$avalia_demo)
uru_s$econ <- uru_s$Q4 +uru_s$Q5
summary(uru_s$econ)
hist(uru_s$econ)

# trocar merge por uru_s

summary(uru_s)
uru_s$strong <- as.character(uru_s$strong)
uru_s <- within(uru_s, {
  strong <- car::Recode(strong, '1 = "4"; 2 = "3"; 3 = 2; 4 = "1"', as.factor=TRUE)
})
table(uru_s$strong) #para conferir

uru_s$strong <- as.numeric(uru_s$strong)

rm(URU, temp1, PCAfit, PCAfitvarimax, fit, matcor, BASE, base)

merge2 <- preProcess(uru_s[,c(7:8)], method = c("range"))
norm1 <- predict(merge2,uru_s[,c(7:8)])
summary(norm1)

merge3 <- preProcess(uru_s[,c(10:11)], method = c("range"))
norm2 <- predict(merge3,uru_s[,c(10:11)])
summary(norm2)


uru_s$direita <- norm2$econ*100
uru_s$desconfia <- norm2$avalia_demo*100
uru_s$strong <- norm1$strong*100
uru_s$abortion <- norm1$abortion*100
summary(uru_s)




Uruguay <- subset(uru_s, select = c(Country, desconfia, direita, strong,abortion,autoideologia)) 

summary(Uruguay)

rm(merge2, merge3, norm1, norm2, uru_s)

merge <- full_join(Uruguay, merge)
table(merge$Country)

#### t test.##


# observação 'merge' é a base que tem os três países, Brasil somente os do Brasil, Poland = Polonia, Uruguay = Uruguay

str(merge)

#t.test para variáveis múltiplas
library(rstatix)
library(ggpubr)


set.seed(1234)
merge %>% sample_n_by(Country, size = 1)

merge %>%
  group_by(Country) %>%
  get_summary_stats(desconfia, type = "mean_sd")
merge %>%
  group_by(Country) %>%
  get_summary_stats(direita, type = "mean_sd")
merge %>%
  group_by(Country) %>%
  get_summary_stats(strong, type = "mean_sd")
merge %>%
  group_by(Country) %>%
  get_summary_stats(abortion, type = "mean_sd")
merge %>%
  group_by(Country) %>%
  get_summary_stats(autoideologia, type = "mean_sd")
res.aov <- merge %>% anova_test(autoideologia ~ Country)
res.aov

# Pairwise comparisons
pwc <- merge %>%
  pairwise_t_test(autoideologia ~ Country, p.adjust.method = "bonferroni")
pwc


# Show adjusted p-values
pwc <- pwc %>% add_xy_position(x = "Country")
ggboxplot(merge, x = "Country", y = "autoideologia") +
  stat_pvalue_manual(pwc, label = "p.adj", tip.length = 0, step.increase = 0.1) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

ggboxplot(merge, x = "Country", y = "autoideologia") +
  stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


pwc <- merge %>%
  pairwise_t_test(desconfia ~ Country, p.adjust.method = "bonferroni")
pwc


# Show adjusted p-values
pwc <- pwc %>% add_xy_position(x = "Country")
ggboxplot(merge, x = "Country", y = "desconfia") +
  stat_pvalue_manual(pwc, label = "p.adj", tip.length = 0, step.increase = 0.1) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

ggboxplot(merge, x = "Country", y = "desconfia") +
  stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


### regression ####

merge$economia <- merge$direita

library(nnet) # pra regressão multinominal
merge$Country <- as.factor(merge$Country)
merge$Country <- relevel(merge$Country, "URU")
multi1<- multinom(Country ~ desconfia + economia + strong + abortion + autoideologia, data = merge)

library(sjPlot)

modmerg1 <- lm(desconfia ~ autoideologia, data = merge)
summary(modmerg1)
modmergURU <- lm(desconfia ~ autoideologia, data = Uruguay)
summary(modmergURU)
modmergPOL <- lm(desconfia ~ autoideologia, data = Poland)
summary(modmergPOL)
modmergBRA <- lm(desconfia ~ autoideologia, data = Brasil)
summary(modmergBRA)

tab_model(modmergBRA, modmergPOL, modmergURU, Intercept = FALSE)

rm(modmerg1, modmergBRA, modmergPOL, modmergURU, multi1, pwc, res.aov)
Brasil$economia_direita <- Brasil$direita
Poland$economia_direita <- Poland$direita
Uruguay$economia_direita <- Uruguay$direita
Brasil <- subset(Brasil, select = c(desconfia, strong, economia_direita, abortion, autoideologia))
Poland <- subset(Poland, select = c(desconfia, strong, economia_direita, abortion, autoideologia))
Uruguay <- subset(Uruguay, select = c(desconfia, strong, economia_direita, abortion, autoideologia))
# fatorial comparação


#iniciando a análise fatorial # BRASIL
matcor <- cor(Brasil)
print(matcor, digits = 2)


corrplot(matcor, method="circle")

require(psych)
cortest.bartlett(Brasil)


KMO(Brasil)

# analise componentes principais
#ACP-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit<-princomp(Brasil,cor=TRUE)
fit



summary(fit)

screeplot(fit)
# escolha 3 fatores


plot(fit,type="lines")


PCAfit<-principal(Brasil, nfactors=3,
                  n.obs=986,rotate="none", scores=TRUE)
PCAfit

PCAfitvarimax<-principal(Brasil, nfactors=3,
                         n.obs=986,rotate="varimax",scores=TRUE)
PCAfitvarimax


# autovalores
#pca
PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)

#iniciando a análise fatorial # Poland
matcor <- cor(Poland)
print(matcor, digits = 2)


corrplot(matcor, method="circle")

require(psych)
cortest.bartlett(Poland)


KMO(Poland)

# analise componentes principais
#ACP-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit<-princomp(Poland,cor=TRUE)
fit



summary(fit)

screeplot(fit)



plot(fit,type="lines")
# escolha 2 fatpres

PCAfit<-principal(Poland, nfactors=2,
                  n.obs=584,rotate="none", scores=TRUE)
PCAfit

PCAfitvarimax<-principal(Poland, nfactors=2,
                         n.obs=584,rotate="varimax",scores=TRUE)
PCAfitvarimax


# autovalores
#pca
PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)

#iniciando a análise fatorial # Uruguay
matcor <- cor(Uruguay)
print(matcor, digits = 2)


corrplot(matcor, method="circle")

require(psych)
cortest.bartlett(Uruguay)


KMO(Uruguay)

# analise componentes principais
#ACP-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit<-princomp(Uruguay,cor=TRUE)
fit



summary(fit)

screeplot(fit)



plot(fit,type="lines")
# escolha 1 fator

PCAfit<-principal(Uruguay, nfactors=2,
                  n.obs=588,rotate="none", scores=TRUE)
PCAfit

PCAfitvarimax<-principal(Uruguay, nfactors=2,
                         n.obs=588,rotate="varimax",scores=TRUE)
PCAfitvarimax


# autovalores
#pca
PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)


