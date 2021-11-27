# POL ####
library(readxl)
pol <- read_excel("F00007614-WV6_Data_Poland_Excel_v20201117.xlsx")


table(pol$`V228A: How often in country's elections: Votes are counted fairly`)# +1 confia, 4 nao confia
table(pol$`V228F: How often in country's elections: Election officials are fair`)# +1 confia, 4 nao confia
table(pol$`V116: Confidence: The Political Parties`)# +1 confia, 4 nao confia
table(pol$`V96: Income equality`)#1 esquerda, 10 direita
table(pol$`V98: Government responsibility`)#1 esquerda, 10 direita
table(pol$`V2: Country/region`)
table(pol$`V127: Political system: Having a strong leader`)
table(pol$`V204: Justifiable: Abortion`)

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
                               `V204: Justifiable: Abortion`)) %>% na.omit()

BASE$Q1 <- BASE$`V228A: How often in country's elections: Votes are counted fairly`
BASE$Q2 <- BASE$`V228F: How often in country's elections: Election officials are fair`
BASE$Q3 <- BASE$`V116: Confidence: The Political Parties`
BASE$Q4 <- BASE$`V96: Income equality`
BASE$Q5 <- BASE$`V98: Government responsibility`
BASE$Country <- BASE$`V2: Country/region`
BASE$strong <- BASE$`V127: Political system: Having a strong leader`
BASE$abortion <- BASE$`V204: Justifiable: Abortion`

#novaseleção
base <- subset(BASE, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion)) 



#primeiro remover categorias missing

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

temp1 <- subset(base, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion))  %>% na.omit()
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
pol_12 <- subset(temp1, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion))  %>% na.omit()

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


#BRA ####
BRA <- read_excel("F00007581-WV6_Data_Brazil_Excel_v20201117(1).xlsx") 


table(BRA$`V228A: How often in country's elections: Votes are counted fairly`) # +1 confia, 4 nao confia
table(BRA$`V228F: How often in country's elections: Election officials are fair`) # +1 confia, 4 nao confia
table(BRA$`V116: Confidence: The Political Parties`) # +1 confia, 4 nao confia
table(BRA$`V96: Income equality`) #1 esquerda, 10 direita
table(BRA$`V98: Government responsibility`) #1 esquerda, 10 direita
table(BRA$`V2: Country/region`)
table(BRA$`V127: Political system: Having a strong leader`)
table(BRA$`V204: Justifiable: Abortion`)


library (dplyr)
library (ggplot2)
library (shiny)
library(tidyverse)
options(scipen = 1000)

# https://smolski.github.io/livroavancado/analisf.html # fonte


BASE2 <- subset(BRA, select = c(`V228A: How often in country's elections: Votes are counted fairly`,
                                `V228F: How often in country's elections: Election officials are fair`,
                                `V116: Confidence: The Political Parties`, 
                                `V96: Income equality`,
                                `V98: Government responsibility`, `V2: Country/region`,
                                `V127: Political system: Having a strong leader`,
                                `V204: Justifiable: Abortion`)) %>% na.omit()

BASE2$Q1 <- BASE2$`V228A: How often in country's elections: Votes are counted fairly`
BASE2$Q2 <- BASE2$`V228F: How often in country's elections: Election officials are fair`
BASE2$Q3 <- BASE2$`V116: Confidence: The Political Parties`
BASE2$Q4 <- BASE2$`V96: Income equality`
BASE2$Q5 <- BASE2$`V98: Government responsibility`
BASE2$Country <- BASE2$`V2: Country/region`
BASE2$strong <- BASE2$`V127: Political system: Having a strong leader`
BASE2$abortion <- BASE2$`V204: Justifiable: Abortion`


#novaseleção
base2 <- subset(BASE2, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion)) 



#primeiro remover categorias missing
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

temp2 <- subset(base2, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion))  %>% na.omit()
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
bra_14 <- subset(temp2, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion))  %>% na.omit()


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



#merge ####
rm(base, BASE, BASE2, base2, fit,fit2,matcor, 
   matcor2, PCAfit, PCAfit2, PCAfitvarimax, 
   PCAfitvarimax2, BRA, pol, temp1, temp2) # nao precisa mais

merge <- full_join(bra_14, pol_12)

URU <- read_excel("F00007632-WV6_Data_Uruguay_Excel_v20201117.xlsx")

BASE <- subset(URU, select = c(`V228A: How often in country's elections: Votes are counted fairly`,
                               `V228F: How often in country's elections: Election officials are fair`,
                               `V116: Confidence: The Political Parties`, 
                               `V96: Income equality`,
                               `V98: Government responsibility`, `V2: Country/region`,
                               `V127: Political system: Having a strong leader`,
                               `V204: Justifiable: Abortion`)) %>% na.omit()

BASE$Q1 <- BASE$`V228A: How often in country's elections: Votes are counted fairly`
BASE$Q2 <- BASE$`V228F: How often in country's elections: Election officials are fair`
BASE$Q3 <- BASE$`V116: Confidence: The Political Parties`
BASE$Q4 <- BASE$`V96: Income equality`
BASE$Q5 <- BASE$`V98: Government responsibility`
BASE$Country <- BASE$`V2: Country/region`
BASE$strong <- BASE$`V127: Political system: Having a strong leader`
BASE$abortion <- BASE$`V204: Justifiable: Abortion`

#novaseleção
base <- subset(BASE, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion)) 



#primeiro remover categorias missing

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

temp1 <- subset(base, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion))  %>% na.omit()
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
uru_s <- subset(temp1, select = c(Q1, Q2, Q3, Q4, Q5, Country, strong, abortion))  %>% na.omit()

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


merge2 <- preProcess(uru_s[,c(7:10)], method = c("range"))
norm1 <- predict(merge2,uru_s[,c(7:10)])
summary(norm1)

uru_s$direita <- norm1$econ*100
uru_s$desconfia <- norm1$avalia_demo*100
uru_s$strong <- norm1$strong*100
uru_s$abortion <- norm1$abortion*100
summary(uru_s)


urug <- subset(uru_s, select = c(Country, desconfia, direita, strong,abortion, Q1, Q2, Q3)) 

summary(urug) 

merge1 <- full_join(merge, urug)

summary(merge1)
summary(merge)


novos <- subset(merge1,, select = c(Country, Q1, Q2, Q3, abortion))
summary(novos)  
#normalizar
pretr <- preProcess(novos[,c(2:4)], method = c("range"))
norm1 <- predict(pretr,novos[,c(2:4)])
summary(norm1)

novos$Q1 <-(1-norm1$Q1)*100
novos$Q2 <- (1-norm1$Q2)*100
novos$Q3 <- (1-norm1$Q3)*100

summary(novos)


p <- ggplot(data= novos, aes(x= Q1, y=Country)) +
  geom_boxplot(fill = 'grey') + coord_flip() + xlab("Os votos são contados de maneira justa?") + ylab("País") + xlim(0,100)

p + stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) + stat_summary(fun.y=median, geom="crossbar", shape=0.1, size=0.6) + geom_jitter(color="black", size=1, alpha=0.2)


p <- ggplot(data= novos, aes(x= Q2, y=Country)) +
  geom_boxplot(fill = 'grey') + coord_flip() + xlab("Os funcionários eleitorais são justos?") + ylab("País") + xlim(0,100)

p + stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) + stat_summary(fun.y=median, geom="crossbar", shape=0.1, size=0.6) + geom_jitter(color="black", size=1, alpha=0.2)


p <- ggplot(data= novos, aes(x= Q3, y=Country)) +
  geom_boxplot(fill = 'grey') + coord_flip() + xlab("Confiança Partidos Políticos") + ylab("País") + xlim(0,100)

p + stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) + stat_summary(fun.y=median, geom="crossbar", shape=0.1, size=0.6) + geom_jitter(color="black", size=1, alpha=0.2)



p <- ggplot(data= novos, aes(x= abortion, y=Country)) +
  geom_boxplot(fill = 'grey') + coord_flip() + xlab("Favorável ao aborto") + ylab("País") + xlim(-1,100)

p + stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) + stat_summary(fun.y=median, geom="crossbar", shape=0.1, size=0.6) + geom_jitter(color="black", size=0.1, alpha=0.2)


summary(bra_14$abortion)
table(bra_14$abortion)
rm(base, pretr, BASE, bra_14, fit, matcor, merge, merge1, merge2, norm1, p, P, PCAfitvarimax, PCAfit, pol_12, temp1,uru_s, urug)

# homossexualidade
pol <- read_excel("F00007614-WV6_Data_Poland_Excel_v20201117.xlsx")
BRA <- read_excel("F00007581-WV6_Data_Brazil_Excel_v20201117(1).xlsx") 
URU <- read_excel("F00007632-WV6_Data_Uruguay_Excel_v20201117.xlsx")

pol <- subset(pol, select = c(`V2: Country/region`,
                               `V203: Justifiable: Homosexuality`)) %>% na.omit()

BRA <- subset(BRA, select = c(`V2: Country/region`,
                              `V203: Justifiable: Homosexuality`)) %>% na.omit()

URU <- subset(URU, select = c(`V2: Country/region`,
                              `V203: Justifiable: Homosexuality`)) %>% na.omit()

URU$Country <- URU$`V2: Country/region`
pol$Country <- pol$`V2: Country/region`
BRA$Country <- BRA$`V2: Country/region`
URU$Country<- dplyr::recode(URU$Country, '858' = "URU")
BRA$Country<- dplyr::recode(BRA$Country, '76' = "BRA")
pol$Country<- dplyr::recode(pol$Country, '616' = "POL")

merge <- full_join(pol,BRA)
merge <- full_join(merge,URU)
merge$homo <- merge$`V203: Justifiable: Homosexuality`
summary(merge$homo)
table(merge$homo)
merge$homo[merge$homo == -2] <- NA # gerar missing
merge$homo[merge$homo == -1] <- NA # gerar missing
summary(merge$homo) # para verificar

table(merge$homo) 
summary(merge$Country)
table(merge$Country)
merge <- subset(merge, select = c(homo, Country)) %>% na.omit()

merge2 <- preProcess(merge[,c(1)], method = c("range"))
norm1 <- predict(merge2,merge[,c(1)])
summary(norm1)


merge$homo <-norm1$homo*100


p <- ggplot(data= merge, aes(x= homo, y=Country)) +
  geom_boxplot(fill = 'grey') + coord_flip() + xlab("Homossexualismo é justificável?") + ylab("País") + xlim(0,100)

p + stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) + stat_summary(fun.y=median, geom="crossbar", shape=0.1, size=0.6) + geom_jitter(color="black", size=1, alpha=0.2)

# teste T para Q3

library(rstatix)
library(ggpubr)
set.seed(1234)
novos %>% sample_n_by(Country, size = 1)
novos %>%
  group_by(Country) %>%
  get_summary_stats(Q3, type = "mean_sd")
res.aov <- novos %>% anova_test(Q3 ~ Country)
res.aov
# Pairwise comparisons
pwc <- novos %>%
  pairwise_t_test(Q3 ~ Country, p.adjust.method = "bonferroni")
pwc


pwc <- pwc %>% add_xy_position(x = "Country")
ggboxplot(novos, x = "Country", y = "Q3") +
  stat_pvalue_manual(pwc, label = "p.adj", tip.length = 0, step.increase = 0.1) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

ggboxplot(novos, x = "Country", y = "Q3") +
  stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
