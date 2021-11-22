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
merge2 <- preProcess(merge[,c(7:10)], method = c("range"))
norm1 <- predict(merge2,merge[,c(7:10)])
summary(norm1)

merge$direita <- norm1$econ*100
merge$desconfia <- norm1$avalia_demo*100
merge$strong <- norm1$strong*100
merge$abortion <- norm1$abortion*100
summary(merge)

hist(merge$strong)

ggplot(data= merge, aes(x= desconfia, y=Country)) +
  geom_boxplot(fill = 'red') + coord_flip() + xlab("Desconfiança na democracia") + ylab("País")


ggplot(data= merge, aes(x= direita, y=Country)) +
  geom_boxplot(fill = 'red') + coord_flip() + xlab("Posição econômica à Direita") + ylab("País")

t.test(desconfia ~ Country, data = merge)
t.test(direita ~ Country, data = merge)

# líder forte




ggplot(data= merge, aes(x= strong, y=Country)) +
  geom_boxplot(fill = 'red') + coord_flip() + xlab("Apoio a líder forte") + ylab("País")

t.test(strong ~ Country, data = merge)


# aborto

ggplot(data= merge, aes(x= abortion, y=Country)) +
  geom_boxplot(fill = 'red') + coord_flip() + xlab("Aborto é justificável?") + ylab("País")

t.test(abortion~ Country, data = merge)

rm(norm1, bra_14, pol_12, merge2)


merge <- subset(merge, select = c(Country, desconfia, direita, strong, abortion)) 
Brasil <- merge %>%
  filter(Country == "BRA")
Poland <- merge %>%
  filter(Country == "POL")

summary(Brasil)
summary(Poland)

matrizBra <- cor(Brasil[2:5], method = "pearson") #estamos pondo as colunas que importam 2 a 4
matrizBra <- round(matrizBra,2)
corrplot(matrizBra)
matrizPoland <- cor(Poland[2:5], method = "pearson") #estamos pondo as colunas que importam 2 a 4
matrizPoland <- round(matrizPoland,2)
corrplot(matrizPoland)


corrplot(matrizBra, method="ellipse", 
         type="lower", order="original",
         tl.col="black", tl.srt=45,
         diag=FALSE)

corrplot(matrizPoland, method="ellipse", 
         type="lower", order="original",
         tl.col="black", tl.srt=45,
         diag=FALSE)



merge$Pais <- as.factor(merge$Country) 

summary(merge)

merge$Pais <- relevel(merge$Pais, "BRA")
mod <- glm(Pais ~ desconfia+direita+strong+abortion, data = merge, family = "binomial")
summary(mod)
exp(mod$coefficients)
0.9576081 - 1 #desconfiar
1.0109666  - 1 #direita
1.0346501 - 1 #strong
1.0126171 - 1 #abortion

library(sjPlot)

tab_model(mod)
##Ajuste do Modelo##
library(performance)
r2_mcfadden(mod)
r2_nagelkerke(mod)
r2_tjur(mod)

##Diagnóstico do Modelo##
#Colinearidade# variavel medidoras são independentes entre si
library(car)
vif(mod)
#Importância das Variáveis Preditoras#
library(caret)
varImp(mod)


t.test(desconfia ~ Country, data = merge)
t.test(direita ~ Country, data = merge)
t.test(strong ~ Country, data = merge)
t.test(abortion~ Country, data = merge)


library(coefplot)
coefplot(mod, intercept = F)


library(treemap)
group <- c("Desconfiança Democracia","Direita","Líder forte", "Anti-Aborto")
Poland_2012 <- c(41.39,49.83,29.51,25.83)
data22 <- data.frame(group,Poland_2012)

# treemap
treemap(data22,
        index="group",
        vSize="Poland_2012",
        type="index"
)

Brasil_2014 <- c(55.51,39.43,59.84,13.87)
data23 <- data.frame(group,Brasil_2014)

# treemap
treemap(data23,
        index="group",
        vSize="Brasil_2014",
        type="index"
)


mergtotal <- subset(merge, select = c(Country, desconfia, direita, strong, abortion)) 
mergdesconf <- subset(merge, select = c(Country, desconfia))
mergdir <- subset(merge, select = c(Country, direita))
mergestrong <- subset(merge, select = c(Country, strong))
mergeabortion <- subset(merge, select = c(Country, abortion))

#install.packages("writexl")
library(writexl)
write_xlsx(merge, "D:/ATUALIZA_PASTA_d/Brasil_POL/merge22.xlsx" )
write_xlsx(mergtotal,"D:/ATUALIZA_PASTA_d/Brasil_POL/analise extra/merge_final.xlsx")
write_xlsx(mergdesconf,"D:/ATUALIZA_PASTA_d/Brasil_POL/analise extra/merge_desconf.xlsx")
write_xlsx(mergdir,"D:/ATUALIZA_PASTA_d/Brasil_POL/analise extra/merge_direita.xlsx")
write_xlsx(mergestrong,"D:/ATUALIZA_PASTA_d/Brasil_POL/analise extra/merge_strong.xlsx")
write_xlsx(mergeabortion,"D:/ATUALIZA_PASTA_d/Brasil_POL/analise extra/merge_aborto.xlsx")



