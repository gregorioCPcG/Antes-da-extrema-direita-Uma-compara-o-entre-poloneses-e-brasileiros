#Poland and Private ownership

Wave <- c("Seven", "Seven", "Six", "Six", "Five", "Five")
#  Private vs state ownership of business - de 0 a 10, quanto maior mais privatista
# fonte: https://www.worldvaluessurvey.org/WVSOnline.jsp
Mean <- c(5.97, 5.53, 5.60,6.45,5.61,6.77 )
Where <- c("Poland", "World", "World", "Poland", "World", "Poland" )

apendiceA1 <- data.frame(Wave, Mean, Where)

library(ggplot2)
ggplot(data= apendiceA1, aes(x= Mean, y=Where)) +
  geom_boxplot(fill = 'red') + coord_flip() + xlab("Posição Privatista") + ylab("País")
