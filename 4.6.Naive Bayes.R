#Formacao Cientista de Dados - Fernando Amaral

library(e1071)

credito = read.csv(file.choose(),sep=',',header=T)
head(credito)
dim(credito)
amostra = sample(2,1000,replace=T, prob=c(0.7,0.3))
amostra
creditotreino = credito[amostra==1,]
creditoteste = credito[amostra==2,]

dim(creditotreino)[1]
dim(creditoteste)[2]

modelo <- naiveBayes(colnames(credito[5]) ~., creditotreino)

predicao <- predict(modelo,creditoteste)

predicao

confusao = table(creditoteste$class,predicao)
taxaacerto = (confusao[1] + confusao[4]) / sum(confusao)
taxaerro = (confusao[2] + confusao[3]) / sum(confusao)


