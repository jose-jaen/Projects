setwd('C:/Users/Usuario/Desktop/Universidad/Análisis Multivariante/Pepe/Datos')

datos <- read.csv('mobiles.csv', sep = ',', dec = '.')

View(datos)

cat('Hay', dim(datos)[2], 'variables en nuestro conjunto de datos \n')

bin <- 0 ; multi <- 0
for (i in 1:dim(datos)[2]) {
  if (length(unique(datos[, i])) == 2) {
    bin <- bin + 1
  } else if (length(unique(datos[, i])) > 2 && length(unique(datos[, i])) <= 4) {
    multi <- multi + 1
  }
}

cat('Concretamente, encontramos', bin, 'variables binarias,', multi, 'variable multiestado y las', dim(datos)[2] - multi - bin, 'restantes son cuantitativas \n')

posiciones <- c()
for (i in 1:dim(datos)[2]) {
  if (length(unique(datos[, i])) > 4) {
    posiciones <- c(posiciones, i)
  }
}

posiciones

cuantis <- datos[, posiciones]
View(cuantis)

medias <- c()
for (i in 1:dim(cuantis)[2]) {
  medias <- c(medias, mean(cuantis[, i]))
}
medias

mean(datos$talk_time)

View(cov(cuantis))

library(e1071)

normal <- c() ; log <- c()
sqrt <- c() ; library(e1071)

for (i in posiciones) {
  normal <- c(normal, skewness(datos[, i]))
  log <- c(log, skewness(log(datos[, i])))
  sqrt <- c(sqrt, skewness(sqrt(datos[, i])))
}

comparar <- data.frame(normal, log, sqrt)

for (i in 1:dim(comparar)[1]) {
  for (j in 1:dim(comparar)[2]) {
    if (is.na(comparar[i,j]) == TRUE) {
      comparar[i, j] <- 10000000
    }
  }
}

comparar

fila <- c() ; columna <- c()
for (i in 1:dim(comparar)[1]) {
  for (j in 1:dim(comparar)[2]) {
    if (abs(comparar[i, j]) == min(abs(comparar[i,]))) {
      fila <- c(fila, i) ; columna <- c(columna, j)
    }
  }
}

transf <- c()
for (j in 1:length(columna)) {
  if (columna[j] == 1) {
    transf <- c(transf, 'normal')
  } else if (columna[j] == 2) {
    transf <- c(transf, 'log')
  } else {
    transf <- c(transf, 'sqrt')
  }
} 

final <- data.frame(fila, transf)

View(final)

>> tabla = readtable('mobiles.csv');
>> cuantis = [tabla(:, [1  3  5  7  8  9 10 11 12 13 14 15 16 17])];
>> cuantis = table2array(cuantis);
>> plotmatrix(cuantis)
>> cuantis_transformadas = [cuantis(:, 1), sqrt(cuantis(:, 2)), sqrt(cuantis(:, 3)), cuantis(:, 4), cuantis(:, 5), cuantis(:, 6), cuantis(:, 7), cuantis(:, 8), sqrt(cuantis(:, 9)), cuantis(:, 10), cuantis(:, 11), cuantis(:, 12), sqrt(cuantis(:, 13)), cuantis(:, 14)];
>> plotmatrix(cuantis_transformadas)

mcov <- round(cov(cuantis), 2)
upper<-mcov
upper[upper.tri(mcov)]<-""
upper<-as.data.frame(upper)
upper

mcor <- round(cor(cuantis), 2) 
upper_cor <- mcor
upper_cor[upper.tri(mcor)]<-""
upper_cor<-as.data.frame(upper_cor)
upper_cor
library(xtable)

print.xtable(xtable(upper), type='html', file='filename.html')
print.xtable(xtable(upper_cor), type='html', file='filename1.html')




setwd('C:/Users/Usuario/Downloads')

datos <- read.table("coches.data", sep = ',')
datos <- datos[, c(4, 8, 10, 11, 12, 13, 14, 17, 22, 23, 26)]
colnames(datos) <- c('combustible', 'rueda_motriz', 'distancia_ejes', 'largo', 'ancho', 'altura', 'peso', 'motor', 'caballos', 'max_revoluciones', 'precio')
View(datos)


for (i in 1:dim(datos)[1]) {
  for (j in 1:dim(datos)[2]) {
    if (datos[i, j] == '?') {
      datos <- datos[-c(i),]
    }
  }
}

for (i in 1:length(datos[,1])) {
  if (datos[i,1] == 'gas') {
    datos[i,1] <- 1 
  } else {
    datos[i,1] <- 0
  }
}

for (i in 1:length(datos[,2])) {
  if (datos[i,2] == 'rwd') {
    datos[i,2] <- 1 
  } else if (datos[i, 2] == 'fwd') {
    datos[i,2] <- 2
  } else {
    datos[i, 2] <- 3
  }
}

for (i in 1:dim(datos)[1]) {
  for (j in 1:dim(datos)[2]) {
    as.numeric(datos[i, j])
  }
}


write.csv(df2,"C:/Users/Usuario/opencv/Scripts/Datos/coches.csv", row.names = FALSE)



setwd('C:/Users/Usuario/Desktop/Universidad/Análisis Multivariante/Pepe/Datos')

df2 <- read.csv('coches.csv', header = T, sep = ",", dec = ".")
library(dplyr)
df2 <- mutate_all(df2, function(x) as.numeric(as.character(x)))
View(df2)


library(e1071)

class(df2$precio)


hist(log(df2$precio), freq = F)

hist(log(df2$caballos), freq = F)


hist(log(df2$max_revoluciones), freq = F)



df2$distancia_ejes <- log(df2$distancia_ejes)
df2$largo <- log(df2$largo)
df2$ancho <- log(df2$ancho)
df2$peso <- log(df2$peso)
df2$motor <- log(df2$motor)
df2$altura <- sqrt(df2$altura)
df2$caballos <- log(df2$caballos)
df2$precio <- log(df2$precio)
df2$max_revoluciones <- log(df2$max_revoluciones)

write.csv(df2,"C:/Users/Usuario/opencv/Scripts/Datos/coches_new.csv", row.names = FALSE)

setwd("C:/Users/Usuario/Desktop/Universidad/Análisis Multivariante/Pepe/Datos")

datos <- read.csv('coches.csv', sep = ',', dec = ".")

cuantis <- datos[, c(3, 4, 5, 6, 7 , 8, 9, 10, 11)]

mcov <- round(cor(cuantis), 2)
upper<-mcov
upper[upper.tri(mcov)]<-""
upper<-as.data.frame(upper)
upper
181/201*100
library(xtable)

print.xtable(xtable(upper), type='latex', file='filename.tex')
print.xtable(xtable(upper_cor), type='html', file='filename1.html')