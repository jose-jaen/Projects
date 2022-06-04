setwd('/home/jose/Desktop/Mis cosas')

datos <- read.csv('train.csv', header = T, sep = ';', dec = ',')

prueba <- datos[500,]

length(names(prueba))
dim(prueba)

letra <- c() ; pos_letra <- c()
num <- c() ; pos_num <- c()
for (i in 1:dim(prueba)[2]) {
    if (class(prueba[,i]) == 'character') {
        letra <- c(letra, names(prueba)[i])
        pos_letra <- c(pos_letra, i)
    } else {
        num <- c(num, names(prueba)[i])
        pos_num <- c(pos_num, i)
    }
}

for (i in 1:dim(prueba)[1]) {
    for (j in letra) {
        if (prueba[i, j] == '000') {
            prueba[i, j] <- NA
        }
    }
}

length(num) + length(letra) == length(names(prueba))


i <- 0 ; j <- i + 1
while (i < length(x)) {
    j <- i + 1
    while (j <= length(x)) {
        if (x[i] != x[j]) {
            j <- j + 1
        } else {
            i <- i + 1
        }
    }
}


