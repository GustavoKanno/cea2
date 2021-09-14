

setwd("C:\\Users\\Rodrigo Araujo\\Documents\\IME-USP\\CEA 2")

data = read.csv('dataset_utrassom.csv', sep = ';', dec = ',')
head(data)


data2 = data[-c(11, 12),]


## Medias
colMeans(data[, -1], na.rm = TRUE)

#install.packages('matrixStats')
require(matrixStats)

paste(colnames(data[, -1]), colSds(as.matrix(data[, -1]), na.rm = TRUE))
#colSds(as.matrix(data[, -1]), na.rm = TRUE)



rownames(data2) <- seq(1, 23)

simulacao <- function(A, B, k){
  
  (A*(1 + (-1*exp(-1*k*x))) + B)
  
}

x <- seq(0, 70, 0.01)



data_y <- data.frame('y' = rep(0, 7001))

for (i in 1:nrow(data2)){
  
  y_1 <- simulacao(data2[i, 2], data2[i, 3], data2[i, 4])
  data_y <- cbind(data_y, y_1)
  
}

curvas = rep('curva', ncol(data_y))
curvas2 = seq(0, 23)
curvas2 = as.character(curvas2)

names_data_y <- paste(curvas, curvas2)

colnames(data_y) <- names_data_y


#colnames(data_y) <- c('curva0', 'curva1', 'curva2', 'curva3', 'curva4', 'curva5', 'curva6', 'curva7', 'curva8', 'curva9')

data_y$X <- x


require(ggplot2)

ggplot(data_y, aes(X)) +                    
  geom_line(aes(y=`curva 1`), colour="red") + 
  geom_line(aes(y=`curva 2`), colour="red") +
  geom_line(aes(y=`curva 3`), colour="red") +
  geom_line(aes(y=`curva 4`), colour="red") +
  geom_line(aes(y=`curva 5`), colour="red") +
  geom_line(aes(y=`curva 6`), colour="red") +
  geom_line(aes(y=`curva 7`), colour="red") +
  geom_line(aes(y=`curva 8`), colour="red") +
  geom_line(aes(y=`curva 9`), colour="red") +
  geom_line(aes(y=`curva 10`), colour="red") + 
  geom_line(aes(y=`curva 11`), colour="red") +
  geom_line(aes(y=`curva 12`), colour="red") +
  geom_line(aes(y=`curva 13`), colour="red") +
  geom_line(aes(y=`curva 14`), colour="red") +
  geom_line(aes(y=`curva 15`), colour="blue") +
  geom_line(aes(y=`curva 16`), colour="blue") +
  geom_line(aes(y=`curva 17`), colour="blue") +
  geom_line(aes(y=`curva 18`), colour="blue") +
  geom_line(aes(y=`curva 19`), colour="blue") +
  geom_line(aes(y=`curva 20`), colour="blue") +
  geom_line(aes(y=`curva 21`), colour="blue") +
  geom_line(aes(y=`curva 22`), colour="blue") +
  geom_line(aes(y=`curva 23`), colour="blue") +
  ylim(-70, -35)







ggplot(data_y, aes(X)) +                    
  geom_line(aes(y=`curva 1`), colour="red") + 
  geom_line(aes(y=`curva 2`), colour="red") +
  geom_line(aes(y=`curva 3`), colour="red") +
  geom_line(aes(y=`curva 4`), colour="red") +
  geom_line(aes(y=`curva 5`), colour="red") +
  geom_line(aes(y=`curva 6`), colour="red") +
  geom_line(aes(y=`curva 7`), colour="red") +
  geom_line(aes(y=`curva 8`), colour="red") +
  geom_line(aes(y=`curva 9`), colour="red") +
  geom_line(aes(y=`curva 10`), colour="red") + 
  geom_line(aes(y=`curva 11`), colour="red") +
  geom_line(aes(y=`curva 12`), colour="red") +
  geom_line(aes(y=`curva 13`), colour="red") +
  geom_line(aes(y=`curva 14`), colour="red") +
  ylim(-100, 0)





ggplot(data_y, aes(X)) +                    
  geom_line(aes(y=`curva 15`), colour="blue") +
  geom_line(aes(y=`curva 16`), colour="blue") +
  geom_line(aes(y=`curva 17`), colour="blue") +
  geom_line(aes(y=`curva 18`), colour="blue") +
  geom_line(aes(y=`curva 19`), colour="blue") +
  geom_line(aes(y=`curva 20`), colour="blue") +
  geom_line(aes(y=`curva 21`), colour="blue") +
  geom_line(aes(y=`curva 22`), colour="blue") +
  geom_line(aes(y=`curva 23`), colour="blue") +
  ylim(-100, 0)





