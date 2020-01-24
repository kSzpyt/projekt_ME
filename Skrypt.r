
































library(fastDummies)
library(psych)
library(ggplot2)
data <-read.csv("IiE20192020dataset19.csv", sep = ";") 

data <- na.omit(data)
dmmy <- dummy_cols(data$X7)
data <- cbind(data, dmmy)
data <- data[,-c(8,9,12)]
colnames(data)[8:9] <- c("A", "B") 
data[, -c(8,9)] <- as.data.frame(lapply(as.list(data[, -c(8,9)]), as.numeric))

#podział na zbiór testowy i uczący się
set.seed(293487)
train.inx <- sample(1:976, 732)
data.train <- data[train.inx,]
data.test <- data[-train.inx,]

#współczynnik zmienności
wsp.zm <- function(dat) 
{
  sd(dat)/mean(dat)
}

for (x in 1:length(data)) {
  plot(data.train[, x], type = "l", main = as.character(x))
}



model <- lm(Y ~ ., data.train)
step(model, direction = 'backward')
