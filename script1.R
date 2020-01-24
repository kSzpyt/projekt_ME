library(psych)

data <- read.csv("IiE20192020dataset19.csv", sep = ";")


set.seed(293487)
inx <- sample(1:1000, 750)
data.train <- data[inx, ]
data.test <- data[-inx, ]

data <- na.omit(data)
data[, -8] <- as.data.frame(lapply(as.list(data[, -8]), as.numeric))

set.seed(293487)
inx <- sample(1:1000, 750)
data.train <- data[inx, ]
data.test <- data[-inx, ]

wsp.zm <- function(dat) 
{
  sd(dat)/mean(dat)
}
zm.df <- t(as.data.frame(lapply(as.list(data.train[, -8]), wsp.zm)))
colnames(zm.df) <- "wsp.zm"
zm.df <- rbind(zm.df, NA)
rownames(zm.df) <- colnames(data.train)

cbind(describe(data.train)[c(3, 4, 11, 12)], zm.df)

for (x in 1:length(data.train)) {
  plot(data.train[, x], type = "l", main = as.character(x))
}


model <- lm(Y ~ ., data.train)
summary(model)

source("Hellwig Method.R")
hellwig(data.train[, 1], data.train[, -1])
#asf
