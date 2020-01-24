library(psych)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tid)

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
  # plot(data.train[, x], type = "l", main = as.character(x))
  ggplot(as.data.frame(data.train[, x])) +
    geom_line()
}

gg.list <- list()
for (x in 1:(length(data.train) - 2)) {
  # plot(data.train[, x], type = "l", main = as.character(x), ylab = paste0("Y", as.character(x)))
  gg.list[[x]] <- data.train %>%
    ggplot(aes(x = 1:732)) +
    geom_line(aes_q(y = as.name(names(data)[x])))
  
  # print(gg)
}
grid.arrange(gg.list[[1]], 
             gg.list[[2]], 
             gg.list[[3]], 
             gg.list[[4]], 
             gg.list[[5]],
             gg.list[[6]], 
             gg.list[[7]], ncol = 2)             

model <- lm(Y ~ ., data.train)
summary(model)

source("Hellwig Method.R")
hellwig(data.train[, 1], data.train[, -1])
#asf
