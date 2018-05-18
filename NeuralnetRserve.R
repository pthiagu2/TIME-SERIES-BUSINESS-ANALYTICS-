install.packages("neuralnet")
install.packages("Rserve")
library(neuralnet)
library(Rserve)
data = read.csv("DJIA_table.csv",header = TRUE)
head(data)

totalrows <- nrow(data)
totalrows
samplesize <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = samplesize)

train <- data[train_ind, ]
test <- data[-train_ind, ]
nn <- neuralnet(train$Adj.Close ~ train$Open + train$High + train$Low + train$Close + train$Volume, data, hidden=5, lifesign='full')
predict <- compute(nn, test[2:6])