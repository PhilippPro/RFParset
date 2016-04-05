# Installation of OpenML
install.packages(c("mlr", "checkmate", "data.table", "digest", "RCurl", "stringi", "XML", "RWeka", "devtools"))
devtools::install_github("openml/r")   
library(OpenML)
saveOMLConfig(apikey = "put_here_your_key_from_openml.org")

task = getOMLTask(task.id = 3595, verbosity=0) 
library(randomForestSRC)  
run = matrix(NA, 1000, 2000)  
set.seed(105)
for(i in 1:1000){
  print(paste(i))
  run[i,] = rfsrc(binaryClass ~., data = task$input$data.set$data, ntree = 2000, importance="none", mtry=2, nodesize = 1)$err.rate[,1]
}
runs = apply(run, 2, mean)
quant1 = apply(run, 2, function(x) quantile(x, 0.25))
quant2 = apply(run, 2, function(x) quantile(x, 0.75))
plot(runs, type="l", ylim = c(min(runs, quant1, quant2), max(runs,quant1, quant2)))
lines(1:2000, quant1, col = "red")
lines(1:2000, quant2, col = "green")

# Installation of OpenML
install.packages(c("mlr", "checkmate", "data.table", "digest", "RCurl", "stringi", "XML", "RWeka", "devtools"))
devtools::install_github("openml/r")   
library(OpenML)
saveOMLConfig(apikey = "put_here_your_key_from_openml.org")

task = getOMLTask(task.id = 3595, verbosity=0) 
library(randomForestSRC)  
run = matrix(NA, 1000, 2000)  
set.seed(105)
for(i in 1:1000){
  print(paste(i))
  run[i,] = rfsrc(binaryClass ~., data = task$input$data.set$data, ntree = 2000, importance="none", mtry=2, nodesize = 1)$err.rate[,1]
}
runs = apply(run, 2, mean)
quant1 = apply(run, 2, function(x) quantile(x, 0.25))
quant2 = apply(run, 2, function(x) quantile(x, 0.75))
plot(runs, type="l", ylim = c(min(runs, quant1, quant2), max(runs,quant1, quant2)))
lines(1:2000, quant1, col = "red")
lines(1:2000, quant2, col = "green")

library(randomForest)
run = matrix(NA, 1000, 2000)  
set.seed(105)
for(i in 1:1000){
  print(paste(i))
  run[i,] = randomForest(binaryClass ~., data = task$input$data.set$data, ntree = 2000, mtry=2, nodesize = 1)$err.rate[,1]
}
runs = apply(run, 2, mean)
quant1 = apply(run, 2, function(x) quantile(x, 0.25))
quant2 = apply(run, 2, function(x) quantile(x, 0.75))
plot(runs, type="l", ylim = c(min(runs, quant1, quant2), max(runs,quant1, quant2)))
lines(1:2000, quant1, col = "red")
lines(1:2000, quant2, col = "green")

library(OpenML)
task = getOMLTask(task.id = 3548, verbosity=0) 
library(randomForest)
run = matrix(NA, 1000, 2000)  
set.seed(108)
for(i in 1:1000){
  print(paste(i))
  run[i,] = randomForest(Type ~., data = task$input$data.set$data, ntree = 2000)$err.rate[,1]
}
runs = apply(run, 2, mean)
quant1 = apply(run, 2, function(x) quantile(x, 0.25))
quant2 = apply(run, 2, function(x) quantile(x, 0.75))
plot(runs, type="l", ylim = c(min(runs, quant1, quant2), max(runs,quant1, quant2)))
lines(1:2000, quant1, col = "red")
lines(1:2000, quant2, col = "green")

# why is it like that? Answer...
library(OpenML)
task = getOMLTask(task.id = 3548, verbosity=0) 
library(randomForest)
run = matrix(NA, 1000, 2000)  
set.seed(108)

preds = list(matrix(NA, 200, 27), matrix(NA, 200, 27))
set.seed(123)
for (i in 1:200){
  print(i)
  preds[[1]][i,] = randomForest(Type ~., data = task$input$data.set$data, ntree = 100)$predicted
  preds[[2]][i,] = randomForest(Type ~., data = task$input$data.set$data, ntree = 3000)$predicted
}

sum(apply(preds[[1]], 2, function(x) names(table(x))[which(table(x) == max(table(x)))] ) == apply(preds[[2]], 2, function(x) names(table(x))[which(table(x) == max(table(x)))] ))

erg = list(matrix(NA, 27, 4), matrix(NA, 27, 4))
for (i in 1:4){
  for (j in 1:27){
    erg[[1]][j, i] = sum(preds[[1]][, j] == i)
    erg[[2]][j, i] = sum(preds[[2]][, j] == i)
  }
}

erg_ges = data.frame(erg[[1]][,1:4], erg[[2]][, 1:4], task$input$data.set$data$Type)
colnames(erg_ges) = c(paste("100", c("a","b","c","o")), paste("3000", c("a","b","c","o")), "real_value")
erg_ges
#    100 a 100 b 100 c 100 o 3000 a 3000 b 3000 c 3000 o real_value
#1     88     5   107     0     18      0    182      0          a
#2    173    27     0     0    200      0      0      0          a
#3    200     0     0     0    200      0      0      0          a
#4     84   116     0     0     45    155      0      0          a
#5      4   196     0     0      0    200      0      0          a
#6    176    24     0     0    200      0      0      0          a
#7      2   198     0     0      0    200      0      0          b
#8    199     1     0     0    200      0      0      0          b
#9    103    97     0     0    152     48      0      0          b
#10     0   200     0     0      0    200      0      0          b
#11     0   200     0     0      0    200      0      0          b
#12     0     0   200     0      0      0    200      0          b
#13     2   198     0     0      0    200      0      0          b
#14     0   200     0     0      0    200      0      0          b
#15     3   197     0     0      0    200      0      0          b
#16     0   198     2     0      0    200      0      0          b
#17     0     2   198     0      0      0    200      0          c
#18     0    10   190     0      0      0    200      0          c
#19     0   171    29     0      0    200      0      0          c
#20     0    94    66    40      0    188     12      0          c
#21     0    23   177     0      0      0    200      0          c
#22     5   195     0     0      0    200      0      0          b
#23     0   107    93     0      0    134     66      0          c
#24     0   198     1     1      0    200      0      0          b
#25   198     2     0     0    200      0      0      0          a
#26    31   155     0    14      0    200      0      0          o
#27     0   200     0     0      0    200      0      0          o


# ranger
# AUC

library(OpenML)
library(mlr)
task = getOMLTask(task.id = 3595, verbosity=0) 
target = task$input$data.set$target
data = task$input$data.set$data
library(randomForestSRC)
measures = list(matrix(NA, 100, 4), matrix(NA, 100, 4))
set.seed(108)
ntrees = c(100,3000)
for(i in 1:100){
  print(paste(i))
  for(j in 1:2){
    pred = rfsrc(binaryClass ~., data = data, ntree = ntrees[j] , importance="none", mtry=2, nodesize = 1)$predicted.oob
    pred2 = factor(colnames(pred)[max.col(pred)], levels = colnames(pred))
    conf.matrix = getConfMatrix2(task$input$data.set, pred2, relative = TRUE)
    k = nrow(conf.matrix)
    AUC = -1
    AUCtry = try(multiclass.auc2(pred, data[,target]))
    if(is.numeric(AUCtry))
      AUC = AUCtry
    measures[[j]][i, ] = c(measureACC(data[, target], pred2), mean(conf.matrix[-k, k]), 
                           measureMMCE(data[, target], pred2), AUC)
  }
}
# Balanced Error Rate
mean(measures[[1]][,2])
# 0.1610566
mean(measures[[2]][,2])
#  0.1652832
# Mean Missclassification Error
mean(measures[[1]][,3])
# 0.1329545
 mean(measures[[2]][,3])
# 0.1361364
#AUC
 mean(measures[[1]][,4])
# 0.7696296
 mean(measures[[2]][,4])
# 0.7755664


