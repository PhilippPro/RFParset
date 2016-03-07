library("BatchExperiments")
dir = "/home/probst/Random_Forest/RFParset"
#dir = "/home/philipp/Promotion/RandomForest/RFParset/results"
setwd(paste(dir,"/results", sep = ""))
load(paste(dir,"/results/clas.RData", sep = ""))
load(paste(dir,"/results/reg.RData", sep = ""))

setConfig(conf = list(cluster.functions = makeClusterFunctionsMulticore(10)))

tasks = rbind(clas_small, reg_small)
regis = makeExperimentRegistry(id = "par_randomForest2", packages=c("OpenML", "mlr", "randomForest", "ranger"))

# Add problem
gettask = function(static, idi) {
  task = getOMLTask(task.id = idi, verbosity=0)$input$data.set
  list(idi = idi, data = task$data, formula = as.formula(paste(task$target.features,"~.") ), 
       target = task$target.features, mtry_max = static[static$task_id == idi,]$NumberOfFeatures - 1)
}
addProblem(regis, id = "taski", static = tasks, dynamic = gettask, seed = 123)

# Add Algorithms
# forest.wrapper = function(static, dynamic, size = 0, ...) {
#   if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
#     err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$err.rate[,1]
#   } else {
#     err = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, ...)$mse
#   }
#   names(err) = 1:10000
#   list(err = err, datainfo = c(static[static$task_id == dynamic$idi, c(1,2, 15, 13, 14, 18,19)]))
# }
# addAlgorithm(regis, id = "forest.ntree", fun = forest.wrapper, overwrite = TRUE)
# 
# forest.wrapper.mtry = function(static, dynamic, ...) {
#   err = matrix(NA, dynamic$mtry_max, 10001)
#   colnames(err) = c("mtry_max", 1:10000)
#   rownames(err) = 1:dynamic$mtry_max
#   err[, 1] = 1:dynamic$mtry_max
#   for(i in 1:dynamic$mtry_max)
#     err[i, 2:10001] = randomForest(formula = dynamic$formula, data = dynamic$data, replace = TRUE, mtry = i, ...)$mse
#   return(err)
# }
# addAlgorithm(regis, id = "forest.mtry", fun = forest.wrapper.mtry, overwrite = TRUE)

forest.wrapper.nodesize = function(static, dynamic, ...) {
  getConfMatrix2 = function(dynamic, pred, relative = TRUE) {
    cls = levels(dynamic$data[,dynamic$target])
    k = length(cls)
    truth = dynamic$data[,dynamic$target]
    tab = table(truth, pred)
    mt = tab * (matrix(1, ncol = k, nrow = k) - diag(1, k, k))
    rowsum = rowSums(mt)
    colsum = colSums(mt)
    result = rbind(cbind(tab, rowsum), c(colsum, sum(colsum)))
    dimnames(result) = list(true = c(cls, "-SUM-"), predicted = c(cls, "-SUM-"))
    if (relative) {
      total = sum(result[1:k, 1:k])
      k1 = k + 1
      result[k1, 1:k] = if (result[k1, k1] != 0)
        result[k1, 1:k] / result[k1, k1]
      else
        0
      rownorm = function(r, len) {
        if (any(r[1:len] > 0))
          r / sum(r[1:len])
        else
          rep(0, len + 1)
      }
      result[1:k, ] = t(apply(result[1:k, ], 1, rownorm, len = k))
      result[k1, k1] = result[k1, k1] / total
    }
    return(result)
  }
  
  multiclass.auc2 = function(pred, pred2){
    resp = pred2
    predP = pred
    # choose the probablity of the choosen response
    predV = vnapply(seq_row(pred), function(i) {
      pred[i, resp[i]]
    })
    auc = pROC::multiclass.roc(response = pred2, predictor = predV)$auc
    as.numeric(auc)
  }
  
  min.node.size = round(nodesize/(10*4))
  
  dynamic$data[,dynamic$target] = droplevels(dynamic$data[,dynamic$target])
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    pred = ranger(formula = dynamic$formula, data = dynamic$data, replace = TRUE, probability = TRUE,num.trees = 10000, 
                  min.node.size = min.node.size, ... )$predictions
    pred2 = factor(colnames(pred)[max.col(pred)], levels = colnames(pred))
    conf.matrix = getConfMatrix2(dynamic, pred2, relative = TRUE)
    k = nrow(conf.matrix)
    measures = c(measureACC(dynamic$data[,dynamic$target], pred2), mean(conf.matrix[-k, k]), 
                 measureMMCE(dynamic$data[,dynamic$target], pred2), multiclass.auc2(pred, pred2))
    names(measures) = c("ACC", "BER", "MMCE", "multi.AUC")
  } else {
    pred = ranger(formula = dynamic$formula, data = dynamic$data, replace = TRUE, min.node.size = min.node.size, ...)$predictions
    measures = c(measureMAE(dynamic$data[,dynamic$target] , pred),  measureMEDAE(dynamic$data[,dynamic$target], pred), 
                 measureMEDSE(dynamic$data[,dynamic$target], pred), measureMSE(dynamic$data[,dynamic$target], pred))
    names(measures) = c("MAE", "MEDAE", "MEDSE", "MSE")
  }
  list(measures = measures, datainfo = c(static[static$task_id == dynamic$idi, c(1, 2, 15, 13, 14, 18, 19)]), nodesize = min.node.size)
}
addAlgorithm(regis, id = "forest.nodesize", fun = forest.wrapper.nodesize, overwrite = TRUE)

pars = list(idi = tasks$task_id[316])
task.design = makeDesign("taski", exhaustive = pars)
# pars = list(ntree = 10000)
# forest.design.ntree = makeDesign("forest.ntree", exhaustive = pars)
# pars = list(ntree = 10000)
# forest.design.mtry = makeDesign("forest.mtry", exhaustive = pars)
pars = list(num.trees = 10000, nodesize = c(1:10))
forest.design.nodesize = makeDesign("forest.nodesize", exhaustive = pars)

# addExperiments(regis, repls = 30, prob.designs = task.design, algo.designs = list(forest.design.ntree)) # 12.5 h 
# addExperiments(regis, repls = 4, prob.designs = task.design, algo.designs = list(forest.design.mtry)) # 33.33 h
addExperiments(regis, repls = 2, prob.designs = task.design, algo.designs = list(forest.design.nodesize)) # 16.5 h


summarizeExperiments(regis)
testJob(regis)

submitJobs(regis)
#waitForJobs(regis)

regis = loadRegistry("/home/probst/Random_Forest/RFParset/results/par_randomForest-files")
showStatus(regis)

