library(mlr)

load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")
param_randomForest = list(c("ntree", "mtry", "nodesize", "maxnodes"), c("sampsize", "replace"))
param_ranger = list(c("num.trees", "mtry", "min.node.size"), c("replace", "sample.fraction"))
param_randomForestSRC = list(c("ntree", "mtry", "nodesize", "nodedepth", "splitrule"), c("samptype", "sampsize"))



i = 457
for (i in unique(hyp_par$problem)) {
  hyp_par_i = hyp_par[hyp_par$problem == i]
  hyp_par_i_error = hyp_par_i[!is.na(hyp_par_i$error)] # sample size for randomForest must be high enough to have more than two classes in inbag
  hyp_par_i = hyp_par_i[is.na(hyp_par_i$error)]
  res_classif_i = res_classif[res_classif$job.id %in% hyp_par_i$job.id]
  
  all(res_classif_i$job.id == hyp_par_i$job.id)

  #hyp_par_i_rf = hyp_par_i_rf[res_classif_i_rf$multiclass.au1u > 0.55]
  #res_classif_i_rf = res_classif_i_rf[res_classif_i_rf$multiclass.au1u > 0.55]
  
  # random forest on random forest
  hyp_par_i_rf = hyp_par_i[hyp_par_i$lrn.id == "randomForest"]
  res_classif_i_rf = res_classif_i[hyp_par_i$lrn.id == "randomForest"]
  # for very high sampsize and FALSE = TRUE, there are no observation left for oob-predictions!
  # exclude these:
  hyp_par_i_rf = hyp_par_i_rf[!is.na(res_classif_i_rf$logloss)]
  res_classif_i_rf = res_classif_i_rf[!is.na(res_classif_i_rf$logloss)]  
  
  for(k in colnames(res_classif_i_rf)[2:ncol(res_classif_i_rf)]) {
    print(k)
    data = cbind(res_classif_i_rf[, k, with = F], hyp_par_i_rf[, c("ntree", "replace", "sampsize", "mtry", "nodesize", "maxnodes"), with = F])
    boxplot(data[, k, with = F], main = k)
    data$replace = as.factor(data$replace)
    data$ntree = as.numeric(data$ntree)
    par(mfrow = c(2, 3))
    for(j in colnames(data)[-1])
      plot(data[, k, with = F][[1]] ~ data[, j, with = FALSE][[1]], xlab = j, ylab = colnames(data)[1])
    par(mfrow = c(1, 1))
    task = makeRegrTask(id = "rf", data = data, target = k)
    lrn = makeLearner("regr.randomForest", par.vals = list(ntree = 1000)) #, predict.type = "se")
    model = train(lrn, task)
    pd = generatePartialPredictionData(model, task, c("ntree", "mtry", "nodesize", "maxnodes"))
    pd$data
    print(plotPartialPrediction(pd))
    
    # KI's machen wenig sinn, da hier von anderen Effekten überlagert!! (siehe ntree)
    #pd = generatePartialPredictionData(model, task, c("ntree"), fun = function(x) quantile(x, c(.25, .5, .75)))
    #plotPartialPrediction(pd)
    
    pd = generatePartialPredictionData(model, task, c("sampsize", "replace"), interaction = TRUE)
    print(plotPartialPrediction(pd, facet = "replace"))
  }
}

# get best parameter constellation for all datasets



1077120/1716480 * 298
reduceResultsDataTable(ids = 1691332, fun = function(r) as.data.frame(as.list(r)), reg = regis)
