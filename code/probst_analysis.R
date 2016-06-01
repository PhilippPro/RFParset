library(mlr)
setwd("/nfsmb/koll/probst/Random_Forest/RFParset/results/")
load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")
param_randomForest = list(c("ntree", "mtry", "nodesize", "maxnodes"), c("sampsize", "replace"))
param_ranger = list(c("num.trees", "mtry", "min.node.size"), c("sample.fraction", "replace"))
param_randomForestSRC = list(c("ntree", "mtry", "nodesize", "nodedepth", "splitrule"), c("sampsize", "samptype"))

# get best parameter constellation for all datasets

hyp_par[c(1,5761)] # every 5760 jobs, there are the same hyp.par.settings
res_classif_aggr = matrix(NA, 5760, 8)
for(i in 1:5760){
  print(i)
  res_classif_job = res_classif[which(res_classif$job.id %in% seq(i,1077120, 5760))]
  if (nrow(res_classif_job) == 187) {
    res_classif_aggr[i, ] = colMeans(res_classif_job)
  } 
}
colnames(res_classif_aggr) = colnames(res_classif_job)
save(res_classif_aggr, file = "/nfsmb/koll/probst/Random_Forest/RFParset/results/results_aggr.RData")

# get the 20 best configurations on average for each measure
# And the winner is...
hyp_par[which(res_classif_aggr[, 2] > 0.9)]
res_classif_aggr[which(res_classif_aggr[, 2] >= c(sort(res_classif_aggr[, 2], decreasing = T)[20])), ]
hyp_par[which(res_classif_aggr[, 2] >= c(sort(res_classif_aggr[, 2], decreasing = T)[20])), ]
hyp_par[which(res_classif_aggr[, 3] <= c(sort(res_classif_aggr[, 3], decreasing = F)[20])), ]
hyp_par[which(res_classif_aggr[, 4] <= c(sort(res_classif_aggr[, 4], decreasing = F)[20])), ]
hyp_par[which(res_classif_aggr[, 5] >= c(sort(res_classif_aggr[, 5], decreasing = T)[20])), ]
hyp_par[which(res_classif_aggr[, 6] <= c(sort(res_classif_aggr[, 6], decreasing = F)[20])), ]
hyp_par[which(res_classif_aggr[, 7] <= c(sort(res_classif_aggr[, 7], decreasing = F)[20])), ]
# nodesize-Effekt (+nodedepth) überlagert alles andere!, rfsrc am besten

# randomForest
hyp_par_rf = hyp_par[1:1920]
res_classif_aggr_rf = data.table(res_classif_aggr[1:1920,])
param = param_randomForest

Visualize_results = function(hyp_par_rf, res_classif_aggr_rf, param) {
for(k in colnames(res_classif_aggr_rf)[2:ncol(res_classif_aggr_rf)]) {
  print(k)
  data = cbind(res_classif_aggr_rf[, k, with = F], hyp_par_rf[, unlist(param), with = F])
  boxplot(data[, k, with = F], main = k)

  data[, param[[1]][1] := as.numeric(data[[param[[1]][1]]]), with = FALSE]
  data[, param[[2]][2] := as.factor(data[[param[[2]][2]]]), with = FALSE]
  
  par(mfrow = c(2, 3))
  for(j in colnames(data)[-1])
    plot(data[, k, with = F][[1]] ~ data[, j, with = FALSE][[1]], xlab = j, ylab = colnames(data)[1])
  par(mfrow = c(1, 1))
  data = data[which(!is.na(data[, k, with = F]))]
  task = makeRegrTask(id = "rf", data = data, target = k)
  lrn = makeLearner("regr.randomForest", par.vals = list(ntree = 1000)) #, predict.type = "se")
  model = train(lrn, task)
  pd = generatePartialPredictionData(model, task, param[[1]])
  print(plotPartialPrediction(pd))
  
  # KI's machen wenig sinn, da hier von anderen Effekten überlagert!! (siehe ntree)
  #pd = generatePartialPredictionData(model, task, c("ntree"), fun = function(x) quantile(x, c(.25, .5, .75)))
  #plotPartialPrediction(pd)
  
  pd = generatePartialPredictionData(model, task, param[[2]], interaction = TRUE)
  print(plotPartialPrediction(pd, facet = param[[2]][2]))
  print(plotPartialPrediction(pd))
}
}

pdf("randomForest.pdf",width=13,height=9.5)
Visualize_results(hyp_par[1:1920], param_randomForest)
dev.off()
pdf("ranger.pdf",width=13,height=9.5)
Visualize_results(hyp_par[1921:3840], data.table(res_classif_aggr[1921:3840,]), param_ranger)
dev.off()
pdf("randomForestSRC.pdf",width=13,height=9.5)
Visualize_results(hyp_par_rf = hyp_par[3841:5760], res_classif_aggr_rf = data.table(res_classif_aggr[3841:5760,]), param = param_randomForestSRC)
dev.off()

# 1. Fazit: nodesize sehr klein setzen, maxnodes, nodedepth auf 1
# unwichtiger: ntree groß, mtry ca. 0.6, sampsize


















# specific dataset analysis
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



