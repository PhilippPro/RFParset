library(mlr)
library(grid)
library(data.table)
setwd("/nfsmb/koll/probst/Random_Forest/RFParset/results/")
load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")
param_randomForest = list(c("ntree", "mtry", "nodesize", "maxnodes"), c("sampsize", "replace"))
param_ranger = list(c("num.trees", "mtry", "min.node.size"), c("sample.fraction", "replace"))
param_randomForestSRC = list(c("ntree", "mtry", "nodesize", "nodedepth", "splitrule"), c("sampsize", "samptype"))

load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results_aggr.RData")

# Write the algo names to the results
res_classif$algo = NA
res_classif$algo[which(res_classif$job.id %in% c(rep(seq(1,1077120, 5760), each=1920) + rep(0:1919, 187)))] = "randomForest"
res_classif$algo[which(res_classif$job.id %in% c(rep(seq(1921,1077120, 5760), each=1920) + rep(0:1919, 187)))] = "ranger"
res_classif$algo[which(res_classif$job.id %in% c(rep(seq(3841,1077120, 5760), each=1920) + rep(0:1919, 187)))] = "randomForestSRC"
# Write a dataset-id to the results
res_classif$did = ceiling(res_classif$job.id / 5760)

res_classif_aggr = data.table(res_classif_aggr)
res_classif_aggr$algo = NA
res_classif_aggr$algo[1:1920] = "randomForest" 
res_classif_aggr$algo[1921:3840] = "ranger" 
res_classif_aggr$algo[3841:5760] = "randomForestSRC" 
res_classif_aggr$did = 1:5760



Visualize_diff_to_best_avg_result = function(hyp_par, res_aggr_rf, param, res_classif, algor) {
  diffs_all = list()
  for(k in colnames(res_aggr_rf)[2:c(ncol(res_aggr_rf)-3)]) {
    print(k)
    res_aggr_rf = res_aggr_rf[res_aggr_rf$algo == algor]
    did_best = res_aggr_rf[order(res_aggr_rf[res_aggr_rf$algo == algor, k, with = F], decreasing = T)[1]]$did
    
    hyp_par_best = hyp_par[did_best]
    hyp_par_best = hyp_par_best[,unlist(param),with = F]
    
    bests = res_classif[which(res_classif$job.id %in% seq(did_best,1077120, 5760)), ]
    diffs = split(replicate(length(names(hyp_par_best)) + 1,numeric(187)), 1:c(length(names(hyp_par_best)) + 1))
    names(diffs) = c("best", names(hyp_par_best))
    
    for(i in 1:187){
      print(i)
      data = res_classif[res_classif$did == i & res_classif$algo == algor ]
      ids = data$job.id
      data = cbind(data[, k, with = F], hyp_par[hyp_par$job.id %in% data$job.id, unlist(param), with = F])
      data[, param[[1]][1] := as.numeric(data[[param[[1]][1]]]), with = FALSE]
      data[, param[[2]][2] := as.factor(data[[param[[2]][2]]]), with = FALSE]
      
      data = data[which(!is.na(data[, k, with = F]))]
      task = makeRegrTask(id = "rf", data = data, target = k)
      lrn = makeLearner(paste0("regr.", algor), par.vals = list(ntree = 1000)) #, predict.type = "se")
      model = train(lrn, task)
      pred = predict(model, task)
      best = pred$data$response[which(ids %in% bests$job.id)]
      diffs[["best"]][i] = max(pred$data$response) - best  # Hier evtl. auch Modellierung verwenden??
      for(j in names(hyp_par_best)) { # change always one parameter; problem: at the edges of hyp_par, the estimation is not so stable
        param_i = makeMyParamSet(algor)$pars[[j]]
        if(param_i$type == "integer")
          hyp_par_vari = round(seq(param_i$lower, param_i$upper, length.out = 1000))
        if(param_i$type == "numeric")
          hyp_par_vari = seq(param_i$lower, param_i$upper, length.out = 1000)
        if(param_i$type == "discrete")
          hyp_par_vari = as.factor(names(param_i$values))
        hyp_par_test = hyp_par_best[rep(1,length(hyp_par_vari))]
        hyp_par_test[,j := hyp_par_vari, with = F]
        pred_test = predict(model, newdata = hyp_par_test)$data$response
        diffs[[j]][i] = max(pred_test) - best
        predict(model, newdata = hyp_par_best)
      }
    }
    diffs_all = c(diffs_all, list(diffs))
  }
  names(diffs_all) = colnames(res_aggr_rf)[2:c(ncol(res_aggr_rf)-3)]
}

save(diffs, file = "diffs_hyp_par.RData")

lapply(diffs, mean)

plot(lapply(diffs, density)[[4]], col = "red")
lines(lapply(diffs, density)[[1]], col = "black")
lines(lapply(diffs, density)[[2]], col = "blue")
lines(lapply(diffs, density)[[3]], col = "green")
lines(lapply(diffs, density)[[5]], col = "pink")
lines(lapply(diffs, density)[[6]], col = "yellow")
lines(lapply(diffs, density)[[7]], col = "orange")
lines(lapply(diffs, density)[[8]], col = "purple")
legend("topright", c("best",names(hyp_par_best)), col = c("black", "blue", "green", "red", "pink", "yellow", "orange", "purple"), lty=1)
# Fazit: Modellierung klappt nicht so gut, Unterschiede zu klein! Wähle andere Surrogat-Funktion?
# Am meisten Veränderung durch mtry, ntree und nodesize, evtl. noch sampsize
# Quantitativ aber keine Aussage möglich

Visualize_diff_to_best_avg_result(hyp_par = hyp_par, res_aggr_rf = res_classif_aggr, 
                                    param = param_randomForest, algor = "randomForest")
  
Visualize_diff_to_best_avg_result(hyp_par = hyp_par, res_aggr_rf = res_classif_aggr, 
                                  param = param_ranger, algor = "ranger")

Visualize_diff_to_best_avg_result(hyp_par = hyp_par, res_aggr_rf = res_classif_aggr, 
                                  param = param_randomForestSRC, algor = "randomForestSRC")


