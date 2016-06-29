library(mlr)
library(grid)
library(data.table)
library(ash)
library(beanplot)
setwd("/nfsmb/koll/probst/Random_Forest/RFParset/results/")
load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")
source("/nfsmb/koll/probst/Random_Forest/RFParset/code/probst_defs.R")

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

hyp_par$replace = as.factor(hyp_par$replace)
hyp_par$ntree = as.numeric(hyp_par$ntree)
hyp_par$num.trees = as.numeric(hyp_par$num.trees)

res_classif_aggr = data.table(res_classif_aggr)
res_classif_aggr$algo = NA
res_classif_aggr$algo[1:1920] = "randomForest" 
res_classif_aggr$algo[1921:3840] = "ranger" 
res_classif_aggr$algo[3841:5760] = "randomForestSRC" 
res_classif_aggr$did = 1:5760

decrease = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
names(decrease) = colnames(res_classif_aggr)[2:c(ncol(res_classif_aggr)-3)]

Visualize_diff_to_best_avg_result = function(hyp_par, res_aggr_rf, param, res_classif, algor) {
  diffs_all = list()
  for(k in colnames(res_aggr_rf)[2:c(ncol(res_aggr_rf)-3)]) {
    print(k)
    
    if(as.logical(decrease[k])) { 
       best_fun = function(x) max(x) 
       } else {
       best_fun =   function(x) min(x)
       }
    
    res_aggr_rf = res_aggr_rf[res_aggr_rf$algo == algor]
    did_best = res_aggr_rf[order(res_aggr_rf[res_aggr_rf$algo == algor, k, with = F], decreasing = as.logical(decrease[k]))[1:10]]$did
    
    res_aggr_rf[did %in% did_best]
    
    hyp_par_best_10 = hyp_par[did_best]
    hyp_par_best_10 = hyp_par_best_10[,unlist(param),with = F]
    
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    hyp_par_best = hyp_par_best_10[1,]
    for(i in names(hyp_par_best_10)) 
      if(is.numeric(hyp_par_best_10[[i]]) | is.integer(hyp_par_best_10[[i]])) {
        hyp_par_best[, i] = mean(hyp_par_best_10[[i]])
      } else {
        hyp_par_best[, i] = Mode(hyp_par_best_10[[i]])
      }
    
    
    #bests = res_classif[which(res_classif$job.id %in% seq(did_best,1077120, 5760)), ]
    diffs = split(replicate(length(names(hyp_par_best)) + 1,numeric(187)), 1:c(length(names(hyp_par_best)) + 1))
    names(diffs) = c("best", names(hyp_par_best))
    
    for(i in 1:187){
      print(i)
      data = res_classif[res_classif$did == i & res_classif$algo == algor ]
      ids = data$job.id
      data = cbind(data[, k, with = F], hyp_par[hyp_par$job.id %in% data$job.id, unlist(param), with = F])
      #data[, param[[1]][1] := as.numeric(data[[param[[1]][1]]]), with = FALSE]
      data[, param[[2]][2] := as.factor(data[[param[[2]][2]]]), with = FALSE]
      
      data = data[which(!is.na(data[, k, with = F]))]
      task = makeRegrTask(id = "rf", data = data, target = k)
      lrn = makeLearner(paste0("regr.", algor)) #, predict.type = "se")
      model = train(lrn, task)
      pred = predict(model, task)
      best = predict(model, newdata = hyp_par_best)$data$response
      
      diffs[["best"]][i] = best_fun(pred$data$response) - best  # Hier evtl. auch Modellierung verwenden??
      for(j in names(hyp_par_best)) { # change always one parameter; problem: at the edges of hyp_par, the estimation is not so stable
        param_i = makeMyParamSet(algor)$pars[[j]]
        if(param_i$type == "integer")
          hyp_par_vari = round(seq(param_i$lower, param_i$upper, length.out = 1000))
        if(param_i$type == "numeric")
          hyp_par_vari = seq(param_i$lower, param_i$upper, length.out = 1000)
        if(param_i$type == "discrete" | param_i$type == "logical")
          hyp_par_vari = as.factor(names(param_i$values))
        hyp_par_test = hyp_par_best[rep(1,length(hyp_par_vari))]
        hyp_par_test[,j := hyp_par_vari, with = F]
        pred_test = predict(model, newdata = hyp_par_test)$data$response
        diffs[[j]][i] = best_fun(pred_test) - best
      }
    }
    diffs_all = c(diffs_all, list(diffs))
  }
  names(diffs_all) = colnames(res_aggr_rf)[2:c(ncol(res_aggr_rf)-3)]
  return(diffs_all)
}

plot_diffs = function(diffs) {
  par(mfrow = c(1,2), oma = c(0, 0, 1, 0))
  
  for(j in 1:length(diffs)){
    diff_j = diffs[[j]]
    
    # density estimation
    f = ash1(bin1(diff_j[[1]], nbin=50), 5)
    plot(f, type = "l", xlab = names(diffs)[j], main = paste("Tunability of ", names(diffs)[j]))
    
    for(i in 2:length(diff_j)) {
      if(any(diff_j[[i]] != 0)) {
        f = ash1(bin1(diff_j[[i]], nbin=50), 5)
        lines(f, col = i) 
      }
    }
    legend_loc = c("topleft", "topright")
    legend(legend_loc[as.numeric(decrease[j])+1], c(names(diff_j)), col = 1:length(diff_j), lty=1)
    
    # beanplot
    beanplot(diff_j[which(sapply(diff_j, function(x) any(x != 0)) )], main = paste("Tunability of ", names(diffs)[j]),bw="nrd0")
    mtext(substr(deparse(substitute(diffs)), 7, 1000), outer = TRUE, cex = 1)
    }
}


diffs_randomForest = Visualize_diff_to_best_avg_result(hyp_par = hyp_par, res_aggr_rf = res_classif_aggr, res_classif = res_classif,
                                    param = param_randomForest, algor = "randomForest")
  
diffs_ranger = Visualize_diff_to_best_avg_result(hyp_par = hyp_par, res_aggr_rf = res_classif_aggr,  res_classif = res_classif,
                                  param = param_ranger, algor = "ranger")

diffs_randomForestSRC = Visualize_diff_to_best_avg_result(hyp_par = hyp_par, res_aggr_rf = res_classif_aggr,  res_classif = res_classif,
                                  param = param_randomForestSRC, algor = "randomForestSRC")

save(diffs_randomForest, diffs_ranger, diffs_randomForestSRC, file = "diffs_hyp_par.RData")

load("diffs_hyp_par.RData")

pdf("rf_tunability.pdf",width=13,height=9)
plot_diffs(diffs_randomForest)
plot_diffs(diffs_ranger)
plot_diffs(diffs_randomForestSRC)
dev.off()

# Fazit: Modellierung klappt nicht so gut, Unterschiede zu klein! Wähle andere Surrogat-Funktion?
# Problem: Raum wird nicht so gut abgedeckt und Variierung hängt stark mit "bester" Hyperparametereinstellung zusammen

# randomForest: 
# acc/mmce tunebar durch  sampsize, mtry, nodesize, maxnodes
# ber tunebar durch: sampsize, nodesize, mtry
# multiclass.au1u tunebar durch: sampsize > nodesize
# multiclass.brier tunebar durch: sampsize
# logloss tunebar durch:  sampsize 

# ranger: 
# acc/mmce tunebar durch: sample.fraction, mtry, nodesize
# ber tunebar durch: sample.fraction, mtry, nodesize
# multiclass.au1u tunebar durch: sample.fraction >  nodesize > mtry 
# multiclass.brier tunebar durch: sample.fraction, mtry, nodesize
# logloss tunebar durch:  sample.fraction, mtry, nodesize

# randomForestSRC: 
# acc/mmce tunebar durch: nodesize > splitrule > sampsize , nodedepth, mtry
# ber tunebar durch: nodesize > sampsize, splitrule, mtry, nodedepth
# multiclass.au1u tunebar durch: nodesize > splitrule, nodedepth, mtry
# multiclass.brier tunebar durch: nodesize > splitrule > sampsize
# logloss tunebar durch: nodesize > splitrule, sampsize, samptype

# Am meisten Veränderung durch sampsize, mtry, nodesize
# ntree und replace spielen keine Rolle
# Quantitativ aber keine Aussage möglich










hyp_par_def[c(1,385)] # every 384 jobs, there are the same hyp.par.settings
res_classif_def_aggr = matrix(NA, 384, 8)
for(i in 1:384){
  print(i)
  res_classif_job = res_classif_def[which(res_classif_def$job.id %in% seq(i,71808, 384))]
  if (nrow(res_classif_job) == 187) {
    res_classif_def_aggr[i, ] = colMeans(res_classif_job)
  } 
}
colnames(res_classif_def_aggr) = colnames(res_classif_job)

res_classif_def_aggr


# Vergleich der besten Default mit defaults aus den Paketen

load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")

    job_id = hyp_par_def[lrn.id == "randomForest" & replace == TRUE & sampsize == 0.632 & mtry == "sqrt" & nodesize == "one"]$job.id
    res_classif_def_best = res_classif_def[job.id %in% job_id]
    
    k = "acc"
    res_aggr_rf = res_classif_aggr
    res_aggr_rf = res_aggr_rf[res_aggr_rf$algo == "randomForest"]
    did_best = res_aggr_rf[order(res_aggr_rf[res_aggr_rf$algo == "randomForest", k, with = F], decreasing = as.logical(decrease[k]))[1]]$did
    
    rbind(round(colMeans(res_classif_def_best),4),
          unlist(round(res_aggr_rf[did == did_best, c(1,2,3,4,5,6,7,8), with = F],4)))
    

 # ranger
job_id = hyp_par_def[lrn.id == "ranger" & replace == TRUE & sample.fraction == 1 & mtry == "sqrt" & min.node.size == "one"]$job.id
res_classif_def_best = res_classif_def[job.id %in% job_id]

k = "acc"
res_aggr_rf = res_classif_aggr
res_aggr_rf = res_aggr_rf[res_aggr_rf$algo == "ranger"]
did_best = res_aggr_rf[order(res_aggr_rf[res_aggr_rf$algo == "ranger", k, with = F], decreasing = as.logical(decrease[k]))[1]]$did

rbind(round(colMeans(res_classif_def_best),4),
      unlist(round(res_aggr_rf[did == did_best, c(1,2,3,4,5,6,7,8), with = F],4)))

# randomForestSRC
job_id = hyp_par_def[lrn.id == "randomForestSRC"  & sampsize == 0.632 & mtry == "sqrt" & nodesize == "one" & samptype == "swor" & splitrule == "normal"]$job.id
res_classif_def_best = res_classif_def[job.id %in% job_id]

k = "acc"
res_aggr_rf = res_classif_aggr
res_aggr_rf = res_aggr_rf[res_aggr_rf$algo == "randomForestSRC"]
did_best = res_aggr_rf[order(res_aggr_rf[res_aggr_rf$algo == "randomForestSRC", k, with = F], decreasing = as.logical(decrease[k]))[1]]$did

rbind(round(colMeans(res_classif_def_best),4),
      unlist(round(res_aggr_rf[did == did_best, c(1,2,3,4,5,6,7,8), with = F],4)))


