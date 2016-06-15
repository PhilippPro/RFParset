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

Visualize_diff_to_best_avg_result = function(hyp_par_rf, res_aggr_rf, param, res_classif) {
  for(k in colnames(res_aggr_rf)[2:c(ncol(res_aggr_rf)-3)]) {
    def_best = order(res_classif_aggr[, k], decreasing = T)[1]
    def_best = res_classif[which(res_classif$job.id %in% seq(best,1077120, 5760)), ]
    a = res_classif[ , max(acc), by = .(algo,did)]
    setkey(a, "algo")
    hist(def_best$acc - a[a$algo == "randomForest"]$V1)
  }
}

    
Visualize_diff_to_best_avg_result(hyp_par_rf = hyp_par[1:1920], res_aggr_rf = data.table(res_classif_aggr[1:1920,]), param = param_randomForest)