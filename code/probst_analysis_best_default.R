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
  hyp_par_best_all = list()
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
    hyp_par_best_i = diffs
    
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
        hyp_par_best_i[[j]][i] = hyp_par_vari[best_fun(pred_test) == pred_test][1]
      }
    }
    diffs_all = c(diffs_all, list(diffs))
    hyp_par_best_all = c(hyp_par_best_all, list(hyp_par_best_i))
  }
  names(diffs_all) = colnames(res_aggr_rf)[2:c(ncol(res_aggr_rf)-3)]
  names(hyp_par_best_all) = colnames(res_aggr_rf)[2:c(ncol(res_aggr_rf)-3)]
  
  return(list(diffs_all = diffs_all, hyp_par_best_all = hyp_par_best_all))
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

save(diffs_randomForest, diffs_ranger, file = "diffs_hyp_par.RData")

load("diffs_hyp_par.RData")

pdf("rf_tunability.pdf",width=18,height=9)
plot_diffs(diffs_randomForest$diffs_all)
plot_diffs(diffs_ranger$diffs_all)
plot_diffs(diffs_randomForestSRC$diffs_all)
dev.off()

pdf("rf_tuning_ranges.pdf", width=16, height=10)
par(mfrow = c(2,3), oma = c(0, 0, 2, 0))

#for(j in 1:6){
#  for(i in 2:7)
#    boxplot(diffs_randomForest$hyp_par_best_all[[j]][[i]], main = paste("Range of ", names(diffs_randomForest$hyp_par_best_all$acc)[i]))
#  mtext(paste("randomForest", names(diffs_randomForest$hyp_par_best_all)[j]), outer = TRUE, cex = 1)
#}

for(j in 1:6){
  for(i in 2:7)
    beanplot(diffs_randomForest$hyp_par_best_all[[j]][[i]], main = paste("Range of ", names(diffs_randomForest$hyp_par_best_all$acc)[i]),bw="nrd0")
  mtext(paste("randomForest", names(diffs_randomForest$hyp_par_best_all)[j]), outer = TRUE, cex = 1)
}
for(j in 1:6){
  par(mfrow = c(2,3), oma = c(0, 0, 2, 0))
  for(i in 2:6)
    beanplot(diffs_ranger$hyp_par_best_all[[j]][[i]], main = paste("Range of ", names(diffs_ranger$hyp_par_best_all$acc)[i]),bw="nrd0")
  mtext(paste("ranger", names(diffs_ranger$hyp_par_best_all)[j]), outer = TRUE, cex = 1)
}

for(j in 1:6){
  par(mfrow = c(2,3), oma = c(0, 0, 2, 0))
  for(i in 2:6)
    beanplot(diffs_randomForestSRC$hyp_par_best_all[[j]][[i]], main = paste("Range of ", names(diffs_randomForestSRC$hyp_par_best_all$acc)[i]),bw="nrd0")
  mtext(paste("randomForestSRC", names(diffs_randomForestSRC$hyp_par_best_all)[j]), outer = TRUE, cex = 1)
}
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




# AB hier müllig!

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

res_classif_def_aggr[1:64,][res_classif_def_aggr[1:64,2] == max(res_classif_def_aggr[1:64,2])]

hyp_par_def[order(res_classif_def_aggr[1:64, 2], decreasing = T)[1:5]]
hyp_par_def[order(res_classif_def_aggr[65:132, 2], decreasing = T)[1:5] + 64]
hyp_par_def[order(res_classif_def_aggr[133:384, 2], decreasing = T)[1:5] + 132]
# Das ist kein Zufall!

res_classif_def_aggr = data.table(res_classif_def_aggr)
res_classif_def_aggr$algo = c(rep("randomForest", 64), rep("ranger", 64), rep("randomForestSRC", 256))


res_regr_def$did = floor((res_regr_def$job.id-1)/384)
res_regr_def$algo = res_regr_def$job.id%%384
res_regr_def_rank = res_regr_def[, list(algo, mse = rank(mse, na.last = "keep"),
                                        mae = rank(mae, na.last = "keep"),
                                        medae = rank(medae, na.last = "keep"),
                                        medse = rank(medse, na.last = "keep")), by = did]


res_regr_def_rank_na = res_regr_def_rank[,list(mse = mean(mse, na.rm=T), mae = mean(mae, na.rm=T), 
                                     medae = mean(medae, na.rm=T), 
                                     medse  = mean(medse , na.rm=T)), by = algo]

hyp_par_def[order(res_regr_def_rank_na[1:64,2, with =F], decreasing = F)[1:5] ]
hyp_par_def[order(res_regr_def_rank_na[65:132,2, with =F], decreasing = F)[1:5] + 64]
hyp_par_def[order(res_regr_def_rank_na[133:384,2, with =F], decreasing = F)[1:5] +132]

res_regr_def[1:385]

res_regr_def_aggr = matrix(NA, 384, 6)
colnames(res_regr_def_aggr) = colnames(res_regr_job)
res_regr_def_aggr[order(res_regr_def_aggr[1:64, 2], decreasing = F)[1:5],]

hyp_par_def[order(res_regr_def_aggr[1:64, 2], decreasing = F)[1:5]]
hyp_par_def[order(res_regr_def_aggr[65:132, 2], decreasing = F)[1:5] + 64]
hyp_par_def[order(res_regr_def_aggr[133:384, 2], decreasing = F)[1:5] + 132]

  
# Vergleich der besten Default mit defaults aus den Paketen

    job_id = hyp_par_def[lrn.id == "randomForest" & replace == TRUE & sampsize == 1 & mtry == "sqrt" & nodesize == "one"]$job.id
    res_classif_def_best = res_classif_def[job.id %in% job_id]
    res_def = round(colMeans(res_classif_def_best),4)
    job_id = hyp_par_def[lrn.id == "ranger" & replace == TRUE & sample.fraction == 1 & mtry == "sqrt" & min.node.size == "one"]$job.id
    res_classif_def_best = res_classif_def[job.id %in% job_id]
    res_def = rbind(res_def, round(colMeans(res_classif_def_best),4))
    job_id = hyp_par_def[lrn.id == "randomForestSRC"  & sampsize == 1 & mtry == "sqrt" & nodesize == "one" & samptype == "swr" & splitrule == "normal"]$job.id
    res_classif_def_best = res_classif_def[job.id %in% job_id]
    res_def = rbind(res_def, round(colMeans(res_classif_def_best),4))
    
    k = "acc"
    res_aggr_rf = res_classif_aggr
    bad_ids = which(res_aggr_rf[res_aggr_rf$algo == "randomForest"]$multiclass.au1u > 0.85)
    bad_ids = c(bad_ids, which(hyp_par[1921:3840]$sample.fraction < 0.1) + 1920)
    bad_ids = c(bad_ids, which(hyp_par[3841:5760]$sampsize < 0.1 | hyp_par[3841:5760]$sampsize > 0.9 |  hyp_par[3841:5760]$nodesize > 0.3) + 3840)
    
    res_aggr_rf = res_aggr_rf[res_aggr_rf$algo == "randomForest"]
    did_best1 = res_aggr_rf[order(res_aggr_rf[res_aggr_rf$algo == "randomForest", k, with = F], decreasing = as.logical(decrease[k]))[1]]$did
    hyp_par[did_best1]
    
    rbind(round(colMeans(res_classif_def_best),4),
          unlist(round(res_aggr_rf[did == did_best1, 1:8, with = F],4)),
          unlist(res_classif_def_aggr[which(res_classif_def_aggr[1:64,2, with = F] == max(res_classif_def_aggr[1:64,2, with = F])), 1:8, with=F]))
    # Verbesserung um 0.0014 (immerhin)
    
    
    # LOOCV
    # mit data.table
    did_best = replicate(6, matrix(NA, 187, 3), simplify=F)
    names(did_best) = colnames(res_classif)[2:7]
    
    hyp_par[c(1,5761)] # every 5760 jobs, there are the same hyp.par.settings
    
    for(j in 1:187){
      print(paste(j, "HAAAAAAALLO"))
      
      res_classif$hyp_id = res_classif$job.id - 5760 * (res_classif$did - 1)
      res_eval = res_classif[res_classif$did != j]
      setkey(res_eval, hyp_id)
      res_eval_mean = res_eval[,list(algo = algo[1], acc = mean(acc), ber = mean(ber), mmce = mean(mmce), multiclass.au1u = mean(multiclass.au1u), 
                                     multiclass.brier = mean(multiclass.brier), logloss = mean(logloss)), by = hyp_id]
      res_eval_mean[hyp_id %in% bad_ids]$multiclass.au1u = 0 # mögliche unrealistische werte auf 0 setzen
      
      res_eval_length = res_eval[,list(acc = length(acc), ber = length(ber), mmce = length(mmce), multiclass.au1u = length(multiclass.au1u), 
                                       multiclass.brier = length(multiclass.brier), logloss = length(logloss)), by = hyp_id]
      
      # pro Algorithmus!
      res_eval_mean2 = res_eval_mean[c(res_eval_length[,acc] == 186),]
      did_best$acc[j,] = res_eval_mean2[, hyp_id[which.max(acc)], by = algo]$V1
      res_eval_mean2 = res_eval_mean[c(res_eval_length[,ber] == 186),]
      did_best$ber[j,] = res_eval_mean2[, hyp_id[which.min(ber)], by = algo]$V1
      res_eval_mean2 = res_eval_mean[c(res_eval_length[,mmce] == 186),]
      did_best$mmce[j,] =res_eval_mean2[, hyp_id[which.min(mmce)], by = algo]$V1
      res_eval_mean2 = res_eval_mean[c(res_eval_length[,multiclass.au1u] == 186),]
      did_best$multiclass.au1u[j,] = res_eval_mean2[, hyp_id[which.max(multiclass.au1u)], by = algo]$V1
      res_eval_mean2 = res_eval_mean[c(res_eval_length[,multiclass.brier] == 186),]
      did_best$multiclass.brier[j,] =res_eval_mean2[, hyp_id[which.min(multiclass.brier)], by = algo]$V1
      res_eval_mean2 = res_eval_mean[c(res_eval_length[,logloss] == 186),]
      did_best$logloss[j,] = res_eval_mean2[, hyp_id[which.min(logloss)], by = algo]$V1
    }
    
   
    res_def = res_def[, -c(1, 8)]
    rownames(res_def) = c("randomForest", "ranger", "randomForestSRC")
    
    pdf("comparison_mydef_to_standarddef_loocv.pdf",width=18,height=9)
    for(j in names(did_best)) {
      res_table = data.frame()      
      
      res_auswahl = res_classif[res_classif$job.id %in% c(did_best[[j]] + 0:186 * 5760),]
      res_table = rbind(res_table, res_auswahl[,mean(acc), by = algo]$V1)
      # häufigste parametereinstellung
      hyp_par[as.numeric(apply(did_best$acc, 2, function(x) names(table(x)[1])))]
      
      res_auswahl = res_classif[res_classif$job.id %in% c(did_best[[j]] + 0:186 * 5760),]
      res_table = rbind(res_table, res_auswahl[,mean(ber), by = algo]$V1)
      hyp_par[as.numeric(apply(did_best$ber, 2, function(x) names(table(x)[1])))]
      
      res_auswahl = res_classif[res_classif$job.id %in% c(did_best[[j]] + 0:186 * 5760),]
      res_table = rbind(res_table, res_auswahl[,mean(mmce), by = algo]$V1)
      hyp_par[as.numeric(apply(did_best$mmce, 2, function(x) names(table(x)[1])))]
      
      # geht nicht!
      res_auswahl = res_classif[res_classif$job.id %in% c(did_best[[j]] + 0:186 * 5760),]
      res_table = rbind(res_table, res_auswahl[,mean(multiclass.au1u), by = algo]$V1)
      hyp_par[as.numeric(apply(did_best$multiclass.au1u, 2, function(x) names(table(x)[1])))]
      
      res_auswahl = res_classif[res_classif$job.id %in% c(did_best[[j]] + 0:186 * 5760),]
      res_table = rbind(res_table, res_auswahl[,mean(multiclass.brier), by = algo]$V1)
      hyp_par[as.numeric(apply(did_best$multiclass.brier, 2, function(x) names(table(x)[1])))]
      
      res_auswahl = res_classif[res_classif$job.id %in% c(did_best[[j]] + 0:186 * 5760),]
      res_table = rbind(res_table, res_auswahl[,mean(logloss), by = algo]$V1)
      hyp_par[as.numeric(apply(did_best$logloss, 2, function(x) names(table(x)[1])))]
      
      
      colnames(res_table) = c("randomForest", "ranger", "randomForestSRC")
      res_table = t(res_table)
      colnames(res_table) = c("acc", "ber", "mmce", "multiclass.au1u", "multiclass.brier", "logloss")
      # logloss stark reduziert, andere Ergebnisse sehr ähnlich zu den normalen besten defaults
      
      plot_data = data.frame(matrix(NA, 6, 8))
      colnames(plot_data) = c("which_default", "algo" , colnames(res_table))
      plot_data$which_default = c(rep("my_default", 3), rep("standard_default", 3))
      plot_data$algo = rep(c("randomForest", "ranger", "randomForestSRC"))
      
      for(i in colnames(res_table)) {
        plot_data[, i] = c(res_table[,i], res_def[,i])
      }
      
      p1 = ggplot(plot_data, aes(factor(algo), acc,  fill = which_default)) + 
        geom_bar(stat="identity", position = "dodge") + coord_cartesian(ylim = range(plot_data[,"acc"]))
      p2 = ggplot(plot_data, aes(factor(algo), ber,  fill = which_default)) + 
        geom_bar(stat="identity", position = "dodge") + coord_cartesian(ylim = range(plot_data[,"ber"]))
      p3 = ggplot(plot_data, aes(factor(algo), mmce,  fill = which_default)) + 
        geom_bar(stat="identity", position = "dodge") + coord_cartesian(ylim = range(plot_data[,"mmce"]))
      p4 = ggplot(plot_data, aes(factor(algo), multiclass.au1u,  fill = which_default)) + 
        geom_bar(stat="identity", position = "dodge") + coord_cartesian(ylim = range(plot_data[,"multiclass.au1u"]))
      p5 = ggplot(plot_data, aes(factor(algo), multiclass.brier,  fill = which_default)) + 
        geom_bar(stat="identity", position = "dodge") + coord_cartesian(ylim = range(plot_data[,"multiclass.brier"]))
      p6 = ggplot(plot_data, aes(factor(algo), logloss,  fill = which_default)) + 
        geom_bar(stat="identity", position = "dodge") + coord_cartesian(ylim = range(plot_data[,"logloss"]))
      
      library(gridExtra)    
      print(grid.arrange(p1, p2, p3, p4, p5, p6, nrow=2, ncol=3, top = paste(j, "optimized")))
    }
    dev.off()
# Fazit: Die "neuen" Defaults sind kaum besser, Ergebnisse ähnlich. 

    

    
# Tuning ranges
  res_eval = res_classif[, .(hyp_id = hyp_id[which.min(acc)]), by = .(algo, did)]
  hyp_par_best = hyp_par[sort(unique(res_eval$hyp_id))]
  hyp_par_best[,.(min(ntree), max(ntree)), by = lrn.id]
  # klappt nicht gut, ganze Bandbreite wird abgedeckt
    
  
  # benutze stattdessen modellierte beste ergebnisse und deren bandbreite
    
    
    
    
    
    