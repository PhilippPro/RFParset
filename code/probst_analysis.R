library(mlr)
library(grid)
library(data.table)
setwd("/nfsmb/koll/probst/Random_Forest/RFParset/results/")
load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")
param_randomForest = list(c("ntree", "mtry", "nodesize", "maxnodes"), c("sampsize", "replace"))
param_ranger = list(c("num.trees", "mtry", "min.node.size"), c("sample.fraction", "replace"))
param_randomForestSRC = list(c("ntree", "mtry", "nodesize", "nodedepth", "splitrule"), c("sampsize", "samptype"))

# classification
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

load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results_aggr.RData")

# get the 20 best configurations on average for each measure
# And the winner is...
hyp_par[which(res_classif_aggr[, 2] > 0.9)]
res_classif_aggr[which(res_classif_aggr[, 2] >= c(sort(res_classif_aggr[, 2], decreasing = T)[20])), ]

interest = c("lrn.id", "ntree", "replace", "sampsize", "mtry", "nodesize", "maxnodes", "num.trees", "sample.fraction", "min.node.size", "samptype", "nodedepth", "splitrule")
hyp_par[order(res_classif_aggr[, 2], decreasing = T)[1:10], interest , with = F]

res_classif_aggr[order(res_classif_aggr[, 2], decreasing = T)[1:10],]

hyp_par[order(res_classif_aggr[, 3], decreasing = F)[1:10], ]
hyp_par[order(res_classif_aggr[, 4], decreasing = F)[1:10], ]
hyp_par[order(res_classif_aggr[, 5], decreasing = T)[1:10], ]
hyp_par[order(res_classif_aggr[, 6], decreasing = F)[1:10], ]
hyp_par[order(res_classif_aggr[, 7], decreasing = F)[1:10], ]
# nodesize-Effekt (+nodedepth) überlagert alles andere!, rfsrc am besten
# beste Einstellung: randomForestSRC, ntree = 9903, sampsize = 0.880, mtry = 0.566, nodesize = 0.0098, samptype = "swr", 
# nodedepth = 0.855, splitrule = "normal"

# Visualization
Visualize_results = function(hyp_par_rf, res_aggr_rf, param) {
for(k in colnames(res_aggr_rf)[2:ncol(res_aggr_rf)]) {
  print(k)
  data = cbind(res_aggr_rf[, k, with = F], hyp_par_rf[, unlist(param), with = F])
  boxplot(data[, k, with = F], main = k)

  data[, param[[1]][1] := as.numeric(data[[param[[1]][1]]]), with = FALSE]
  data[, param[[2]][2] := as.factor(data[[param[[2]][2]]]), with = FALSE]
  
  par(mfrow = c(2, 3))
  for(j in colnames(data)[-1])
    plot(data[, k, with = F][[1]] ~ data[, j, with = FALSE][[1]], xlab = j, ylab = colnames(data)[1])
  
  # Analysis of the thick lines in the plots
  # abline(lm(data[, k, with = F][[1]] ~ data[, j, with = FALSE][[1]])$coefficients)
  # # 0.75034, -0.02645
  # j = "sampsize"
  # abnormal = which((data[, k, with = F][[1]] < 0.75534 -0.02645 * data[, j, with = FALSE][[1]]) & (data[, k, with = F][[1]] > 0.74034 -0.02645 * data[, j, with = FALSE][[1]]))
  # par(mfrow = c(2, 3))
  # for(j in colnames(data)[-1])
  # plot(data[abnormal, k, with = F][[1]] ~ data[abnormal, j, with = FALSE][[1]], xlab = j, ylab = colnames(data)[1])

  par(mfrow = c(1, 1))
  data = data[which(!is.na(data[, k, with = F]))]
  task = makeRegrTask(id = "rf", data = data, target = k)
  lrn = makeLearner("regr.randomForest", par.vals = list(ntree = 1000)) #, predict.type = "se")
  model = train(lrn, task)
  pd = generatePartialPredictionData(model, task, param[[1]], gridsize = 20)
  print(plotPartialPrediction(pd))
  
 # Interactions
 # pd = generatePartialPredictionData(model, task, param[[1]][c(1,3)], interaction = TRUE, gridsize = 20)
 # print(plotPartialPrediction(pd), facet = param[[1]][3])
  
 # par(mfrow = c(4, 5))
 # for(i in 1:20) {
 #  plot(pd$data[c((i-1)*20 + 1):c(i*20), 2], pd$data[c((i-1)*20 + 1):c(i*20), 1], main = paste("nodesize", round(pd$data[c(i-1)*20 +1, 3],2)), ylab = k, xlab = colnames(pd$data)[2])
 #}
    
  
  # KI's machen wenig sinn, da hier von anderen Effekten überlagert!! (siehe ntree)
  #pd = generatePartialPredictionData(model, task, c("ntree"), fun = function(x) quantile(x, c(.25, .5, .75)))
  #plotPartialPrediction(pd)
  
  pd = generatePartialPredictionData(model, task, param[[2]], interaction = TRUE, gridsize = 20)
  pd$data = pd$data[-c(1,21),]
  print(plotPartialPrediction(pd, facet = param[[2]][2]))
  print(plotPartialPrediction(pd))
}
}

pdf("randomForest2.pdf",width=15,height=9.5)
Visualize_results(hyp_par_rf = hyp_par[1:1920], res_aggr_rf = data.table(res_classif_aggr[1:1920,]), param = param_randomForest)
dev.off()
pdf("ranger.pdf",width=13,height=9.5)
Visualize_results(hyp_par[1921:3840], data.table(res_classif_aggr[1921:3840,]), param_ranger)
dev.off()
pdf("randomForestSRC.pdf",width=13,height=9.5)
Visualize_results(hyp_par_rf = hyp_par[3841:5760], data.table(res_classif_aggr[3841:5760,]), param = param_randomForestSRC)
dev.off()

# 1. Fazit: nodesize sehr klein setzen, 
# maxnodes auf 1
# nodedepth auf 1
# unwichtiger: ntree groß, mtry ca. 0.6, sampsize
# Inkonsistenz bei sampsize bei den verschiedenen Algos. randomForest ist seltsam...
# randomForestSRC besser, braucht aber auch länger
# splitrule normal

# Regression
# scale the measures by computing rankings for each dataset
res_regr_rank = as.data.frame(res_regr)
for(i in 1:111){
  print(i)
  ordered_matrix = apply(res_regr[which(res_regr$job.id %in% (c((i-1)*5760+1077121):c(i*5760+1077120)))], 2, rank)
  res_regr_rank[which(res_regr$job.id %in% (c((i-1)*5760+1077121):c(i*5760+1077120))),] = ordered_matrix
}

# get best parameter constellation for all datasets
hyp_par[c(1077121, 1077122)] # every 5760 jobs, there are the same hyp.par.settings
res_regr_aggr = matrix(NA, 5760, 6)
for(i in 1:5760){
  print(i)
  res_regr_job = res_regr_rank[which(res_regr$job.id %in% seq(i+1077120, 1716480, 5760)),]
  if (nrow(res_regr_job) == 111) {
    res_regr_aggr[i, ] = colMeans(res_regr_job)
  } 
}
colnames(res_regr_aggr) = colnames(res_regr_job)
colnames(res_regr_aggr)[2:5] = paste(colnames(res_regr_aggr)[2:5], "rank", sep = "_")
save(res_classif_aggr, res_regr_aggr, file = "/nfsmb/koll/probst/Random_Forest/RFParset/results/results_aggr.RData")

load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results_aggr.RData")

res_regr_aggr[order(res_regr_aggr[, 2], decreasing = F)[1:30], ]
res_regr_aggr[order(res_regr_aggr[, 6], decreasing = F)[1:10],]

hyp_par[order(res_regr_aggr[, 2], decreasing = F)[1:100], ]
hyp_par[order(res_regr_aggr[, 3], decreasing = F)[1:10], ]
hyp_par[order(res_regr_aggr[, 4], decreasing = F)[1:10], ]
hyp_par[order(res_regr_aggr[, 5], decreasing = F)[1:10], ]
hyp_par[order(res_regr_aggr[, 6], decreasing = F)[1:10], ]

pdf("randomForest_regr.pdf",width=13,height=9.5)
Visualize_results(hyp_par[1:1920], data.table(res_regr_aggr[1:1920,]), param_randomForest)
dev.off()
pdf("ranger_regr.pdf",width=13,height=9.5)
Visualize_results(hyp_par[1921:3840], data.table(res_regr_aggr[1921:3840,]), param_ranger)
dev.off()
pdf("randomForestSRC_regr.pdf",width=13,height=9.5)
Visualize_results(hyp_par_rf = hyp_par[3841:5760], data.table(res_regr_aggr[3841:5760,]), param = param_randomForestSRC)
dev.off()

# Ähnliche Ergebnisse wie bei Klassifikation
# nodesize sehr klein (bei ranger nicht unbedingt minimal) setzen
# maxnodes möglichst groß: 1
# nodedepth möglichst groß: 1
# mtry ca 0.8
# sample.fraction bei allen relativ groß setzen (ca.0.75) (Unterschied zur Regression)
# splitrule: normal
# ranger besser als randomForest besser als randomForestSRC
# allerbesten Ergebnisse allerdings von randomForest!


# Function for pushing several ggplots in one graphic
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
 #  If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# specific dataset analysis
param = param_randomForest 
j = "nodesize"

for (i in unique(hyp_par$problem)) { # i = 457
  print(i)
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
  
  par(mfrow = c(2, 3))
  plots = list()
  for(k in colnames(res_classif_i_rf)[2:c(ncol(res_classif_i_rf)-1)]) {
    #print(k)
    data = cbind(res_classif_i_rf[, k, with = F], hyp_par_i_rf[,  unlist(param), with = F])
    data$replace = as.factor(data$replace)
    data$ntree = as.numeric(data$ntree)
    task = makeRegrTask(id = "rf", data = data, target = k)
    lrn = makeLearner("regr.randomForest", par.vals = list(ntree = 1000)) #, predict.type = "se")
    model = train(lrn, task)
    pd = generatePartialPredictionData(model, task, j, gridsize = 20)
    pd$data
    plots[[k]] = plotPartialPrediction(pd)
    # KI's machen wenig sinn, da hier von anderen Effekten überlagert!! (siehe ntree)
    #pd = generatePartialPredictionData(model, task, c("ntree"), fun = function(x) quantile(x, c(.25, .5, .75)))
    #plotPartialPrediction(pd)
    
    #pd = generatePartialPredictionData(model, task, c("sampsize", "replace"), interaction = TRUE)
    #print(plotPartialPrediction(pd, facet = "replace"))
  }
  
  pdf(paste0(j, "/classif/", i),width=13,height=9.5)
  multiplot(plots[["acc"]],plots[["ber"]], plots[["mmce"]], plots[["multiclass.au1u"]], plots[["multiclass.brier"]], plots[["logloss"]], cols = 2)
  dev.off()
}


