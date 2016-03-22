library("BatchExperiments")
dir = "/home/probst/Random_Forest/RFParset"
#dir = "/home/philipp/Promotion/RandomForest/RFParset/results"
setwd(paste(dir,"/results", sep = ""))
load(paste(dir,"/results/clas.RData", sep = ""))
load(paste(dir,"/results/reg.RData", sep = ""))

setConfig(conf = list(cluster.functions = makeClusterFunctionsMulticore(2)))

tasks = rbind(clas_small, reg_small)
regis = makeExperimentRegistry(id = "par_randomForest_random", packages=c("OpenML", "mlr", "randomForest", "ranger"), 
                               work.dir = paste(dir,"/results", sep = ""), src.dirs = paste(dir,"/functions", sep = ""), seed = 1)

# Add problem
gettask = function(static, idi, rel.nodesize = 0.0000001, rel.mtry = NULL, sample.fraction = ifelse(replace, 1, 0.632), 
                   replace = TRUE, respect.unordered.factors = FALSE) {
  task = getOMLTask(task.id = idi, verbosity=0)$input$data.set
  
  list(idi = idi, data = task$data, formula = as.formula(paste(task$target.features,"~.") ), 
       target = task$target.features,
       min.node.size = ceiling(rel.nodesize*nrow(task$data)),
       rel.nodesize = rel.nodesize,
       mtry = ceiling(rel.mtry * (ncol(task$data)-1)), 
       rel.mtry = rel.mtry,
       sample.fraction = sample.fraction,
       replace = replace, 
       respect.unordered.factors = respect.unordered.factors)
}
addProblem(regis, id = "taski", static = tasks, dynamic = gettask, seed = 123)

# Add algorithm
forest.wrapper.parset = function(static, dynamic, ...) {
  if(static[static$task_id == dynamic$idi, 2] == "Supervised Classification") {
    dynamic$data[,dynamic$target] = droplevels(as.factor(dynamic$data[,dynamic$target]))
    pred = ranger(formula = dynamic$formula, data = dynamic$data, min.node.size = dynamic$min.node.size, 
                  mtry = dynamic$mtry, sample.fraction = dynamic$sample.fraction, 
                  replace = dynamic$replace, probability = TRUE, 
                  respect.unordered.factors = dynamic$respect.unordered.factors, ... )$predictions
    pred2 = factor(colnames(pred)[max.col(pred)], levels = colnames(pred))
    conf.matrix = getConfMatrix2(dynamic, pred2, relative = TRUE)
    k = nrow(conf.matrix)
    AUC = -1
    AUCtry = try(multiclass.auc2(pred, dynamic$data[,dynamic$target]))
    if(is.numeric(AUCtry))
      AUC = AUCtry
    measures = c(measureACC(dynamic$data[,dynamic$target], pred2), mean(conf.matrix[-k, k]), 
                 measureMMCE(dynamic$data[,dynamic$target], pred2), AUC)
    names(measures) = c("ACC", "BER", "MMCE", "multi.AUC")
  } else {
    pred = ranger(formula = dynamic$formula, data = dynamic$data, min.node.size = dynamic$min.node.size, 
                  mtry = dynamic$mtry, sample.fraction = dynamic$sample.fraction, 
                  replace = dynamic$replace, probability = TRUE, 
                  respect.unordered.factors = dynamic$respect.unordered.factors, ... )$predictions
    measures = c(measureMAE(dynamic$data[,dynamic$target] , pred),  measureMEDAE(dynamic$data[,dynamic$target], pred), 
                 measureMEDSE(dynamic$data[,dynamic$target], pred), measureMSE(dynamic$data[,dynamic$target], pred))
    names(measures) = c("MAE", "MEDAE", "MEDSE", "MSE")
  }
  list(measures = measures, datainfo = c(static[static$task_id == dynamic$idi, c(1, 2, 15, 13, 14, 18, 19)]), 
       algoinfo = list(nodesize = dynamic$min.node.size, rel.nodesize = dynamic$rel.nodesize, mtry = dynamic$mtry, 
                       rel.mtry = dynamic$rel.mtry, sample.fraction = dynamic$sample.fraction, replace = dynamic$replace, 
                       respect.unordered.factors = dynamic$respect.unordered.factors))
}
addAlgorithm(regis, id = "forest.parset", fun = forest.wrapper.parset, overwrite = TRUE)

# Dataframe with random parameter settings, that should be tested

# restricted
p = 10
ps = makeParamSet(
  makeNumericParam("rel.nodesize", lower = 0.0000001 / (10*4), upper = 1/4),
  makeNumericParam("rel.mtry", lower = 0.0000001 , upper = 1)
)
#generateGridDesign(ps, resolution = c(x1 = 4, x2 = 5), trafo = TRUE)
n = 10
restr.design = generateRandomDesign(n = n, ps)
restr.design = data.frame(idi = rep(tasks$task_id, each = n), restr.design[rep(1:n, nrow(tasks)) ,])

# exhaustive
p = 10
ps = makeParamSet(
  makeNumericParam("rel.nodesize", lower = 0.0000001 / (10*4), upper = 1/4),
  makeNumericParam("rel.mtry", lower = 0.00000001, upper = 1),
  makeNumericParam("sample.fraction", lower = 0.000001, upper = 1),
  makeLogicalParam("replace"),
  makeLogicalParam("respect.unordered.factors")
)
n = 10
exhau.design = generateRandomDesign(n = n, ps)
exhau.design = data.frame(idi = rep(tasks$task_id, each = n), exhau.design[rep(1:n, nrow(tasks)) ,])

task.design1 = makeDesign("taski", design = restr.design)
task.design2 = makeDesign("taski", design = exhau.design)
pars = list(num.trees = 10000)
forest.design.parset = makeDesign("forest.parset", exhaustive = pars)

addExperiments(regis, repls = 10, prob.designs = task.design1, algo.designs = list(forest.design.parset)) # 
addExperiments(regis, repls = 10, prob.designs = task.design2, algo.designs = list(forest.design.parset)) # 

summarizeExperiments(regis)
testJob(regis)


