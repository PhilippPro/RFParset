library(BatchExperiments)
dir = "/home/probst/Random_Forest/RFParset"
#dir = "/home/philipp/Promotion/RandomForest/RFParset/results"
setwd(paste0(dir,"/results"))
load(paste0(dir,"/results/clas.RData"))
load(paste0(dir,"/results/reg.RData"))

setConfig(conf = list(cluster.functions = makeClusterFunctionsMulticore(2)))

tasks = rbind(clas_small, reg_small)
regis = makeExperimentRegistry(id = "par_randomForest_mlrMBO", 
                               packages=c("OpenML", "mlr", "randomForest", "ranger", "randomForestSRC", "mlrMBO"), 
                               work.dir = paste(dir,"/results", sep = ""), src.dirs = paste(dir,"/functions", sep = ""), 
                               seed = 1)

source(paste0(dir, "/functions/gettask.R"))
addProblem(regis, id = "taski", static = tasks, dynamic = gettaskMBO, seed = 123, overwrite = TRUE)

source(paste0(dir, "/functions/mlrMBO.R"))
addAlgorithm(regis, id = "ranger.mbo", fun = forest.wrapper.mbo, overwrite = TRUE)

prob.design = data.frame(idi = rep(tasks$task_id, 2), learner = rep(c("randomForest", "km"), each = nrow(tasks)))
prob.design = makeDesign("taski", design = prob.design)


addExperiments(regis, prob.designs = prob.design, algo.designs = "ranger.mbo")
summarizeExperiments(regis)
testJob(regis)
