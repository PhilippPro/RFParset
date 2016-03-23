library("BatchExperiments")
dir = "/home/probst/Random_Forest/RFParset"
regis = loadRegistry("/home/probst/Random_Forest/RFParset/results/par_randomForest-files")
load(paste(dir,"/results/clas.RData", sep = ""))
load(paste(dir,"/results/reg.RData", sep = ""))
tasks = rbind(clas_small, reg_small)


ids = findExperiments(reg = regis, ids = findDone(regis), algo.pattern = "forest.ntree", prob.pars = (idi %in% clas_small$task_id))
res = loadResults(regis, ids)
# unlist(sapply(res, "[[", 2)[1,]); take always 30 consecutive results and aggregate them
for (i in 151:190)
plot(rowMeans(sapply(res[c(30*(i-1)+1):c(30*i)], "[[", 1)), type = "l", main = i)


ids = findExperiments(reg = regis, ids = findDone(regis), algo.pattern = "forest.parset", prob.pars = (idi %in% clas_small$task_id))
res = loadResults(regis, ids)
for (i in 1:50)
  plot(rowMeans(sapply(res[c(10*(i-1)+1):c(10*i)], "[[", 1)), type = "l", main = i)


ids = findExperiments(reg = regis, ids = findDone(regis), algo.pattern = "forest.ntree", prob.pars = (idi %in% reg_small$task_id))
res = loadResults(regis, ids)
# unlist(sapply(res, "[[", 2)[1,]); take always 30 consecutive results and aggregate them
for (i in 1:50)
  plot(rowMeans(sapply(res[c(30*(i-1)+1):c(30*i)], "[[", 1)), type = "l", main = i)

res[[1]]
res$da
?loadResult
