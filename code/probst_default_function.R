setwd("/nfsmb/koll/probst/Random_Forest/RFParset/results/")
load("/home/probst/Random_Forest/RFParset/results/clas.RData")
load("/home/probst/Random_Forest/RFParset/results/reg.RData")
tasks = rbind(clas_small, reg_small)
tasks = tasks[, c("task_id", "task_type", "did", "name", "NumberOfInstances", "NumberOfFeatures", "NumberOfClasses",        
                  "MajorityClassSize", "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")]
tasks$task_type = substr(tasks$task_type, 12, 1000)
library(data.table)
tasks = data.table(tasks)
tasks = tasks[!(tasks$did %in% c(1054, 1071, 1065))]
tasks$did = 1:298
tasks[, task_id := NULL]
tasks[, name := NULL]
#keys = c("task_type", "NumberOfInstances", "NumberOfFeatures")
#setkeyv(tasks, keys)


load("/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")

hyp_par = hyp_par[1:5760, 15:27, with = F]
hyp_par$hyp_id = 1:5760

res_classif[,timetrain := NULL]
res_classif$did = ceiling(res_classif$job.id / 5760)
res_classif$hyp_id = res_classif$job.id%%5760
res_classif[hyp_id ==0]$hyp_id = 5760

res_regr[,timetrain := NULL]
res_regr$did = ceiling(res_regr$job.id / 5760)
res_regr$hyp_id = res_regr$job.id%%5760
res_regr[hyp_id ==0]$hyp_id = 5760

save(tasks, hyp_par, res_classif, res_regr, file = "results_compressed.RData")
