library(batchtools)
library(mlr)

setwd("/nfsmb/koll/probst/Random_Forest/RFParset/results/")
regis = loadRegistry("probs-muell")

min(getJobStatus()$started, na.rm = T)
max(getJobStatus()$done, na.rm = T)
# 2 Tage hat es gebraucht auf dem lokalen Rechner (12 Kerne)

error_ids = getErrorMessages()$job.id # errors nicht laden
ids_ok = c(1:1716480)[-error_ids]
ids_classif = ids_ok[ids_ok %in% 1:1077120]
ids_regr = ids_ok[ids_ok %in% 1077121:1716480]

res_classif = reduceResultsDataTable(ids = ids_classif, fun = function(r) as.data.frame(as.list(r)), reg = regis, fill = TRUE)
right = which(is.na(res_classif$medse)) # delete regressions that were accidentally made
res_classif = res_classif[right]
wrong_ids = ids_classif[!(ids_classif %in% res_classif$job.id)] # save ids of these jobs
res_classif = res_classif[, 1:8, with = FALSE]

reduceResultsDataTable(ids = 1073592, fun = function(r) as.data.frame(as.list(r)), reg = regis, fill = TRUE)
res_regr = reduceResultsDataTable(ids = ids_regr, fun = function(r) as.data.frame(as.list(r)), reg = regis, fill = TRUE)

hyp_par = getJobTable()

save(res_classif, res_regr, error_ids, wrong_ids, hyp_par, file = "/nfsmb/koll/probst/Random_Forest/RFParset/results/results.RData")
