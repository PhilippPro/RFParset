regis = loadRegistry("/nfsmb/koll/probst/Random_Forest/RFParset/results/probs-muell")

ids_ok = c(1:9000)[-getErrorMessages()$job.id]
res = reduceResultsDataTable(ids = ids_ok, fun = function(r) as.data.frame(as.list(r)), reg = regis)
res

getJobTable()$ntree
min(getJobStatus()$started)
max(getJobStatus()$done)

res = reduceResultsDataTable(ids = 1141:1806, fun = function(r) as.data.frame(as.list(r)), reg = regis)
res

