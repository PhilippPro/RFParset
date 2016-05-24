library(mlr)
library(xtable)
xtable1 = rbind(
  c("ntree", lower = 50, upper = 100), 
  c("replace", NA, NA),
  c("sampsize", lower = 0, upper = 1),
  c("mtry", lower = 0, upper = 1),
  c("nodesize", lower = 0, upper = 0.5),
  c("maxnodes", lower = 0, upper = 1)
)
xtable1
xtable(xtable1)
xtable2 = rbind(
  c("ntree", lower = 50, upper = 100), 
  c("samptype", lower = NA, upper = NA), # entspricht replace
  c("sampsize", lower = 0, upper = 1), 
  c("mtry", lower = 0, upper = 1),
  c("nodesize", lower = 0, upper = 0.5),
  c("nodedepth", lower = 0, upper = 1),
  c("splitrule", lower = NA, upper = NA)
)
xtable2
xtable(xtable2)

xtable3 = rbind(
  c("num.trees", lower = 50, upper = 100), 
  c("replace", NA, NA),
  c("sample.fraction", lower = 0, upper = 1),
  c("mtry", lower = 0, upper = 1),
  c("min.node.size", lower = 0, upper = 0.5)
)
xtable3
xtable(xtable3)
