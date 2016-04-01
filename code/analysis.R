library("BatchExperiments")
dir = "/home/probst/Random_Forest/RFParset"
regis = loadRegistry("/home/probst/Random_Forest/RFParset/results/par_randomForest_ntree_grid-files")
load(paste(dir,"/results/clas.RData", sep = ""))
load(paste(dir,"/results/reg.RData", sep = ""))
tasks = rbind(clas_small, reg_small)

# ntree
ids = findExperiments(reg = regis, ids = findDone(regis), algo.pattern = "forest.ntree", prob.pars = (idi %in% clas_small$task_id))
res = loadResults(regis, ids)
# unlist(sapply(res, "[[", 2)[1,]); take always 100 consecutive results and aggregate them
pdf(paste(dir,"/results/graphics/clas_ntree.pdf", sep = ""), width = 6, height = 6)
par(mfrow = c(3,3))
for (i in 1:190){
 print(i)
 upper = apply(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1), 1, function(x) quantile(x,0.95))
 lower = apply(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1), 1, function(x) quantile(x,0.05))
 meani = rowMeans(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1))
 plot(meani, type = "l", main = i, xlab = "ntree", ylab = "oob mmce", ylim = range(c(upper[100:10000], lower[100:10000], meani)))
 lines(1:10000, upper, col = "red")
 lines(1:10000, lower, col = "blue")
}
dev.off()

ids = findExperiments(reg = regis, ids = findDone(regis), algo.pattern = "forest.ntree", prob.pars = (idi %in% reg_small$task_id))
res = loadResults(regis, ids)
pdf(paste(dir,"/results/graphics/reg_ntree_2000.pdf", sep = ""),width=6,height=6)
par(mfrow = c(3,3))
for (i in 1:111){
  print(i)
  upper = apply(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1), 1, function(x) quantile(x,0.95))
  lower = apply(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1), 1, function(x) quantile(x,0.05))
  meani = rowMeans(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1))
  plot(meani[1:2000], type = "l", main = i, xlab = "ntree", ylab = "oob mse", ylim = range(c(upper[100:2000], lower[100:2000], meani[1:2000])))
  lines(1:2000, upper[1:2000], col = "red")
  lines(1:2000, lower[1:2000], col = "blue")
}
dev.off()

pdf(paste(dir,"/results/graphics/reg_ntree.pdf", sep = ""),width=6,height=6)
par(mfrow = c(3,3))
for (i in 1:111){
  print(i)
  upper = apply(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1), 1, function(x) quantile(x,0.95))
  lower = apply(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1), 1, function(x) quantile(x,0.05))
  meani = rowMeans(sapply(res[c(100*(i-1)+1):c(100*i)], "[[", 1))
  plot(meani, type = "l", main = i, xlab = "ntree", ylab = "oob mse", ylim = range(c(upper[100:10000], lower[100:10000], meani[1:10000])))
  lines(1:10000, upper[1:10000], col = "red")
  lines(1:10000, lower[1:10000], col = "blue")
}
dev.off()

# mtry - nodesize
ids = findExperiments(reg = regis, algo.pattern = "forest.parset", prob.pars = ((idi %in% clas_small$task_id) ))
res = loadResults(regis, ids)

# Transform to matrix

res_mat <- matrix(sapply(res, "[[", 1)[4,] , 12, 13, byrow = TRUE)
colnames(res_mat) = c(-5, -1, 0.0000001, seq(1/40, 1/4, length.out = 10))
rownames(res_mat) = c(-1, 0.0000001, seq(1/10, 1, length.out = 10))
res_mat

#sapply(res, "[[", 1)
#sapply(res, "[[", 4)[2,]
namen = sapply(res, "[[", 3)[1,]
res_mat = list()
for(i in 1: nrow(clas_small)){
  res_mat[[i]] <- matrix(sapply(res[which(namen == clas_small$task_id[i])], "[[", 1)[3,] , 12, 13, byrow = TRUE)
  colnames(res_mat[[i]]) = c(-5, -1, 0.0000001, seq(1/40, 1/4, length.out = 10))
  rownames(res_mat[[i]]) = c("sqrt(p)", 0.0000001, seq(1/10, 1, length.out = 10))
}

res[which(namen == clas_small$task_id[3])]
res_mat[[3]]



# Make heatmap tables
library(reshape2)
library(ggplot2)

res_mat_ges = Reduce('+', res_mat)/length(res_mat)

scheee = melt(res_mat_ges)
ggplot(scheee, aes(as.factor(Var1), as.factor(Var2), group=Var2)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(fill = scheee$value, label = round(scheee$value, 3))) +
  #scale_fill_gradient(low = "red", high = "blue") +
  ylab("relative maximum nodesize") +
  xlab("relative mtry") +
  ggtitle("Heatmap of multiclass.AUC values of grid design of relative nodesize and relative mtry")
