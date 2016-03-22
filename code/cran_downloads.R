library(cranlogs)
library(data.table)
library(xtable)

downloads = cran_downloads(packages = c("randomForest", "xgboost", "randomForestSRC", "ranger", "Rborist"), when = c("last-month"))
downloads = data.table(downloads)
downloads[,sum(count), by = "package"]
xtable(downloads[,sum(count), by = "package"], digits=0)

downloads = cran_downloads(packages = c("h2o","ParallelForest", "bigrf"), when = c("last-month"))
downloads = data.table(downloads)
downloads[,sum(count), by = "package"]
xtable(downloads[,sum(count), by = "package"], digits=0)

downloads = cran_downloads(packages = c("rpart", "RRF", "randomForestSRCSyn", "obliqueRF", "rotationForest", 
                                        "rFerns", "randomUniformForest", "wsrf", "roughrf", "trimTrees", "extraTrees", "party" ), when = c("last-month"))
downloads = data.table(downloads)
downloads = downloads[,sum(count), by = "package"]
downloads[order(downloads$V1, decreasing = T),]
xtable(downloads[,sum(count), by = "package"], digits=0)
