library(cranlogs)
library(data.table)
library(knitr)

downloads = cran_downloads(packages = c("randomForest", "xgboost", "randomForestSRC", "ranger", "Rborist"), when = c("last-month"))
downloads = data.table(downloads)
downloads = downloads[,sum(count), by = "package"]
colnames(downloads) = c("**package**", "**RStudio downloads in the last month**")
kable(downloads, format = "markdown")
barplot(downloads[,2], names.arg = downloads$package, col = "blue")

downloads = cran_downloads(packages = c("h2o","ParallelForest", "bigrf"), when = c("last-month"))
downloads = data.table(downloads)
downloads = downloads[,sum(count), by = "package"]
colnames(downloads) = c("**package**", "**RStudio downloads in the last month**")
kable(downloads, format = "markdown")

downloads = cran_downloads(packages = c("rpart", "RRF", "randomForestSRCSyn", "obliqueRF", "rotationForest", 
                                        "rFerns", "randomUniformForest", "wsrf", "roughrf", "trimTrees", "extraTrees", "party" ), when = c("last-month"))
downloads = data.table(downloads)
downloads = downloads[,sum(count), by = "package"]
downloads = downloads[order(downloads$V1, decreasing = T),]
colnames(downloads) = c("**package**", "**RStudio downloads in the last month**")
kable(downloads, format = "markdown")
