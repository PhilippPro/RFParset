library(cranlogs)
library(data.table)
library(xtable)

downloads = cran_downloads(packages = c("randomForest", "party", "randomForestSRC", "ranger", "Rborist", "bigrf"), when = c("last-month"))
downloads = data.table(downloads)
downloads[,sum(count), by = "package"]
xtable(downloads[,sum(count), by = "package"], digits=0)
