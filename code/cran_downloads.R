library(cranlogs)
library(data.table)
downloads = cran_downloads(packages = c("randomForest", "party", "randomForestSRC", "ranger", "Rborist", "bigrf"), when = c("last-month"))
downloads = data.table(downloads)
downloads[,sum(count), by = "package"]
