#' @export multiclass.brier
#' @rdname measures
#' @format none
multiclass.brier = makeMeasure(id = "multiclass.brier", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Multiclass Brier score",
  fun = function(task, model, pred, feats, extra.args) {
  measureMulticlassBrier(getPredictionProbabilities(pred), pred$data$truth)
  }
)

#' @export measureMulticlassBrier
#' @rdname measures
#' @format none
measureMulticlassBrier = function(probabilities, truth) {
  mean(rowSums((probabilities - model.matrix( ~ . -1, data = as.data.frame(truth)))^2))
}


data(iris)
iris = cbind(iris, sample(as.factor(c("c","a")), nrow(iris), replace = TRUE))
colnames(iris)[6] = "bin"
train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris.train <- iris[train.idx, ]
iris.test <- iris[-train.idx, ]
rg.iris <- ranger(bin ~ ., data = iris.train, write.forest = TRUE, probability = TRUE)$predictions
measureMulticlassBrier(probabilities = rg.iris, truth = iris.train$bin)

pred.iris <- predict(rg.iris, dat = iris.test)
