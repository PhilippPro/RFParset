measureMulticlassBrier = function(probabilities, truth) {
  mean(rowSums((probabilities - model.matrix( ~ . -1, data = as.data.frame(truth)))^2))
}

measureLogloss = function(probabilities, truth){
   eps = 1e-15;
   #let's confine the predicted probabilities to [eps,1-eps], so logLoss doesn't reach infinity under any circumstance
   probabilities[probabilities>1-eps]=1-eps
   probabilities[probabilities<eps]=eps
   truth.model = model.matrix(~.-1, data = as.data.frame(truth))
   -1*mean(log(probabilities[(truth.model-probabilities)>0]))
}

getConfMatrix2 = function(dynamic, pred, relative = TRUE) {
  cls = levels(dynamic$data[,dynamic$target])
  k = length(cls)
  truth = dynamic$data[,dynamic$target]
  tab = table(truth, pred)
  mt = tab * (matrix(1, ncol = k, nrow = k) - diag(1, k, k))
  rowsum = rowSums(mt)
  colsum = colSums(mt)
  result = rbind(cbind(tab, rowsum), c(colsum, sum(colsum)))
  dimnames(result) = list(true = c(cls, "-SUM-"), predicted = c(cls, "-SUM-"))
  if (relative) {
    total = sum(result[1:k, 1:k])
    k1 = k + 1
    if (result[k1, k1] != 0) {
      result[k1, 1:k] = result[k1, 1:k] / result[k1, k1] 
    } else {
      result[k1, 1:k] = 0 
      }
    rownorm = function(r, len) {
      if (any(r[1:len] > 0)) {
        r / sum(r[1:len])
      } else {
        rep(0, len + 1)
      }
      }
    result[1:k, ] = t(apply(result[1:k, ], 1, rownorm, len = k))
    result[k1, k1] = result[k1, k1] / total
  }
  return(result)
}

multiclass.au1u = function(probabilities, truth) {
  m = colAUC(probabilities, truth)
  if (all(dim(m) == c(1,1))) {
    c(max(m, 1-m))
  } else {
    c = c(combn(1:nlevels(truth), 2))
    mean(m[cbind(rep(1:nrow(m), each = 2), c)])
  }
}
