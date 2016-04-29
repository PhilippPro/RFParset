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

multiclass.auc2 = function(pred, resp){
  predP = pred
  # choose the probablity of the choosen response
  predV = vnapply(seq_row(pred), function(i) {
    pred[i, resp[i]]
  })
  auc = pROC::multiclass.roc(response = resp, predictor = predV)$auc
  as.numeric(auc)
}