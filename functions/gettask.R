# Add problem
gettask = function(static, idi, rel.mtry = "sqrt(p)", rel.nodesize = "one" , sample.fraction = 0.632, 
                   replace = FALSE, respect.unordered.factors = FALSE, rel.maxnodes = NULL, bootstrap = NULL, 
                   rel.nodedepth = NULL, splitrule = NULL) {
  task = getOMLTask(task.id = idi, verbosity=0)$input$data.set
  n = nrow(task$data)
  p = ncol(task$data) - 1
  
  # mtry
  if (rel.mtry == "log(p)"){
    mtry = floor(log(p))
  } else {
    if (rel.mtry == "sqrt(p)") {
      mtry = floor(sqrt(p))
    } else {
      if (rel.mtry == "one") {
        mtry = 1
      } else {
        mtry = ceiling(as.numeric(as.character(rel.mtry)) * (p))
      }
    }
  }
  
  # min.node.size
  if (rel.nodesize == "log(n)"){
    min.node.size = floor(log(n))
  } else {
    if (rel.nodesize == "sqrt(n)") {
      min.node.size = floor(sqrt(n - 1))
    } else {
      if (rel.nodesize == "one") {
        min.node.size = 1
      } else {
        if (rel.nodesize == "five") {
          min.node.size = 5
        } else {
          min.node.size = ceiling(as.numeric(as.character(rel.nodesize))*n)
        }
      }
    }
  }
  
  # sampsize
  sample.fraction = as.numeric(as.character(sample.fraction))
  sampsize = max(floor(sample.fraction * n), 1)
  
  # maxnodes
  if(!is.null(rel.maxnodes)) {
    maxnodes = ceiling(as.numeric(as.character(rel.maxnodes)) * n)
  } else {
    maxnodes = NULL
  }
  
  # bootstrap
  if(is.null(bootstrap)) {
    bootstrap = "by.root"
  } else {
    bootstrap = as.character(bootstrap)
  }
  # nodedepth
  if(!is.null(rel.nodedepth)) {
    nodedepth = ceiling(as.numeric(as.character(rel.nodedepth)) * n)
  } else {
    nodedepth = NULL
  }
  
  # splitrule
  if (!is.null(splitrule)){
    if (is.numeric(task$target.features)){
      if (splitrule == "normal"){
        splitrule = "mse"
      } else {
        if (splitrule == "unwt") {
          splitrule = "mse.unwt"
        } else {
          splitrule = "mse.hvwt"
        }
      }
    } else {
      if (splitrule == "normal"){
        splitrule = "gini"
      } else {
        if (splitrule == "unwt") {
          splitrule = "gini.unwt"
        } else {
          splitrule = "gini.hvwt"
        }
      }
    }
  }
  
  list(idi = idi, data = task$data, formula = as.formula(paste(task$target.features,"~.") ), 
       target = task$target.features,
       mtry = mtry, 
       rel.mtry = rel.mtry,
       min.node.size = min.node.size,
       rel.nodesize = rel.nodesize,
       sampsize = sampsize,
       sample.fraction = sample.fraction,
       replace = replace, 
       respect.unordered.factors = respect.unordered.factors,
       maxnodes = maxnodes,
       rel.maxnodes = rel.maxnodes, 
       bootstrap = bootstrap, 
       nodedepth = nodedepth, 
       rel.nodedepth = rel.nodedepth,
       splitrule = splitrule
  )
}

gettaskMBO = function(static, idi, learner, rel.mtry = "sqrt(p)", rel.nodesize = "one" , sample.fraction = 1, 
                   replace = TRUE, respect.unordered.factors = FALSE, rel.maxnodes = NULL, bootstrap = NULL, 
                   rel.nodedepth = NULL, splitrule = NULL) {
  task = getOMLTask(task.id = idi, verbosity=0)$input$data.set
  list(idi = idi, data = task$data, formula = as.formula(paste(task$target.features,"~.") ), 
       target = task$target.features, learner = learner)
}