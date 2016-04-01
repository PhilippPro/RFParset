library(mlrMBO)

learner_km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
