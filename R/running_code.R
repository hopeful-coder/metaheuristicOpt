rangeVar = matrix(c(-3,3), nrow = 2)
cs = CS(costFunction3, optimType = 'MIN', numVar = 3, rangeVar = rangeVar, numPopulation = 100)
pso = PSO(costFunction3, optimType = 'MIN', numVar = 3, rangeVar = rangeVar, numPopulation = 100)
costFunctionD(cs)

detach("package:metaheuristicOpt", unload = TRUE)
install.packages('C:/Users/Admin/Desktop/Github/metaOpt', repos = NULL, type = 'source')
library(metaheuristicOpt)
set.seed(1)
PSO(costFunctionA, optimType = 'MIN', numVar = nrow(dataStudy), rangeVar = rangeV)

