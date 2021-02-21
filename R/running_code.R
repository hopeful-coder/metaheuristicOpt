setwd('C:/Users/Admin/Desktop/Github/amg')
# dataStudy <- read.csv(file='A-example-chapter-16-3.csv') ## CoefVar = 1.2 - rather medium 
# dataStudy1 = read.csv('new1.csv')
# dataStudy2 = read.csv('new2.csv')

datasets = c('A-example-chapter-16-3.csv', 'A-example-chapter-16-2.csv', 'A-example-chapter-16.csv', 'A-example-chapter-20.csv', 'new1.csv','new2.csv')
# for(i in 5:length(datasets)){
i = 1
dataStudy = read.csv(datasets[i])
if(i %in% c(5,6)){
  dataStudy = dataStudy[,-c(13,14,17,20)]
}
# dataStudy = ds
## new functions for mean country costs for pts enrolled

source("functions-V5B-short.R")

source("functions-mean-country-costs.R")

## additional functions to check: 

source("functions-for-output.R") 

# dataStudy=read.csv(file='C:/Users/User/Documents/R/Amgen/OptCostV1/Osteo-Study-cost.csv')
#dataStudy=read.csv(file='C:/Users/User/Documents/R/Amgen/OptCostV2-small/Osteo-short-cost-10.csv')

## data preparation for cost function

source("module-data-preparation-country-costs-V1-f_Oct18-19.R")

## dimension
5*prod(vecUpp-vecLow+1)

##  8.534938e+12

# 5*1.42249e+12 =  7.11245e+12


## for 16 countries
#AvcostPts <- c(15962, 13246, 13242, 14064, 13089, 19537, 13260, 14651, 12312, 13802, 14629, 16589, 13397, 10776, 12056, 14823)

## Ex1
## AvcostPts <- c(15600, 14250, 13550, 14200, 13900, 14300, 13400, 14250, 12300, 13800, 14600, 16380, 13400, 11200, 14000, 14100)

######
## Ex2-best
AvcostPts <- c(15600, 14250, 13550, 14200, 13800, 14300, 13400, 14250, 12300, 13800, 14600, 16380, 13400, 11200, 14000, 14100)
AvcostPts = runif(nrow(dataStudy),11000, 16000 )
# AvcostPts <- c(15600, 14250, 13550, 14200, 13800, 14700, 13400, 14250, 12300, 13800, 14600, 16380, 13400, 11200, 14000, 14100)

#AvcostPts <- c(15900, 14250, 13500, 14060, 13090, 19000, 13260, 14650, 12300, 13800, 14600, 16580, 13400, 11200, 14000, 14200)

### Creating input cost to run in optimization -- Cost2

## Calculate country costs using planned duration of enrollment TT and costs per patients enrolled during TT

## Input cost for optimization 

# Avcost <- FAggregCostsMeanPtsCountryTT(AvcostPts,TT,vecCountries,vecN,vecSiteRate,vecCvar,Mattimes)

###############################  IF THERE ARE COSTS PER EACH SITE INITIATION IN EACH COUNTRY

vecSitesCost <-  rep(5000,nrow(dataStudy)) #rep(5000,20)
## we should use 

Avcost <- FAggregCostsMeanPtsCountryTT(AvcostPts,TT,vecCountries,vecN,vecSiteRate,vecCvar,Mattimes) + vecSitesCost

## > Avcost
# [1] 136309.08 127801.64  64742.42 161520.74  87970.02 168354.74  61395.56  76396.30  44440.82  57547.68  57667.93 208529.07
# [13] 125847.64  56625.79  89172.48 115205.83


## 

## use for speed only
#Cost2 <- rep(1,length(Cost1))

### RUN file
## this module was updated - Oct 01, 2019
## Vlad - added 3 rows - 
## for each allocation: PoS - actual, PoS using normal approx., and directly calculated costs for mean # of patients enrolled
## this is only for testing the versions 


source("module-chapter.R")
filename = paste0('simplex',nrow(dataStudy),'.csv')
write.csv(OptMat3, filename, row.names = F)

algos = c('PSO','ALO','GWO','DA','FFA','GA','GOA','HS','MFO','SCA','WOA','CLONALG','DE')

costFunctionA = function(input){
  h = dataStudy
  sitesLow = h$SitesLow
  sitesUpp = h$SitesUpp
  cur_sites = input
  obj1 = Avcost * cur_sites
  vecmeantimes = vecmeantimes
  vecb = vecSiteRate * vecmeantimes
  vecG = vecG
  vecx = cur_sites
  vecy = vecN - sitesLow
  zq = qnorm(.7)
  
  #constraints
  constraints = c(1:3)
  oj = sum(obj1)
  penalty = 0
  constraints[1] = any(vecx > sitesUpp)
  constraints[2] = any(vecx < sitesLow)
  constraints[3] = nn - sum(vecb*cur_sites) + zq*sqrt(sum(cur_sites*vecG)) > 0
  for(i in 1:3){
    penalty = sum(penalty, constraints[i])
  }
  return(oj + penalty * 10000000)
}


start = proc.time()

library(pso)
library(metaheuristicOpt)

# c = psoptim(rep(NA,nrow(dataStudy)), fn = costFunctionA, lower = dataStudy$SitesLow, upper = dataStudy$SitesUpp,
#             control=list(trace.stats = T, maxit = 1000, s = 40))
# c

###################################################################
#Whale optimization
#
###################################################################
rVar = matrix(t(data.frame(a = dataStudy$SitesLow,b = dataStudy$SitesUpp)), nrow = 2)
rangeV = rVar
PSO(costFunctionA, optimType = 'MIN', numVar = nrow(dataStudy), rangeVar = rangeV)


detach("package:metaheuristicOpt", unload = TRUE)
install.packages('C:/Users/Admin/Desktop/Github/metaOpt', repos = NULL, type = 'source')
library(metaheuristicOpt)
set.seed(1)
PSO(costFunctionA, optimType = 'MIN', numVar = nrow(dataStudy), rangeVar = rangeV)

