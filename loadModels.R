## One-off functions, load the already created files. 
## The point here is that they are big enough, don't want to recreate them too 
## often (tens of millions records). 
loadAllModelsNO <- function(){
  
  load("5_models/freq1GramNO.tf")
  load("5_models/freq2GramNO.tf")
  load("5_models/freq3GramNO.tf")
  load("5_models/freq4GramNO.tf")
  load("5_models/freq5GramNO.tf")
  
  load("5_models/indexes1GramNO.tf")
  load("5_models/indexes2GramNO.tf")
  load("5_models/indexes3GramNO.tf")
  load("5_models/indexes4GramNO.tf")
  load("5_models/indexes5GramNO.tf")
  
  load("5_models/posterior2GramNO.tf")
  load("5_models/posterior3GramNO.tf")
  load("5_models/posterior4GramNO.tf")
  load("5_models/posterior5GramNO.tf")

  load("5_models/prior2GramNO.tf")
  load("5_models/prior3GramNO.tf")
  load("5_models/prior4GramNO.tf")
  load("5_models/prior5GramNO.tf")
  
  load("5_models/prob1GramNO.tf")
  load("5_models/prob2GramSimple.tf")
  load("5_models/prob3GramSimple.tf")
  load("5_models/prob4GramSimple.tf")
  load("5_models/prob5GramSimple.tf")
  
}


## load the original models
loadAllModels <- function(){
  
  load("5_models/freq1Gram.tf")
  load("5_models/freq2Gram.tf")
  load("5_models/freq3Gram.tf")
  load("5_models/freq4Gram.tf")
  load("5_models/freq5Gram.tf")
  
  load("5_models/indexes1Gram.tf")
  load("5_models/indexes2Gram.tf")
  load("5_models/indexes3Gram.tf")
  load("5_models/indexes4Gram.tf")
  load("5_models/indexes5Gram.tf")
  
  load("5_models/posterior2Gram.tf")
  load("5_models/posterior3Gram.tf")
  load("5_models/posterior4Gram.tf")
  load("5_models/posterior5Gram.tf")
  
  load("5_models/prior2Gram.tf")
  load("5_models/prior3Gram.tf")
  load("5_models/prior4Gram.tf")
  load("5_models/prior5Gram.tf")
  
  load("5_models/prob1Gram.tf")
  load("5_models/prob2Gram.tf")
  load("5_models/prob3Gram.tf")
  load("5_models/prob4Gram.tf")
  load("5_models/prob5Gram.tf")
  
}


dt1GramNO <- dt1GramNO[order(-probabilities)]
dt2GramNO <- dt2GramNO[order(prior, -probabilities)]
dt3GramNO <- dt3GramNO[order(prior, -probabilities)]
dt4GramNO <- dt4GramNO[order(prior, -probabilities)]
dt5GramNO <- dt5GramNO[order(prior, -probabilities)]

head(dt1GramNO)
tail(dt1GramNO)
head(dt2GramNO)
tail(dt2GramNO)
head(dt3GramNO)
tail(dt3GramNO)
head(dt4GramNO)
tail(dt4GramNO)
head(dt5GramNO)
tail(dt5GramNO)

