## script-ish code, for TDM manipulation, and after...


## Java stuff
Sys.setenv(JAVA_HOME="")
options(java.parameters="-Xmx14g")

## load RWeka, tm libs
library(RWeka)
library(tm)

## load a previously saved TermDocumentMatrix
load("5.tdm")
tdmMatrix <- as.matrix(tdm)

## get the sums over rows
tf5grams <- rowSums(tdmMatrix)
tf5sorted <- sort(tf5grams, decreasing = TRUE)

## save the vector, has as element names the N-grams (.tf stands for term frequencies)
save(tf5grams, file="5Grams.tf")
save(tf5sorted, file="5Sorted.tf")




## Calculate ALL probabilities, truncate after (or not at all)

## 1 GRAMS
load("3_grams//1sorted.tf") ## the creates a tf1sorted vector
indexes1Gram <- 1:length(tf1sorted)
names(indexes1Gram) <- names(tf1sorted)
save(indexes1Gram, file="5_models/indexes1Gram.tf")
freq1Gram <- unname(tf1sorted)
save(freq1Gram, file="5_models/freq1Gram.tf")
remove(tf1sorted)

## calculate probabilities
sum1Gram <- sum(freq1Gram)
prob1Gram <- freq1Gram / sum1Gram
save(prob1Gram, file = "5_models/prob1Gram.tf")

## put all together in a data.table
dt1Gram <- data.table(grams = names(indexes1Gram), 
                       index = indexes1Gram, 
                       frequency = freq1Gram, 
                       probabilities = prob1Gram)
## most of the searches and operations will be done on priors
setkey(dt1Gram, index)
save(dt1Gram, file="5_models/dt1Gram.dt")


## 2 GRAMS
load("3_grams//2sorted.tf") ## the creates a tf2sorted vector
len2 <- length(tf2sorted)
indexes2Gram <- 1:len2
names(indexes2Gram) <- names(tf2sorted)
save(indexes2Gram, file="5_models/indexes2Gram.tf")

## save frequencies
freq2Gram <- unname(tf2sorted)
save(freq2Gram, file="5_models/freq2Gram.tf")
allNames2 <- names(tf2sorted)

## get the post, prior
allNamesCollapsed2 <- paste(allNames2, collapse = " ")
allNamesVector2 <- unlist(strsplit(allNamesCollapsed2, split = " "))
prior2 <- allNamesVector2[seq.int(1, len2 * 2, 2)]
post2 <- allNamesVector2[seq.int(2, len2 * 2, 2)]
prior2Ix <- indexes1Gram[prior2]
post2Ix <- indexes1Gram[post2]
save(prior2Ix, file="5_models/prior2Gram.tf")
save(post2Ix, file="5_models/posterior2Gram.tf")

## calculate probabilities
start <- proc.time()
prob2Gram <- vector(mode="double", length = length(freq2Gram))
for(i in 1:length(freq2Gram)){
  if(i %% 100000 == 0) {
    print(proc.time() - start)
    print(paste("index: ", i))
  }
  prob2Gram[i] <- (freq2Gram[i] / freq1Gram[prior2Ix[i]])
}
print(proc.time() - start)
save(prob2Gram, file="5_models/prob2Gram.tf")

## put all together in a data.table
dt2Gram <- data.table(grams = names(indexes2Gram), 
                       index = indexes2Gram, 
                       frequency = freq2Gram, 
                       prior = prior2Ix, 
                       posterior = post2Ix, 
                       probabilities = prob2Gram)
## most of the searches and operations will be done on priors
save(dt2Gram, file="5_models/dt2Gram.dt")

## cleanup
remove(tf2sorted)
remove(allNames2)
remove(allNamesCollapsed2)
remove(allNamesVector2)
remove(prior2)
remove(post2)
remove(len2)



## 3 GRAMS
load("3_grams//3sorted.tf") ## the creates a tf3sorted vector
len3 <- length(tf3sorted)
indexes3Gram <- 1:len3
names(indexes3Gram) <- names(tf3sorted)
save(indexes3Gram, file="5_models/indexes3Gram.tf")

## save frequencies
freq3Gram <- unname(tf3sorted)
save(freq3Gram, file="5_models/freq3Gram.tf")
allNames3 <- names(tf3sorted)

## get the post, prior
allNamesCollapsed3 <- paste(allNames3, collapse = " ")
allNamesVector3 <- unlist(strsplit(allNamesCollapsed3, split = " "))
prior3_1 <- allNamesVector3[seq.int(1, len3 * 3, 3)]
prior3_2 <- allNamesVector3[seq.int(2, len3 * 3, 3)]
post3 <- allNamesVector3[seq.int(3, len3 * 3, 3)]
prior3Ix <- indexes2Gram[paste(prior3_1, prior3_2, sep=" ")]
post3Ix <- indexes1Gram[post3]
save(prior3Ix, file="5_models/prior3Gram.tf")
save(post3Ix, file="5_models/posterior3Gram.tf")

## calculate probabilities
start <- proc.time()
prob3Gram <- vector(mode="double", length = length(freq3Gram))
for(i in 1:length(freq3Gram)){
  if(i %% 100000 == 0) {
    print(proc.time() - start)
    print(paste("index: ", i))
  }
  prob3Gram[i] <- (freq3Gram[i] / freq2Gram[prior3Ix[i]])
}
print(proc.time() - start)
save(prob3Gram, file="5_models/prob3Gram.tf")

## put all together in a data.table
dt3Gram <- data.table(grams = names(indexes3Gram), 
                       index = indexes3Gram, 
                       frequency = freq3Gram, 
                       prior = prior3Ix, 
                       posterior = post3Ix, 
                       probabilities = prob3Gram)
## most of the searches and operations will be done on priors
save(dt3Gram, file="5_models/dt3Gram.dt")

## cleanup
remove(tf3sorted)
remove(allNames3)
remove(allNamesCollapsed3)
remove(allNamesVector3)
remove(prior3)
remove(post3)
remove(len3)




## 4 GRAMS
load("3_grams//4sorted.tf") ## the creates a tf4sorted vector
len4 <- length(tf4sorted)
indexes4Gram <- 1:len4
names(indexes4Gram) <- names(tf4sorted)
save(indexes4Gram, file="5_models/indexes4Gram.tf")

## save frequencies
freq4Gram <- unname(tf4sorted)
save(freq4Gram, file="5_models/freq4Gram.tf")
allNames4 <- names(tf4sorted)

## get the post, prior
allNamesCollapsed4 <- paste(allNames4, collapse = " ")
allNamesVector4 <- unlist(strsplit(allNamesCollapsed4, split = " "))
prior4_1 <- allNamesVector4[seq.int(1, len4 * 4, 4)]
prior4_2 <- allNamesVector4[seq.int(2, len4 * 4, 4)]
prior4_3 <- allNamesVector4[seq.int(3, len4 * 4, 4)]
post4 <- allNamesVector4[seq.int(4, len4 * 4, 4)]
prior4Ix <- indexes3Gram[paste(prior4_1, prior4_2, prior4_3, sep=" ")]
post4Ix <- indexes1Gram[post4]
save(prior4Ix, file="5_models/prior4Gram.tf")
save(post4Ix, file="5_models/posterior4Gram.tf")

## calculate probabilities
start <- proc.time()
prob4Gram <- vector(mode="double", length = length(freq4Gram))
for(i in 1:length(freq4Gram)){
  if(i %% 100000 == 0) {
    print(proc.time() - start)
    print(paste("index: ", i))
  }
  prob4Gram[i] <- (freq4Gram[i] / freq3Gram[prior4Ix[i]])
}
print(proc.time() - start)
save(prob4Gram, file="5_models/prob4Gram.tf")

## put all together in a data.table
dt4Gram <- data.table(grams = names(indexes4Gram), 
                       index = indexes4Gram, 
                       frequency = freq4Gram, 
                       prior = prior4Ix, 
                       posterior = post4Ix, 
                       probabilities = prob4Gram)
## most of the searches and operations will be done on priors
save(dt4Gram, file="5_models/dt4Gram.dt")


## cleanup
remove(tf4sorted)
remove(allNames4)
remove(allNamesCollapsed4)
remove(allNamesVector4)
remove(prior4)
remove(post4)
remove(len4)




## 5 GRAMS
load("3_grams//5sorted.tf") ## the creates a tf5sorted vector
## tf5sorted <- head(tf5sorted, 1000000) ## truncate, too many
len5 <- length(tf5sorted)
indexes5Gram <- 1:len5
names(indexes5Gram) <- names(tf5sorted)
save(indexes5Gram, file="5_models/indexes5GramNO.tf")

## save frequencies
freq5Gram <- unname(tf5sorted)
save(freq5Gram, file="5_models/freq5GramNO.tf")
allNames5 <- names(tf5sorted)

## get the post, prior
allNamesCollapsed5 <- paste(allNames5, collapse = " ")
allNamesVector5 <- unlist(strsplit(allNamesCollapsed5, split = " "))
prior5_1 <- allNamesVector5[seq.int(1, len5 * 5, 5)]
prior5_2 <- allNamesVector5[seq.int(2, len5 * 5, 5)]
prior5_3 <- allNamesVector5[seq.int(3, len5 * 5, 5)]
prior5_4 <- allNamesVector5[seq.int(4, len5 * 5, 5)]
post5 <- allNamesVector5[seq.int(5, len5 * 5, 5)]

prior5Ix <- indexes4Gram[paste(prior5_1, prior5_2, prior5_3, prior5_4, sep=" ")]
post5Ix <- indexes1Gram[post5]
save(prior5Ix, file="5_models/prior5GramNO.tf")
save(post5Ix, file="5_models/posterior5GramNO.tf")

## calculate probabilities
start <- proc.time()
prob5Gram <- vector(mode="double", length = length(freq5Gram))
for(i in 1:length(freq5Gram)){
  if(i %% 100000 == 0) {
    print(proc.time() - start)
    print(paste("index: ", i))
  }
  prob5Gram[i] <- (freq5Gram[i] / freq4Gram[prior5Ix[i]])
}
print(proc.time() - start)
save(prob5Gram, file="5_models/prob5GramNO.tf")

## put all together in a data.table
dt5Gram <- data.table(grams = names(indexes5Gram), 
                       index = indexes5Gram, 
                       frequency = freq5Gram, 
                       prior = prior5Ix, 
                       posterior = post5Ix, 
                       probabilities = prob5Gram)
## most of the searches and operations will be done on priors
save(dt5Gram, file="5_models/dt5GramNO1.dt")



## cleanup
remove(tf5sorted)
remove(allNames5)
remove(allNamesCollapsed5)
remove(allNamesVector5)
remove(prior5_1)
remove(prior5_2)
remove(prior5_3)
remove(prior5_4)
remove(post5)
remove(len5)






## prepare the sorted subsets of high prob prior vs. 3 choices


dt1GramFinalTriv3Choices <- dt1GramFinalTriv3Choices[order(-probabilities)]
dt2GramFinalTriv3Choices <- dt2GramFinalTriv3Choices[order(prior, -probabilities)]
dt3GramFinalTriv3Choices <- dt3GramFinalTriv3Choices[order(prior, -probabilities)]
dt4GramFinalTriv3Choices <- dt4GramFinalTriv3Choices[order(prior, -probabilities)]
dt5GramFinalTriv3Choices <- dt5GramFinalTriv3Choices[order(prior, -probabilities)]

## keep only 3 posterior choices for each prior, the ones with the higher probability
dt1GramFinalTriv3Choices <- head(dt1GramNO, 4)
save(dt1GramFinalTriv3Choices, file="6_models/dt1GramFinalTriv3Choices.dt")

## 2-Grams
dt2Priors <- dt2GramNO$prior
dt2Keep <- cmpMarkFirstN(dt2Priors, 3)  ## cmpMarkFirstN is a compiled function
dt2GramNO$keep <- dt2Keep
dt2GramFinalTriv3Choices <- dt2GramNO[keep == TRUE]
dt2GramFinalTriv3Choices$keep <- NULL
save(dt2GramFinalTriv3Choices, file="6_models/dt2GramFinalTriv3Choices.dt")
remove(dt2Keep)
remove(dt2Priors)
head(dt2GramFinalTriv3Choices)
tail(dt2GramFinalTriv3Choices)


## 3-Grams
dt3Priors <- dt3GramNO$prior
dt3Keep <- cmpMarkFirstN(dt3Priors, 3)  ## cmpMarkFirstN is a compiled function
dt3GramNO$keep <- dt3Keep
dt3GramFinalTriv3Choices <- dt3GramNO[keep == TRUE]
dt3GramFinalTriv3Choices$keep <- NULL
save(dt3GramFinalTriv3Choices, file="6_models/dt3GramFinalTriv3Choices.dt")
remove(dt3Keep)
remove(dt3Priors)
head(dt3GramFinalTriv3Choices)
tail(dt3GramFinalTriv3Choices)


## 4-Grams
dt4Priors <- dt4GramNO$prior
dt4Keep <- cmpMarkFirstN(dt4Priors, 3)  ## cmpMarkFirstN is a compiled function
dt4GramNO$keep <- dt4Keep
dt4GramFinalTriv3Choices <- dt4GramNO[keep == TRUE]
dt4GramFinalTriv3Choices$keep <- NULL
save(dt4GramFinalTriv3Choices, file="6_models/dt4GramFinalTriv3Choices.dt")
remove(dt4Keep)
remove(dt4Priors)
head(dt4GramFinalTriv3Choices)
tail(dt4GramFinalTriv3Choices)


## 5-Grams
dt5Priors <- dt5GramNO$prior
dt5Keep <- cmpMarkFirstN(dt5Priors, 3)  ## cmpMarkFirstN is a compiled function
dt5GramNO$keep <- dt5Keep
dt5GramFinalTriv3Choices <- dt5GramNO[keep == TRUE]
dt5GramFinalTriv3Choices$keep <- NULL
save(dt5GramFinalTriv3Choices, file="6_models/dt5GramFinalTriv3Choices.dt")
remove(dt5Keep)
remove(dt5Priors)
head(dt5GramFinalTriv3Choices)
tail(dt5GramFinalTriv3Choices)


