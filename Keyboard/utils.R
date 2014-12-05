## Keyboard related functions

allLoaded <- FALSE

## loadModels. The shiny app comes with a bunch of saved data.tables. These need to be loaded
loadModelTrivial <- function() {
  
  load("dt1GramFinalTriv3Choices.dt", .GlobalEnv)
  load("dt2GramFinalTriv3Choices.dt", .GlobalEnv)
  load("dt3GramFinalTriv3Choices.dt", .GlobalEnv)
  load("dt4GramFinalTriv3Choices.dt", .GlobalEnv)
  load("dt5GramFinalTriv3Choices.dt", .GlobalEnv)
  allLoaded <- TRUE
}


# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


## ----------------- MAIN FUNCTIONs ------------------------------


getNGramProposals <- function(gram, n, datatableN, datatableNMinus1 = NULL, dictionary, special, level = 1) {
  
  proposeN <- list(words = c(), probs = c(), ngrams = c(), left = n)
  
  ## special is a request to return all 1-Grams (first 3)
  if (special) {
    dt <- head(dictionary, n)
    len <- length(dt$grams)
    if (len == 0)
      return(proposeN)
    
    for (i in 1:len){
      proposeN$words <- c(proposeN$words, dt[i]$grams)
      proposeN$probs <- c(proposeN$probs, strtrim(paste("P=", dt[i]$probabilities, sep=""), 9))
      proposeN$ngrams <- c(proposeN$ngrams, paste(1, "-Gram", sep=""))
    }
    
    proposeN$left <- n - len
    return(proposeN)
  }
  
  ## not that special
  
  ## look for the given gram in the N-1 Gram table
  dt1Minus1 <- datatableNMinus1[grams == gram]
  lenMinus1 <- length(dt1Minus1)
  ## the gram is not in the N-1 Gram data table
  if(lenMinus1 == 0) return(proposeN)
  
  ## if it is
  priorIx <- dt1Minus1$index
  dt <- head(datatableN[prior == priorIx], n)
  len <- length(dt$grams)
  
  if (len == 0)
    return(proposeN)
  
  for (i in 1:len){
    ## find the posterior, return
    dict <- dictionary[index == dt[i]$posterior]
    if (length(dict$grams) == 1) 
      post <- dict[1]$grams
    else 
      post <- "not found"
    proposeN$words <- c(proposeN$words, post)
    proposeN$probs <- c(proposeN$probs, strtrim(paste("P=", dt[i]$probabilities, sep=""), 9))
    proposeN$ngrams <- c(proposeN$ngrams, paste(level, "-Gram", sep=""))
  }
  
  proposeN$left <- n - len
  return(proposeN)
}

## merge two similar list, used underneath
mergeProposals <- function(proposal1, proposal2, n) {
  propose3 <- list(words = c(), probs = c(), ngrams = c(), left = 0)
  propose3$words <- c(proposal1$words, proposal2$words)
  propose3$probs <- c(proposal1$probs, proposal2$probs)
  propose3$ngrams <- c(proposal1$ngrams, proposal2$ngrams)
  propose3$left <- n
  return(propose3)
}


## propose 3 options, takes a text and returns a list with the following structure: 
## "words" <- list(3 options)
## "probabilities" <- list(3 numbers)
## "ngrams" <- list(which ngrams did come from)
propose3Options <- function(wordList, 
                            dt5 = dt5GramFinalTriv3Choices, 
                            dt4 = dt4GramFinalTriv3Choices, 
                            dt3 = dt3GramFinalTriv3Choices, 
                            dt2 = dt2GramFinalTriv3Choices, 
                            dt1 = dt1GramFinalTriv3Choices) {
  propose3 <- list(words = c(), probs = c(), ngrams = c())
  
  ## start checking last 4
  last4 <- tail(wordList, 4)
  switch(as.character(length(last4)),
         
         "0"={
           ## empty, so return only the 1-Gram proposal
           propose3 <- getNGramProposals("", 3, dt1, NULL, dt1, TRUE, 1)
           return(propose3)
         },
         
         
         "1"={
           ## 1 word, so start looking for the 2-Gram proposals
           ## search in 2-Grams
           last1Gram <- tail(last4, 1)
           propose3_2 <- getNGramProposals(last1Gram, 3, dt2, dt1, dt1, FALSE, 2)
           if (propose3_2$left == 0)
             return(propose3_2)
           
           ## continue looking at the 1-Grams
           propose3_1 <- getNGramProposals("", propose3_2$left, dt1, NULL, dt1, TRUE, 1)
           
           ## merge the two, return result
           propose3 <- mergeProposals(propose3_2, propose3_1, propose3_1$left)
           return(propose3)
         },
         
         
         "2"={
           ## 2 words, so start looking for the 3-Gram proposals
           ## search in 3-Grams
           last2Gram <- paste(tail(last4, 2), collapse = " ")
           propose3_3 <- getNGramProposals(last2Gram, 3, dt3, dt2, dt1, FALSE, 3)
           if (propose3_3$left == 0)
             return(propose3_3)
           
           ## search in 2-Grams
           last1Gram <- tail(last4, 1)
           propose3_2 <- getNGramProposals(last1Gram, propose3_3$left, dt2, dt1, dt1, FALSE, 2)
           propose3 <- mergeProposals(propose3_3, propose3_2, propose3_2$left)
           if (propose3$left == 0)
             return(propose3)
           
           ## continue looking at the 2-Grams
           propose3_1 <- getNGramProposals("", propose3_2$left, dt1, NULL, dt1, TRUE, 1)
           
           ## merge the two, return result
           propose3 <- mergeProposals(propose3, propose3_1, propose3_1$left)
           return(propose3)
         },
         
         
         "3"={
           ## 3 words, so start looking for the 4-Gram proposals
           ## search in 4-Grams
           last3Gram <- paste(tail(last4, 3), collapse=" ")
           propose3_4 <- getNGramProposals(last3Gram, 3, dt4, dt3, dt1, FALSE, 4)
           propose3 <- mergeProposals(propose3, propose3_4, propose3_4$left)
           if (propose3$left == 0)
             return(propose3)
           
           ## 2 words, so start looking for the 3-Gram proposals
           ## search in 3-Grams
           last2Gram <- paste(tail(last4, 2), collapse=" ")
           propose3_3 <- getNGramProposals(last2Gram, propose3$left, dt3, dt2, dt1, FALSE, 3)
           propose3 <- mergeProposals(propose3, propose3_3, propose3_3$left)
           if (propose3$left == 0)
             return(propose3)
           
           ## search in 2-Grams
           last1Gram <- tail(last4, 1)
           propose3_2 <- getNGramProposals(last1Gram, propose3$left, dt2, dt1, dt1, FALSE, 2)
           propose3 <- mergeProposals(propose3, propose3_2, propose3_2$left)
           if (propose3$left == 0)
             return(propose3)
           
           ## continue looking at the 2-Grams
           propose3_1 <- getNGramProposals("", propose3$left, dt1, NULL, dt1, TRUE, 1)
           propose3 <- mergeProposals(propose3, propose3_1, propose3_1$left)
           return(propose3)
         },
         
         
         "4"={
           
           ## 4 words, so start looking for the 5-Gram proposals
           ## search in 5-Grams
           last4Gram <- paste(tail(last4, 4), collapse=" ")
           propose3_5 <- getNGramProposals(last4Gram, 3, dt5, dt4, dt1, FALSE, 5)
           propose3 <- mergeProposals(propose3, propose3_5, propose3_5$left)
           if (propose3$left == 0)
             return(propose3)
           
           ## 3 words, so start looking for the 4-Gram proposals
           ## search in 4-Grams
           last3Gram <- paste(tail(last4, 3), collapse=" ")
           propose3_4 <- getNGramProposals(last3Gram, propose3$left, dt4, dt3, dt1, FALSE, 4)
           propose3 <- mergeProposals(propose3, propose3_4, propose3_4$left)
           if (propose3$left == 0)
             return(propose3)
           
           ## 2 words, so start looking for the 3-Gram proposals
           ## search in 3-Grams
           last2Gram <- paste(tail(last4, 2), collapse=" ")
           propose3_3 <- getNGramProposals(last2Gram, propose3$left, dt3, dt2, dt1, FALSE, 3)
           propose3 <- mergeProposals(propose3, propose3_3, propose3_3$left)
           if (propose3$left == 0)
             return(propose3)
           
           ## search in 2-Grams
           last1Gram <- tail(last4, 1)
           propose3_2 <- getNGramProposals(last1Gram, propose3$left, dt2, dt1, dt1, FALSE, 2)
           propose3 <- mergeProposals(propose3, propose3_2, propose3_2$left)
           if (propose3$left == 0)
             return(propose3)
           
           ## continue looking at the 2-Grams
           propose3_1 <- getNGramProposals("", propose3$left, dt1, NULL, dt1, TRUE, 1)
           propose3 <- mergeProposals(propose3, propose3_1, propose3_1$left)
           return(propose3)
           
         },
{
  ## default   
  ## empty, so return only the 1-Gram proposal
  propose3 <- getNGramProposals("", 3, dt1, NULL, TRUE, 1)
}

  )
return(propose3)
}


