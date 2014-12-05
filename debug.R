## all sorts of one-off functions, for tests and so. 


## debug - something is wrong with the 1-grams: 
## no "a", "i", and so on...

findWords <- function(v, w) {
  
  l <- nchar(w)
  names <- names(v)
  r <- vector()
  
  for (word in names){
    if (substr(word, 1, l) == w){
      r <- append(r, word)
    }
  }
  
  return(r)
}


## tests with Appends - which one is faster. 
## preallocated vector. 

## append list call
testFastAppend <- function() {
  start <- proc.time()
  j <- list()
  for (i in 1:100000)
    j <- append(j, 1000)
  
  print(proc.time() - start)
}

## c() list
testFastAppend2 <- function() {
  start <- proc.time()
  j <- list()
  for (i in 1:100000)
    j <- c(j, 1000)
  
  print(proc.time() - start)
}

## 
testFastAppend3 <- function() {
  start <- proc.time()
  j <- vector()
  for (i in 1:100000)
    j <- append(j, 1000)
  
  print(proc.time() - start)
}

## append vector
testFastAppend4 <- function() {
  start <- proc.time()
  j <- vector()
  for (i in 1:100000)
    j <- c(j, 1000)
  
  print(proc.time() - start)
}

## some funky environment manipulation
push <- function(l, x) {
  assign(l, append(eval(as.name(l)), x), envir=parent.frame())
}

## plus test
l <- list()
testFastAppend5 <- function(lst) {
  start <- proc.time()
  for (i in 1:100000)
    push(lst, 1000)
  
  print(proc.time() - start)
}

## direct access to elements in list
testFastAppend6 <- function() {
  start <- proc.time()
  j <- list()
  for (i in 1:100000)
    j[[i]] <- 1000
  
  print(proc.time() - start)
}

## preallocate vector, direct addressing after that
testFastAppend7 <- function() {
  start <- proc.time()
  j <- as.integer(vector(mode = "integer", length = 100000))
  lengths  <- 0
  for (i in 1:100000){
    j[[lengths+1]] <- 1000
    lengths <- lengths + 1
  }
  print(proc.time() - start)
  return(j)
}
