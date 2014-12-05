# start <- proc.time()
# 
# ## Java stuff
# Sys.setenv(JAVA_HOME="")
# options(java.parameters="-Xmx14g")
# 
# ## load RWeka, tm libs
# library(RWeka)
# library(tm)
# 
# ## create a tokenizer for up to 2-Grams
# tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))                                                      
# en_texts <- VCorpus(DirSource(directory="2_train/", encoding="UTF-8"))
# 
# ## create the TermDocumentMatrix
# tdm <- TermDocumentMatrix(en_texts, control=list(tokenizer=tokenizer))
# 
# ## show the running time
# proc.time() - start


## special treatment, 1-Grams, problem with the RWeka above, it doesn't return the 1-letter and 2-letter words.
create1grams <- function(fileInput) {
  ## get the lines from the file. 
  input <- file(fileInput, "rb", encoding = "UTF-8")
  allLines <- readLines(input, skipNul = TRUE, encoding = "UTF-8")
  allWords <- unlist(strsplit(allLines, split = " "))
  close(input)
  
  t <- table(allWords)
  
  return(t)
}

##Rprof(filename = "out.txt", interval = 0.005)
##v <- create1grams("test.txt")
##Rprof(NULL)
