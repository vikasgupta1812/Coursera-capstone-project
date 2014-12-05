## break the files in smaller chunks, easier to process by the NLP libs

## 

tokenize <- function(file) {
  tokens <- c()
  # open file
  lines <- readLines(file)
  # parse lines
  for (i in 1:length(lines)) {
    line <- lines[i]
    tokens <- c(tokens, WordTokenizer(line))
  }
  return(tokens)
}