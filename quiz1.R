# functions used for the first quiz, Capstone project


# return the longest line in a file
longestLine <- function (file) {
    
    # initialize some length
    longest <- -1
    
    # open file
    lines <- readLines(file)
    # parse lines
    for (i in 1:length(lines)) {
        # count chars/line, save in a var
        line <- lines[i]
        if(nchar(line)>longest)
            longest <- nchar(line)
    }
    
    # return the result
    return(longest)    
}


# return the number of occurences of a word, in a file. 
numberOccurences <- function(file, word) {
    # initialize some length
    occurences <- 0
    lenword <- nchar(word)  # no need to check for 0, whatev'
    
    # open file
    lines <- readLines(file)
    # parse lines
    for (i in 1:length(lines)) {
        # count chars/line, save in a var
        line <- lines[i]
        
        # replace the word with "", count the length before and after
        lenbefore <- nchar(line)
        lineafter <- gsub(word, "", line)
        lenafter <- nchar(lineafter)
        
        occurences <- occurences + (lenbefore - lenafter)/lenword
    }
    
    return(occurences)
}


# return a line that matches something
matchWords <- function(file, sentence) {
    # open file
    lines <- readLines(file)
    # parse lines
    for (i in 1:length(lines)) {
        line <- lines[i]
        found <- grepl(x=line, pattern=sentence)
        if (found==TRUE)
            return(line)
    }
}


# return the number of occurences of a sentence, in a file. 
numberTweets <- function(file, sentence) {
    # initialize some length
    occurences <- 0
    
    # open file
    lines <- readLines(file)
    # parse lines
    for (i in 1:length(lines)) {
        line <- lines[i]
        if (grepl(x=line, pattern=sentence))
            occurences <- occurences + 1
    }
    
    return(occurences)
}


# 1. 200
# 2. > 2mil
# 3. > 40000
# 4. 4
# 5. study exam
# 6. 3


