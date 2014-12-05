# in short, the story is: 
# I want to predict the "next" word a user will type. For this: 
# 1. I'll analyze a "big" body of text, in English - a book from Gutenberg, on gardening :)
# 2. I'll extract the list of words, for each will list the words which most often follow (5 
# words, decreasing prob)
# 3. When a user types a word and space, I'll check and present the list of 5 possible words. User can click on
# one of those. 
# 4. Every time a user clicks on one of the words, I'll increase the "accuracy" counter. 

# 1. prepare the data, get the book
#book <- "http://stat.ethz.ch/R-manual/R-devel/library/base/html/readLines.html"
#book <- "http://www.gutenberg.org/files/46052/46052-h/46052-h.htm"
#download.file(url=book, destfile="book.txt", method="curl", cacheOK=TRUE)
#lines <- readLines("book.txt")
lines <- c("line one", "line two", "gogo line two", "momo two", "momo one", "momo 3", "line momo two three")

# some initializations
hashmap <- list()
prev <- "xoxo"
hashmap[[prev]] <- list()


# 2. analyze the text, build the hashmap of words
for (i in 1:length(lines)){
    line <- lines[i]
    words <- unlist(strsplit(line, split=" "))
    for (j in 1:length(words)){
        word <- words[j]
        #print(c("word: ", word))
        # cleanup the word, replace special characters
        word <- gsub("[<>,.()!?/\\:]", "", word)
        word <- tolower(word)
        
        if (! (word %in% names(hashmap))){
            hashmap[[word]] <- list()
            #print(c("not in hashmap, add: ", word))
        }
        else{
            #print(c("word", word, " is already in hashmap"))
        }
        
        # we find the current word in the previous's list?
        #print(c("hashmap[", prev, "]=", hashmap[[prev]]))
        #print(c("hashmap[", prev, "][", word, "]=", hashmap[[prev]][[word]]))
        if (! is.null(hashmap[[prev]][[word]])){
            counter <- hashmap[[prev]][[word]]
            hashmap[[prev]][[word]] <- counter + 1
        }
        # current word is not in the previous's list, so add it
        else {
            hashmap[[prev]][[word]] <- 1
        }

        prev <- word
    }
}

# re-order the hashmap by the freq/prob of the "following" words
nn <- names(hashmap)
for (i in 1:length(nn)) {
    name <- nn[i]
    print (name)
    elem <- hashmap[[name]]
    if (length(elem) > 0) {
        reord <- elem[order(unlist(elem), decreasing=T)]
        hashmap[[name]] <- reord
    }
}
#print(lines)