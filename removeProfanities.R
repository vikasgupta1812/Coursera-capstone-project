## The list with profanities is taken from 
## https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en. 
## Is a simplified version, will match tokens exactly. Will split lines using the long-ish set 
## of characters in the regex and will replace, if needed, profanities with (...). 
## Will also write only lower case in the "clean" files. 

## Java stuff
Sys.setenv(JAVA_HOME="")
options(java.parameters="-Xmx14g")

## load RWeka, tm libs
library(RWeka)
library(tm)
library(stringi)

## list of profanities
profanities <- c("acrotomophilia", "anal", "anilingus", "arsehole", "ass", "asshole", "assmunch", 
                 "bangbros", "bastinado", "bbw", "beastiality", "beaver", "bitch", "bj", "blowjob", "blumpkin", 
                 "bollocks", "boner", "bukkake", "bulldyke", "bunghole", "butthole", 
                 "camgirl", "camslut", "camwhore", "carpetmuncher", "circlejerk", "clusterfuck", "cock", "cocks", 
                 "coprolagnia", "coprophilia", "cornhole", "cum", "cumming", "cummin", "cunt", 
                 "darkie", "deepthroat", "dick", "dong", "fagot", "figging", "fisting", "footjob", "frotting", 
                 "fuck", "fucking", "fuckin", "fucker", "f*ck", "f*cking", "f*ckin", "futa", "futanari", 
                 "goatcx", "goatse", "gokkun", "goodpoop", "guro", 
                 "handjob", "hardcore", "honkey", "homo", "hooker", "humping", 
                 "jigaboo", "jiggaboo", "jiggerboo", "jizz", "kike", "kinbaku", "kinkster", 
                 "milf", "muff", "nazi", "negro", "nigga", "nigger", 
                 "omoroashi", "paedophile", "paedo", "pedo", "pedophile", "pegging", "pisspig", "ponyplay", 
                 "poof", "porn", "porno", "pornography", "pornstar", "pussy", 
                 "queaf", "raghead", "rape", "raping", "rapist", "rectum", "rimjob", "rimming", 
                 "scat", "schlong", "scissoring", "semen", "sex", "sexo", "sexy", 
                 "shemale", "shibari", "shit", "shota", "slanteye", "slut", "smut", "sodomize", "sodomizing", 
                 "sodomy", "spooge", "strapon", "strapado", "suck", "sucks", "swastika", 
                 "tits", "titt", "titties", "topless", "tosser", "towelhead", "tranny", "tubgirl", 
                 "tush", "tushy", "twat", "upskirt", "urethra", "urophilia", "vagina", "vulva", 
                 "wank", "wetback", "xx", "xxx", "yaoi", "yiffy", "zoophilia"
                 )


## function, will remove profanities from a line and will return the line with no punctuation, 
## uses Alphabetic tokenizer from RWeka
removeProfanities <- function(line) {
  ## leave the # and ' alone
  ##  tokens <- strsplit(line, "[~`!@$%^&*\\(\\)\\-_=+,<.>/?\\|\\{} \"]")
  tokens <- AlphabeticTokenizer(line)
  newline <- ""
  for (token in unlist(tokens)){
    if (token %in% profanities){
      newline <- paste(newline, "(...)")
    }
    else 
      newline <- paste(newline, stri_trans_tolower(token), sep=" ")
  }
    
  return(newline)
}


## function will parse a file and will write clean lines of that file in other file. 
## e.g. A call like cleanFile("meh.txt", "clean") will create the folder "clean" and inside
## a files - clean_meh.txt etc. This should not contain profanities (as defined above). 

## NOTE - please careful with the param, the function doesn't check them at all. 
cleanFile <- function(fileInput, folderOutput) {
  if (!file.exists(folderOutput)){
    ## create the folder if it doesn't exist
    dir.create(folderOutput)
  }
    
  ## get the lines from the file. 
  input <- file(fileInput, "rb", encoding = "UTF-8")
  lines <- readLines(input, skipNul = TRUE, encoding = "UTF-8")
  close(input)
  
  ## open a file for writing
  output <- file(paste(folderOutput, "/clean_", fileInput, sep=""), "w", encoding = "UTF-8")
  
  for (line in lines){
    ## readlines, clean them, write them in the new file. 
    cleanLine <- removeProfanities(line)
    writeLines(text = cleanLine, con = output)
  }
  
  close(output)
    
}