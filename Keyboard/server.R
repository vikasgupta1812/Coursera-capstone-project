library(shiny)
library(data.table)

print("Loading utils.R...")
source(file = "utils.R")
print("...done.")
print("Loading data...")
loadModelTrivial()
print("...done.")


## -------------------------------------- shiny -----------------------------
## the reactive part
shinyServer(func = function(input, output, clientData, session) {

        ## SESSION SCOPED VARIABLES 

        ## current options on the buttons
        currentOptions <- c()
        currentOption <- ""
        
        currentLog <- ""
        
        ## save the last text in the input, use it as flag (at least)
        lastText <- ""
        
        ## hits/totalWords will give the accuracy
        hits <- 0
        totalWords <- 0
        accuracy <- 0
        
        ## ----------------  REACTIVE CODE --------------------------
        
        
        ## the suggestions for the next words
        output$captureWriting <- renderText({
            inputText <- input$inputText
            
            ## return if nothing in the box
            if (inputText=="") return("Input text rendered...")
            if (inputText == lastText) return("Input text rendered...")

            ## save in variable
            lastText <<- inputText
            
            ## find the best 3 options
            ## parse the text, extract the last 4 (to propose a 5'th)
            wordList <- strsplit(gsub("[^[:alnum:] ]", "", tolower(inputText)), " +")[[1]]
            
            ## save the length
            totalWords <- length(wordList)

            ## GET THE PROPOSED OPTIONS
            proposedOptions <- propose3Options(wordList)
            if(length(proposedOptions$words) != 3)
              return
            
            ## format the answers
            if (proposedOptions$words[1] == "i")  proposedOptions$words[1] <- "I"
            if (proposedOptions$words[2] == "i")  proposedOptions$words[2] <- "I"
            if (proposedOptions$words[3] == "i")  proposedOptions$words[3] <- "I"
            
            currentOptions <<- c(proposedOptions$words[1], proposedOptions$words[2], proposedOptions$words[3])

            ## format log messages
            message1 <- paste(proposedOptions$words[1], ", probability: ", proposedOptions$probs[1], ", found in ", proposedOptions$ngrams[1])
            message2 <- paste(proposedOptions$words[2], ", probability: ", proposedOptions$probs[2], ", found in ", proposedOptions$ngrams[2])
            message3 <- paste(proposedOptions$words[3], ", probability: ", proposedOptions$probs[3], ", found in ", proposedOptions$ngrams[3])
            
            if (totalWords > 0)
              accuracy <<- hits/totalWords
            else 
              accuracy <<- 0
            
            ## write in the log
            currentLog <<- paste("Accuracy: ", strtrim(as.character(accuracy), 5), 
                                 "\n ***", message1, 
                                 "\n***", message2, 
                                 "\n***", message3)
            
            ## log details
            updateTextInput(session, inputId = "logText", value = currentLog, label = "Logs")
            
            if (! is.null(currentOptions) && length(currentOptions) > 0)
                currentOption <<- currentOptions[1]
            ## update the checkboxes
            updateRadioButtons(session, inputId = "options", choices = currentOptions, selected = currentOption)
            "Input text rendered..."
        })
        
        
        
        ## listen for clicking in the radio widget
        output$captureChangeOption <- renderText({
            currentOption <<- input$options
            "Radiobuttons rendered..."
        })
        
        
        
        ## you can access the value of the buttons with input$id_of_button, e.g.
        output$captureAddChoice <- renderText({ 
            
            ## make this code depend on the button click
            buttonClicked <- input$addChoice
            
            print(paste("current option in AddChoice: .", currentOption, "."))
            
            if (is.null(currentOption) || length(currentOption) == 0) return
            
            hits <<- hits + 1
            
            ## get the choices selected
            if (! is.null(currentOption) && (nchar(currentOption) == 1) && (currentOption %in% c("d", "t", "s"))) {
                    newText <- paste(lastText, "'", currentOption, sep = "")
                    lastText <<- newText
                    updateTextInput(session = session, inputId = "inputText", value = newText)
            }
            else { 
                if (! is.null(currentOption) && (nchar(currentOption) == 2) && (currentOption %in% c("ll", "nt", "ve"))) {
                        newText <- paste(lastText, "'", currentOption, sep = "")
                        lastText <<- newText
                        updateTextInput(session = session, inputId = "inputText", value = newText)
                }
                else {
                    newText <- paste(lastText, currentOption)
                    lastText <<- newText
                    updateTextInput(session = session, inputId = "inputText", value = newText)
                }
            }
            paste("Button rendered...")
        })
        
    }
)