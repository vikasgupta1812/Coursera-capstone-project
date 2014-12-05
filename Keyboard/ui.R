library(shiny)
shinyUI(fluidPage(
    responsive = TRUE,
    
    titlePanel(windowTitle = "Predictive keyboard", title = "Predictive Keyboard"),
    
    sidebarLayout(position = "left",
                  sidebarPanel("",
                            helpText("Input any text you want in this text box"),
                            tagList(
                                tags$textarea(id="inputText", rows=10, cols=20, "")
                            ),
                            radioButtons(inputId = "options", label = "Suggestions for next word", choices = c("", "", "")), 
                            actionButton(inputId = "addChoice", label = "Add choice"),
                            br(),
                            br(),
                            helpText("Logs"),
                            tagList(
                              tags$textarea(id="logText", rows=6, cols=40)
                            )
                  ), 
                  mainPanel(
                            helpText("This is a model of a predictive keyboard. As the user starts writing ",
                                     "text in the input box at the left, the system tries to suggest the ", 
                                     "most probable following words. Clicking on one of the suggestions ", 
                                     "presented and then clicking on the button 'Add choice' should update the ",
                                     "text in the input box with the selected choice. Unfortunately, it doesn't ",
                                     "immediately suggest a new word, there was some issue with the reactivity of ", 
                                     "Shiny, which was leading to some circular reactions - I wasn't able to break ", 
                                     "that dependency in a simple manner - so the user will have to continue to type ", 
                                     "at least a space in the input text, so then the system again suggests ",
                                     "a word."),
                            helpText("The system will also try to track the accuracy of the predictor ", 
                                     "by counting the number of words and how many times the user does choose ", 
                                     "one of the presented options. Will present the latest accuracy calculation ", 
                                     "and some details (like probabilities and where did the suggestion come from) ",
                                     "in a box to the left, named 'Logs'. One more thing. The system needs to load ",
                                     "some data tables, so please wait with the text input until the 'rendered..' ", 
                                     "messages show up. "), 
                            helpText("English please..."),
                            br(),
                            textOutput(outputId = "captureWriting"), 
                            textOutput(outputId = "captureChangeOption"), 
                            textOutput(outputId = "captureAddChoice"), 
                            
                            br(),
                            br(),
                            br(),
                            em("Capstone project, Data Science Specialization at Coursera"), 
                            br(),
                            em("Sebastian Popa, oct. 2014")
                  )
                 )
            )
)
