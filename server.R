library(shiny)
library(stringi)
library(data.table)
library(ggplot2)

swearWords <- readLines("./swearWords.txt", warn = FALSE)
stopwords  <- readLines("./stopwords.txt", warn = FALSE)

dictionary <- readRDS("./dictionary.rds")
unigrams   <- readRDS("./unigrams.rds")
bigrams    <- readRDS("./bigrams.rds")
trigrams   <- readRDS("./trigrams.rds")
fourgrams  <- readRDS("./fourgrams.rds")

source("./functions.R")

global.env <- new.env()
global.env$count <- 1

shinyServer(
  function(input, output) {
    observe({
      
      if (input$predict == global.env$count)
      { 
        #ensure that this section runs only when predict button is clicked
        global.env$count <- global.env$count + 1
              
        prediction <- pred_next_word(string = input$word, 
                         ngrams = list(unigrams, bigrams, trigrams, fourgrams),
                         dictionary = dictionary, 
                         nword = 3,
                         swearWords)
        
        data <- strsplit(prediction$t0, " ")
        
        output$text1 <- renderText({ 
          data[[1]]
        })
        
        output$text2 <- renderText({ 
          data[[2]]
        })
        
        output$text3 <- renderText({ 
          data[[3]]
        })
        
      }#end if 
    })
  }
)
