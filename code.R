
library(dplyr)
library(ggplot2)
library(stringi)
library(data.table)
library(shiny)
library(shinyapps)

source("./functions.R")

##### DATA LOADING #####

dataBlogs <- load_file("./final/en_US/en_US.blogs.txt")
dataNews <- load_file("./final/en_US/en_US.news.txt")
dataTwitter <- load_file("./final/en_US/en_US.twitter.txt")

swearWords <- readLines("./text_predictor/swearWords.txt", warn = FALSE)
stopwords  <- readLines("./text_predictor/stopwords.txt", warn = FALSE)

##### DATA CLEANING #####

dataBlogsC <- data_clean(dataBlogs, TRUE, FALSE, FALSE)
dataNewsC <- data_clean(dataNews, TRUE, FALSE, FALSE)
dataTwitterC <- data_clean(dataTwitter, TRUE, FALSE, FALSE) # 196 s

rm(dataBlogs, dataNews, dataTwitter)

dataAllC <- c(dataBlogsC, dataNewsC, dataTwitterC)

rm(dataBlogsC, dataNewsC, dataTwitterC)

##### CREATE NGRAMS #####

# Get tokens
tokensAll <- get_tokens(dataAllC) # 32 s
rm(dataAllC)

# Get dictionary
dictionaryFull <- data.table(index = seq_along(unique(tokensAll)),
                             token = unique(tokensAll))
setkey(dictionaryFull, token)

# Create a data.table with tokens, keep a variable for token order
tokensAllDT <- data.table(order = seq_along(tokensAll),
                          token = tokensAll)
setkey(tokensAllDT, token)
rm(tokensAll)

# Create integer tokens by joining the data.tables, isolate only indices
tokensAllI <- tokensAllDT[dictionaryFull][order(order)][, index]
rm(tokensAllDT)

unigramsI  <- get_ngrams(tokensAllI, n = 1, min.freq = 2) # 6 s
bigramsI   <- get_ngrams(tokensAllI, n = 2, min.freq = 2) # 25 s
trigramsI  <- get_ngrams(tokensAllI, n = 3, min.freq = 2) # 49 s
fourgramsI <- get_ngrams(tokensAllI, n = 4, min.freq = 2) # 482 s (memory limit)
fivegramsI <- get_ngrams(tokensAllI, n = 5, min.freq = 2) # cluster
rm(tokensAllI)

 save(unigramsI, file = "./unigramsI.RData")
 save(bigramsI, file = "./bigramsI.RData")
 save(trigramsI, file = "./trigramsI.RData")
 save(fourgramsI, file = "./fourgramsI.RData")
 save(fivegramsI, file = "./fivegramsI.RData")

setkey(dictionaryFull, index)
dictionary <- dictionaryFull[index %in% unigramsI$t0]
rm(dictionaryFull)

 save(dictionary, file = "./dictionary.RData")


##### SET OF NGRAMS WITHOUT STOPWORDS #####

# load("./RData/tokensAll.RData")

tokensNoSw <- tokensAll[!(tokensAll %in% stopwords)] # 15 s
rm(tokensAll)

unigramsNoSw <- get_ngrams(tokensNoSw, n = 1, min.freq = 2) # 6 s
bigramsNoSw <- get_ngrams(tokensNoSw, n = 2, min.freq = 2) # 34 s
trigramsNoSw <- get_ngrams(tokensNoSw, n = 3, min.freq = 2) # 101 s
fourgramsNoSw <- get_ngrams(tokensNoSw, n = 4, min.freq = 2) # 194 s

# save(unigramsNoSw, file = "./RData/unigramsNoSw.RData")
# save(bigramsNoSw, file = "./RData/bigramsNoSw.RData")
# save(trigramsNoSw, file = "./RData/trigramsNoSw.RData")
# save(fourgramsNoSw, file = "./RData/fourgramsNoSw.RData")


##### REDUCING NGRAM SIZE #####

# Calls the function that filters only <ntop> top suggestions for an (n-1)-gram
unigramsITop  <- unigramsI[order(-freq)][1:5]
bigramsITop   <- filter_top(bigramsI, ntop = 5)   # 62 s
trigramsITop  <- filter_top(trigramsI, ntop = 5)  # 868 s
fourgramsITop <- filter_top(fourgramsI, ntop = 5) # 1456 s
fivegramsITop <- filter_top(fivegramsI, ntop = 5) # 924 s

# save(unigramsITop, file = "./RData/unigramsITop.RData")
# save(bigramsITop, file = "./RData/bigramsITop.RData")
# save(trigramsITop, file = "./RData/trigramsITop.RData")
# save(fourgramsITop, file = "./RData/fourgramsITop.RData")
# save(fivegramsITop, file = "./RData/fivegramsITop.RData")


##### TESTING #####

load("./RData/dictionary.RData")
load("./RData/unigramsITop.RData")
load("./RData/bigramsITop.RData")
load("./RData/trigramsITop.RData")
load("./RData/fourgramsITop.RData")
load("./RData/fivegramsITop.RData")

# Example
pred_next_word(string = "Today I bought a bottle of", 
               ngrams = list(unigramsITop, bigramsITop, 
                             trigramsITop, fourgramsITop), 
               dictionary)

# Create test set
load("./RData/_clean_data.RData")

set.seed(1222)
nLines <- 300
testLines <- c(sample(dataBlogsC, nLines, replace = FALSE),
               sample(dataNewsC, nLines, replace = FALSE),
               sample(dataTwitterC, nLines, replace = FALSE))

options <- stri_opts_brkiter(type = "word", skip_word_none = TRUE, 
                             skip_word_number = TRUE)
tokensTest <- stri_split_boundaries(testLines, opts_brkiter = options)

# Takes strings of <testLen> length and creates a list of length-two vectors
#    First element contains preceding text and second element is the word to be 
#    predicted
testLen <- 5
testStrings<- lapply(tokensTest, 
                     function(tokens) {
                          c(paste(tokens[1: min(testLen, length(tokens)) - 1],
                                  collapse = " "),
                            tokens[min(testLen, length(tokens))])
                     })

# Get top <nword> predictions
nword <- 5

predictions <- 
     lapply(testStrings, 
            function(string) {
                 c(pred_next_word(
                      string[1], 
                      ngrams = list(unigramsITop, bigramsITop, 
                                    trigramsITop, fourgramsITop), 
                      dictionary, nword = nword)[, t0],
                   rep(NA, nword))[1:nword]
            })

testResult <- data.frame(actual = unlist(lapply(testStrings, function(x) x[2])),
                         pred = matrix(unlist(predictions), 
                                       nrow = 3*nLines, byrow = TRUE),
                         stringsAsFactors = FALSE)

predOk <- cumsum(sapply(1:nword, function(i) {
     sum(testResult$actual == testResult[, i+1], na.rm = TRUE)
}))

predAccur <- predOk/(3*nLines)

rm(nLines, nword, options, predAccur, predOk, predictions, testLen, testLines, 
   testResult, testStrings, tokensTest)
rm(dataBlogsC, dataNewsC, dataTwitterC)

##### PLOT PREDICTIONS FOR SHINY #####

# Plot predictions
predictions <- pred_word_ngram(
     string = "Today I bought a bottle of", 
     ngrams = list(unigrams, bigramsTop, trigramsTop, fourgramsTop), 
     backoff.vec = c(1, 2, 2, 2), nword = 10, incl.stopwords = TRUE)[[2]][
          order(freq)]

predictions$t0 <- factor(predictions$t0, levels = as.character(predictions$t0))
g <- ggplot(predictions[1:5], aes(x = t0, y = freq, fill = freq)) + 
     geom_bar(stat = "identity") + coord_flip() + 
     xlab(NULL) + ylab(NULL) + 
     scale_y_discrete(breaks = NULL) +
     scale_fill_continuous(low = "powderblue", high = "slateblue4") +
     theme_minimal() + 
     theme(panel.border = element_blank(), panel.grid.major = element_blank(),
           axis.ticks = element_blank(), legend.position = "none",
           axis.text.y = element_text(size = 18, family = "Helvetica"))
     

##### SHINY #####

# Prepare the data for shinyapps.io (run from shiny project)
saveRDS(dictionary, file = "./WordPredApp/data/dictionary.rds")
saveRDS(unigramsITop, file = "./WordPredApp/data/unigrams.rds")
saveRDS(bigramsITop, file = "./WordPredApp/data/bigrams.rds")
saveRDS(trigramsITop, file = "./WordPredApp/data/trigrams.rds")
saveRDS(fourgramsITop, file = "./WordPredApp/data/fourgrams.rds")
saveRDS(fivegramsITop, file = "./WordPredApp/data/fivegrams.rds")

# Run app
runApp(appDir = "./WordPredApp")

# Deploy app
deployApp(appDir = "./WordPredApp", appName = "WordPredApp")

# OPEN QUESTIONS:
# - spell checker to reduce number of ngrams 
#      http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/
# - how to implement model with removed stopwords?
# - implement skip-grams?
# - tagging beginning and end of sentences?
# - implement approximate matching?
# - smoothing: linear interpolation / Good-Turing discounting / Lidstone's
#      smoothing / Katz' back-off model / Kneser-Ney smoothing
# - explore word bags - https://class.coursera.org/dsscapstone-003/forum/thread?thread_id=131#post-558
# - index tables to decrease object size (integer values + lookup)