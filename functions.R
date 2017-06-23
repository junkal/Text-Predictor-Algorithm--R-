# FUNCTION DEFINITIONS

# Loads a file from the given path into a character vector
load_file <- function(path, encoding = "UTF-8") {
  con <- file(path, open = "rb")
  data <- readLines(con, encoding = encoding, skipNul = TRUE)
  close(con)
  return(data)
}

# Cleans data (character vector) for further text processing
data_clean <- function(data, tolatin = FALSE, removeProfanity = FALSE,
                       removeStopwords = FALSE) {
  
  # Substitute right, left single quotation marks and backtick for apostrophe
  data1 <- gsub("\u2018|\u2019|`", "'", data)
  
  # Uses one of two ways how to get rid of extra characters
  if (tolatin == TRUE) {
    # Converts text from UTF-8 to latin1
    data1 <- iconv(data1, from="UTF-8", to="latin1", sub=" ")
  }
  
  # Substitutes everything that is NOT a character, apostrophe and 
  # several punctuations for white space (clear special characters)
  data1 <- gsub("[^a-zA-Zéïçñ0-9'\\.,\\?! ]", " ", data1)
  
  # Converts text to lower case
  data1 <- tolower(data1)
  
  if (removeProfanity == TRUE) {
    # Filters out profanity words
    data1 <- gsub(paste(swearWords, collapse = "|(^| )"), " bleep", data1)
  }
  
  if (removeStopwords == TRUE) {
    # Filters out stopwords
    for(i in seq_along(stopwords)) {
      data1 <- gsub(stopwords[i], " ", data1, fixed = TRUE)
    }
  }
  
  # Gets rid of extra spaces
  data1 <- gsub("( )+", " ", data1)     
  
  return(data1)
}

# Given a character vector, creates a token list
get_tokens <- function(data) {
  stopifnot(is.character(data))
  
  options <- stri_opts_brkiter(type = "word", skip_word_none = TRUE, 
                               skip_word_number = TRUE)
  tokens <- unlist(stri_split_boundaries(data, opts_brkiter = options))
  return(tokens)
}

# Creates n-gram data.table and aggregates by frequency (given min.freq)
get_ngrams <- function(tokens, n, min.freq = 1) {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  
  len <- length(tokens)
  
  if (n == 1) {
    ngram0 <- data.table(t0 = tokens, 
                         count = 1)
    ngram <- ngram0[, sum(count), by = t0][V1 >= min.freq]
  } else if (n == 2) {
    ngram0 <- data.table(t1 = tokens[-len], 
                         t0 = tokens[-1], 
                         count = 1)
    ngram <- ngram0[, sum(count), by = c("t1", "t0")][V1 >= min.freq]
    setkey(ngram, t1)
  } else if (n == 3) {
    ngram0 <- data.table(t2 = tokens[-c(len-1, len)],
                         t1 = tokens[-c(1, len)], 
                         t0 = tokens[-c(1, 2)],
                         count = 1)
    ngram <- ngram0[, sum(count), by = c("t2", "t1", "t0")][
      V1 >= min.freq]
    setkey(ngram, t2, t1)
  } else if (n == 4) {
    ngram0 <- data.table(t3 = tokens[-c(len-2, len-1, len)],
                         t2 = tokens[-c(1, len-1, len)],
                         t1 = tokens[-c(1, 2, len)],
                         t0 = tokens[-c(1, 2, 3)],
                         count = 1)
    ngram <- ngram0[, sum(count), by = c("t3", "t2", "t1", "t0")][
      V1 >= min.freq]
    setkey(ngram, t3, t2, t1)
  } else if (n == 5) {
    ngram0 <- data.table(t4 = tokens[-c((len-3):len)],
                         t3 = tokens[-c(1, (len-2):len)],
                         t2 = tokens[-c(1, 2, len-1, len)],
                         t1 = tokens[-c(1:3, len)],
                         t0 = tokens[-c(1:4)],
                         count = 1)
    ngram <- ngram0[, sum(count), by = c("t4", "t3", "t2", "t1", "t0")][
      V1 >= min.freq]
    setkey(ngram, t4, t3, t2, t1)
  } else {
    ngram <- character(0)
  }
  
  if (length(ngram) > 0) {
    setnames(ngram, "V1", "freq")
  }
  
  return(ngram)
}

# Function for filtering only the top <ntop> best words in each group
filter_top <- function(ngram, ntop) {
  stopifnot(is.data.table(ngram), is.numeric(ntop), is.finite(ntop), 
            ntop > 0)
  
  # Save keys of the ngram
  keyvec <- key(ngram)
  
  # Order ngram by key values and reverse frequency (most frequent on top)
  # and set the key back for faster calculation
  setorderv(ngram, c(keyvec, "freq"), c(rep(1, length(keyvec)), -1))
  setkeyv(ngram, keyvec)
  
  # Creates the ngram data.table with maximum of <ntop> occurences for a key
  ngramTop <- ngram[, .SD[1:min(ntop, .N)], by = key(ngram)]
  
  # Set key for the output data.table
  setkeyv(ngramTop, keyvec)
  
  return(ngramTop)
}


# Algorithm for the next word prediction
pred_word_ngram <- function(string, ngrams, dictionary = NULL, 
                            indexed = !is.null(dictionary),
                            backoff.vec = c(rep(2, length(ngrams))), 
                            nword = 5, incl.stopwords = TRUE) {
  
  options(warn = -1)
  stopifnot(is.character(string), is.numeric(nword), is.finite(nword), 
            nword > 0, is.logical(incl.stopwords))
  
  wordList <- get_tokens(data_clean(string, F, F))
  
  if (incl.stopwords == FALSE) {
    wordList <- wordList[!(wordList %in% stopwords)]
  }
  
  if(indexed == TRUE) {
    # Uses the dictionary to translate words to indices
    wordListAux <- lapply(seq_along(wordList), function(i) {
      dictionary[token == wordList[i], index]
    })
    
    # If a word is not found in the dictionary, use index 0
    wordList <- sapply(wordListAux, function(x) {
      ifelse(length(x)==0, 0, x)
    })
  }
  
  len <- length(wordList)
  
  used.ngram <- "None"
  topPred <- data.table(t0 = rep(NA, nword),
                        freq = rep(NA, nword))
  
  for (i in length(ngrams):2) {
    if (len >= i-1 & all(is.na(topPred[, t0])) & !is.null(ngrams[[i]])) {
      topPred <- ngrams[[i]][as.list(tail(wordList, i - 1))][
        freq >= backoff.vec[i]][order(-freq)][
          1:min(nword, .N), list(t0, freq)]
      used.ngram <- paste0(i, "-gram")
    }
  }
  
  if (all(is.na(topPred[, t0])) & !is.null(ngrams[[1]])) {
    topPred <- ngrams[[1]][order(-freq)][
      1:min(nword, .N), list(t0, freq)]
    used.ngram <- "1-gram"
  }
  
  if(indexed == TRUE) {
    topPred$t0 <- 
      sapply(topPred$t0, function(i) dictionary[index == i, token])
  }
  
  return(list(ngram = used.ngram, 
              predictions = topPred))
}

pred_next_word <- function(string, ngrams, dictionary = NULL, nword = 5, 
                           swearWords) {
  resDT <- pred_word_ngram(string, 
                           ngrams = ngrams,
                           dictionary = dictionary, 
                           indexed = !is.null(dictionary),
                           backoff.vec = c(1, 2, 2, 2), 
                           nword = nword, 
                           incl.stopwords = TRUE)[[2]]
  
  for(i in seq_along(swearWords)) {
    resDT$t0 <- gsub(swearWords[i], "<bleep>", resDT$t0)
  }
  
  return(resDT)
}