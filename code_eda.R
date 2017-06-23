##### EXPLORATORY DATA ANALYSIS #####

length(dataBlogs)
length(dataNews)
length(dataTwitter)

corpusStats <- data.frame(corpus = c(rep("Blogs", length(dataBlogs)),
                                     rep("News", length(dataNews)),
                                     rep("Twitter", length(dataTwitter))),
                          row.length = c(str_length(dataBlogs),
                                         str_length(dataNews),
                                         str_length(dataTwitter)),
                          word.count = c(str_count(dataBlogs, "( ){1, }") + 1,
                                         str_count(dataNews, "( ){1, }") + 1,
                                         str_count(dataTwitter, "( ){1, }") + 1)
)

summary(filter(corpusStats, corpus == "Blogs")$row.length)
summary(filter(corpusStats, corpus == "News")$row.length)
summary(filter(corpusStats, corpus == "Twitter")$row.length)

g1 <- ggplot(corpusStats, aes(x = row.length)) +
     geom_histogram(binwidth = 10, color = I("white"), fill = I("royalblue4")) +
     facet_wrap(~ corpus, ncol = 3) +
     scale_x_continuous(limits = c(0, 500)) +
     ggtitle("Distribution of row lengths")

summary(filter(corpusStats, corpus == "Blogs")$word.count)
summary(filter(corpusStats, corpus == "News")$word.count)
summary(filter(corpusStats, corpus == "Twitter")$word.count)

g2 <- ggplot(corpusStats, aes(x = word.count)) +
     geom_histogram(binwidth = 2, color = I("white"), fill = I("royalblue4")) +
     facet_wrap(~ corpus, ncol = 3) +
     scale_x_continuous(limits = c(0, 100)) +
     ggtitle("Distribution of word counts")


##### PLOT NGRAM FREQUENCIES #####

plotData <- ngramBlogs[[1]][1:25, ]
plotData$token <- factor(plotData$token, levels = as.character(plotData$token))

g11 <- ggplot(plotData, aes(x = token, y = count)) +
     geom_bar(stat = "identity") +
     ggtitle("Unigram frequencies - Blogs corpus")

plotData <- ngramBlogs[[2]][1:25, ]
plotData$token <- factor(plotData$token, levels = as.character(plotData$token))

g12 <- ggplot(plotData, aes(x = token, y = count)) +
     geom_bar(stat = "identity") +
     ggtitle("Bigram frequencies - Blogs corpus") + 
     theme(axis.text.x = element_text(angle = 60, hjust = 1))

plotData <- ngramBlogs[[3]][1:25, ]
plotData$token <- factor(plotData$token, levels = as.character(plotData$token))

g13 <- ggplot(plotData, aes(x = token, y = count)) +
     geom_bar(stat = "identity") +
     ggtitle("Trigram frequencies - Blogs corpus") + 
     theme(axis.text.x = element_text(angle = 60, hjust = 1))

# ngram coverage

coverUnigram <- 
     data.frame(corpus = c(rep("Blogs", dim(ngramBlogs[[1]])[1]),
                           rep("News", dim(ngramNews[[1]])[1]),
                           rep("Twitter", dim(ngramTwitter[[1]])[1])),
                word.count = c(seq_along(ngramBlogs[[1]]$token),
                               seq_along(ngramNews[[1]]$token),
                               seq_along(ngramTwitter[[1]]$token)),
                coverage = c(with(ngramBlogs[[1]], cumsum(count)/sum(count)),
                             with(ngramNews[[1]], cumsum(count)/sum(count)),
                             with(ngramTwitter[[1]], cumsum(count)/sum(count)))
     )

coverUnigramTable <- data.frame(coverage = paste(seq(10, 100, 10), "%"),
                                Blogs = vector(length = 10), 
                                News = vector(length = 10), 
                                Twitter = vector(length = 10))
for (i in 1:10) {
     coverUnigramTable[[2]][i] <- 
          max(which(filter(coverUnigram, corpus=="Blogs")$coverage < i/10)) + 1
     coverUnigramTable[[3]][i] <- 
          max(which(filter(coverUnigram, corpus=="News")$coverage < i/10)) + 1
     coverUnigramTable[[4]][i] <- 
          max(which(filter(coverUnigram, corpus=="Twitter")$coverage < i/10))+ 1
}

g20 <- ggplot(coverUnigram, aes(y = coverage, x = word.count)) + 
     geom_line(size = 1, color = I("royalblue4")) +
     scale_x_log10(breaks = c(10, 100, 1000, 10000, 100000), labels = comma) +
     xlab("Words needed") +
     ggtitle("Corpus coverage by unigrams") +
     facet_wrap(~corpus)
