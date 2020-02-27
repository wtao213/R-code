##########################################################
## start Feb,27th,2019
## try to do some txt mining and word cloud
##
#########################################################


## install.packages("tm")  # for text mining
## install.packages("SnowballC") # for text stemming
## install.packages("wordcloud") # word-cloud generator 
## install.packages("RColorBrewer") # color palettes
## install.packages("NLP")




library("NLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")

## use setwd('') to change / to \
## read raw data
df<- read.csv("C:/Users/012790/Desktop/survey/comments_survey1819.csv")

rm(df_V2)

a <-df$improve_area

## remove the empty string line, there are 1622 comments are left
## compared with the original 3874
a <- a[a != ""]


## translate the line to corups
tweetsDS.Corpus<- Corpus(VectorSource(a))

docs <-tweetsDS.Corpus
#look at the word inside
inspect(docs)


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))


# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("better", "like","make","lower"))



# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)






# Text stemming
# docs <- tm_map(docs, stemDocument)

##################################################################
## prepare and show your word could
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 50,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set1"))





###########################################################################
## some other correlation test for your top words
## find the words relate to the associate words
## corlimit is set the lowerest value for another termed correlation with your terms


## correlation words for account or accounts
findAssocs(dtm, terms = "account", corlimit = 0.2)
findAssocs(dtm, terms = "account", corlimit = 0.25)


findAssocs(dtm, terms = "accounts", corlimit = 0.4)

## correlation words for fees
findAssocs(dtm, terms = "fees", corlimit = 0.25)
findAssocs(dtm, terms = "fee", corlimit = 0.25)


## correlation words for trading
findAssocs(dtm, terms = "trading", corlimit = 0.25)
findAssocs(dtm, terms = "trade", corlimit = 0.2)

## correlation words for questrade
findAssocs(dtm, terms = "questrade", corlimit = 0.20)






##look at the words at least shows up 100 times
findFreqTerms(dtm, lowfreq = 100)


# Bar plot of the frequency for the top10
barplot(d[1:20,]$freq, las = 2, 
        names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")








############################################################################
## second part: now we want to split customer to promoter vs detractors
## too see whether they comment very different in question

## filter data for promoter in 

levels(df$NPS._Segment)


pro <- df[df$Overall_satisfy >= 8,]
det <- df[df$Overall_satisfy <= 4,]


## for high score people 8.9.10
rm(a)
a <- pro$improve_area
a <- a[a != ""]
docs<- Corpus(VectorSource(a))
inspect(docs)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
## docs <- tm_map(docs, removeWords, c("better", "like","make","lower"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)



dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 20,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set1"))


## repeat it for below with low score
a <- det$improve_area
a <- a[a != ""]
docs<- Corpus(VectorSource(a))
inspect(docs)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
## docs <- tm_map(docs, removeWords, c("better", "like","make","lower"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)



dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 20,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Set1"))


