# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("NLP")

#antivirtus prevent install, method 1
debug(utils:::unpackPkgZip)
install.packages("wordcloud")

#method 2 fix!!!
trace(utils:::unpackPkgZip, quote(Sys.sleep(2)), at = which(grepl("Sys.sleep", body(utils:::unpackPkgZip), fixed = TRUE)))
install.packages("wordcloud")

trace(utils:::unpackPkgZip, quote(Sys.sleep(2)), at = which(grepl("Sys.sleep", body(utils:::unpackPkgZip), fixed = TRUE)))
install.packages("NLP") 

# Load
library("NLP")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# remember switch \ to /
df1<- read.csv("C:/Users/wtao/Desktop/testing_survey_2.csv")


#type=c("text", "url", "file")
RSiteSearch("rquery.wordcloud")


text <- readLines(file.choose("C:/Users/wtao/Desktop/survey2text.txt"))

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

inspect(docs)


# text mining starting from here
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# Text stemming
# docs <- tm_map(docs, stemDocument)


# build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#run this to generate your word cloud plot
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# this part haven't really work yet
rquery.wordcloud(df1$q7.improve, type=c("text"), 
                 lang="english", excludeWords = NULL, 
                 textStemming = FALSE,  colorPalette="Dark2",
                 max.words=200)

help.search("rquery.wordcloud")