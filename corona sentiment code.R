library(rtweet)
library(tm)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(scales)
#to get current development version from github:
if(!requireNamespace("remotes",quietly = TRUE)){install.packages("remotes")}
##install dev version of rtweet from github
remotes::install_github("ropensci/rtweet")
library(rtweet)
#install httpuv if not already
if(!requireNamespace("httpuv",quietly = TRUE)){install.packages("httpuv")}
library(rtweet)
#generating tokem
api_key<-""
api_secret_key<-""
token<-create_token("",consumer_key = api_key,consumer_secret = api_secret_key)
token
vignette("auth",package = "rtweet")
vignette("intro",package = "rtweet")
coro<-search_tweets('#COVID-19 + #Coronavirus',n=30000, lang = "en",include_rts = FALSE)
# check data to see if there are emojis
head(coro$text)
#remove url
coro$stripped_text <- gsub("http.*","",  coro$text)
coro$stripped_text <- gsub("https.*","", coro$stripped_text)
## remove punctuation, convert to lowercase, add id for each tweet!
coro_clean <- coro %>% dplyr::select(stripped_text) %>% unnest_tokens(word, stripped_text)

#plot the top 15 words -- notice any issues?
coro_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")
# load(stopwords)
data("stop_words")
head(stop_words)
# remove stop words from your list of words
cleaned_tweet_words <- coro_clean %>%anti_join(stop_words)
# plot the top 15 words -- notice any issues?
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n,fill=word))+
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip() +theme_minimal()+
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

# retweet frequency
coro %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #coronavirus #covid19 Twitter statuses from 04/05/2020 - 08/05/2020 ",
    subtitle = "Twitter status (tweet) counts aggregated using six-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet @satomartin")+scale_x_datetime(date_breaks = "6 hours",labels = date_format("%a-%d\n%H:%M"))+theme(axis.text.x =element_text(angle = 50,hjust = 1))
#twitter user unique location
coro %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(20) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col(fill="#C57534") +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ",
       caption = "\nSource: Data collected from Twitter's REST API via rtweet @satomartin")

save_as_csv(coro,"coroo.csv",prepend_ids = TRUE,fileEncoding = "UTF-8")
coro <- read_csv("coro.csv")
coro2<-coro %>% select(5)

# build a corpus, and specify the source to be character vectors
myCorpus <- VCorpus(VectorSource(coro2))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  #??
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

myCorpus <- tm_map(myCorpus, stripWhitespace) 
myCorpus <- tm_map(myCorpus ,PlainTextDocument)
#
#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(myCorpus[[i]]))}

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm
## Freqency words and Association
idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx + (0:5), 101:110])
#inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq=15))
library(wordcloud)
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)
#mediummmmmmmmmmmmm
tokenizer <- function(x) { NGramTokenizer(x, Weka_control(min = 1, max = 1))}
#inspect frequent words
freq.terms <- findFreqTerms(tdm, lowfreq =  50)
View(freq.terms)
termFreq <- rowSums(as.matrix(tdm))
termFreq <- subset(termFreq, termFreq >=20)
dfm <- data.frame(term = names(termFreq), freq = termFreq)
View(dfm)
#word cloud
data_cleaned_freq <- rowSums(m)
pal <- brewer.pal(9,"Set1")
wordcloud(names(data_cleaned_freq), data_cleaned_freq, min.freq=50,max.words = 50, random.order=TRUE,random.color = TRUE, rot.per=.15, colors = pal,scale = c(3,1))
#word cloud 2
library(wordcloud2)
# colors
pel <- brewer.pal(9, "BuGn")
pel <- pel[-(1:4)]

#calculate the frequency of words as sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud2(dfm, color = "random-dark", backgroundColor = "white")
wordcloud2

View(dfm)
#Transform sentences into words
data_tibble <-coro%>% unnest_tokens(output = "words", input = text, token = "words")
#Remove stop words from tibble
virus_tibble_clean <- data_tibble %>% anti_join(stop_words, by=c("words"="word"))
data_tidy_sentiment <- virus_tibble_clean %>%
  # Inner join to bing lexicon by term = word 
  inner_join(get_sentiments("bing"), by = c("words" = "word")) %>% 
  # Count by term and sentiment, weighted by count
  count(words, sentiment) %>% 
  # Spread sentiment, using n as values 
  spread(sentiment, n, fill = 0) %>% 
  # Mutate to add a polarity column 
  mutate(polarity = positive - negative)
summary(data_tidy_sentiment)
data_tidy_pol <- data_tidy_sentiment %>% 
  # Filter for absolute polarity at least 80 
  filter(abs(polarity) >= 80) %>% 
  # Add positive/negative status 
  mutate( pos_or_neg = ifelse(polarity > 0, "positive", "negative") )
# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
library(ggthemes)
# Plot polarity vs. (term reordered by polarity), filled by pos_or_neg
ggplot(data_tidy_pol, aes(reorder(words, polarity), polarity, fill = pos_or_neg)) + geom_col() + ggtitle("Coronavirus related tweets: Sentiment Word Frequency")+
  # Rotate text and vertically justify 
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size =
                                                     10))+ xlab("Word")+ labs(caption = "@SatoMartin")+theme(plot.caption = element_text(size=10,hjust = 1,face = "italic",color = "blue"))

word_counts <- virus_tibble_clean %>% 
  # Implement sentiment analysis using the "bing" lexicon 
  inner_join(get_sentiments("bing"), by = c("words" = "word")) %>%
  # Count by word and sentiment 
  count(words, sentiment)
top_words <- word_counts %>% 
  # Group by sentiment 
  group_by(sentiment) %>% 
  # Take the top 10 for each sentiment
  top_n(10) %>% ungroup() %>% 
  # Make word a factor in order of n
  mutate(words = reorder(words, n))
# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(words, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) + theme_minimal()+
  geom_text(aes(label = n, hjust=1), size = 3.5, color = "black") +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip() +
  ggtitle("Most common positive and negative words")+labs(caption = "@SatoMartin")+theme(plot.caption = element_text(size=10,hjust = 1,face = "italic",color = "blue"))
#sentiment word cloud
library(textdata)
data_tidy <- virus_tibble_clean %>%
  # Inner join to nrc lexicon
  inner_join(get_sentiments("nrc"), by = c("words" = "word")) %>% 
  # Drop positive or negative
  filter(!grepl("positive|negative", sentiment)) %>% 
  # Count by sentiment and term
  count(sentiment, words) %>% 
  # Spread sentiment, using n for values
  spread(sentiment, n, fill = 0)  %>% 
  # Convert to data.frame, making term the row names
  data.frame(row.names = "words")
# Plot comparison cloud
comparison.cloud(data_tidy, max.words = 80, title.size = 1)
library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(coro, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
attach(sentimentscores)
write.csv(sentimentscores,"sentimento.csv")
sentimento <- read_csv("sentimento.csv")
sentiment<-sentimento %>% rename(sentiment=X1,score=colSums.ew_sentiment.....)
ggplot(data=sentiment,aes(x=sentiment,y=score))+ geom_bar(aes(fill=sentiment),stat = "identity")+ theme(legend.position="none")+ xlab("Sentiments")+ylab("Scores")+ ggtitle("Total sentiment based on scores")+ theme_minimal()+
  labs(caption = "@SatoMartin")+theme(plot.caption = element_text(size=10,hjust = 1,face = "italic",color = "blue"))
ndemo<-get_sentiment(tweets)

