#install.packages('twitteR')
library(twitteR)
#install.packages('RCurl')
library(RCurl)
#install.packages('ROAuth')
library(ROAuth)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('SnowballC')
library(SnowballC)

#install.packages("stringr")
library(stringr)
#install.packages('wordcloud')
library(wordcloud)
#install.packages('tm')
library('tm')
library(dplyr)

library(RColorBrewer)
#install.packages("httr", repos = "http://cran.us.r-project.org")

library('httr')
#install.packages("syuzhet")
library('syuzhet')

API_Key<-"Enter your API key"
API_Secret<-"Enter your API key secret"
Access_Token<-"Enter your Access token"
Access_Token_Secret<-"Enter your Access token secret"

setup_twitter_oauth(API_Key,API_Secret,Access_Token,Access_Token_Secret)

EPS_tweets = searchTwitter("#EPS",since = "2021-02-01",until = "2021-02-25", n=670,lang = "en")

EPS_tweets.df = twListToDF(EPS_tweets)
head(EPS_tweets.df)

library(xlsx)
write.xlsx(EPS_tweets.df, "C:/Users/Nivetheni/Desktop/EPS_tweets.xlsx")

EPS_tweets.df$text = gsub("&amp", "", EPS_tweets.df$text)
EPS_tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", EPS_tweets.df$text)
EPS_tweets.df$text = gsub("@\\w+", "", EPS_tweets.df$text)

EPS_tweets.df$text = gsub("[[:punct:]]", "", EPS_tweets.df$text)
EPS_tweets.df$text = gsub("[[:digit:]]", "", EPS_tweets.df$text)
EPS_tweets.df$text = gsub("http\\w+", "", EPS_tweets.df$text)
EPS_tweets.df$text = gsub("[ \t]{2,}", "", EPS_tweets.df$text)
EPS_tweets.df$text = gsub("^\\s+|\\s+$", "", EPS_tweets.df$text)

EPS_tweets.df$text <- iconv(EPS_tweets.df$text, "UTF-8", "ASCII", sub = "")

write.xlsx(EPS_tweets.df, "C:/Users/Nivetheni/Desktop/EPS_tweets_preprocessed.xlsx")

emotions <- get_nrc_sentiment(EPS_tweets.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion for EPS")
p

# Create comparison word cloud data

wordcloud_tweet = c(
  paste(EPS_tweets.df$text[emotions$anger > 0], collapse=" "),
  paste(EPS_tweets.df$text[emotions$anticipation > 0], collapse=" "),
  paste(EPS_tweets.df$text[emotions$disgust > 0], collapse=" "),
  paste(EPS_tweets.df$text[emotions$fear > 0], collapse=" "),
  paste(EPS_tweets.df$text[emotions$joy > 0], collapse=" "),
  paste(EPS_tweets.df$text[emotions$sadness > 0], collapse=" "),
  paste(EPS_tweets.df$text[emotions$surprise > 0], collapse=" "),
  paste(EPS_tweets.df$text[emotions$trust > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

