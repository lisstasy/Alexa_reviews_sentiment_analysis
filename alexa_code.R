rm(list= ls())
# PACKAGES NEEDED --------------------------------------------------------------
install.packages("tidyverse")
install.packages("readr")
install.packages("RColorBrewer")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("stringr")
install.packages("stringi")
install.packages("XML")
install.packages("RCurl")
install.packages("httr")
install.packages("tm")
install.packages("SnowballC")
install.packages("dplyr")
install.packages("sentimentr")
install.packages("rtweet")
install.packages("ggplot2")
install.packages("data.table")
install.packages("magrittr")
install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
#LIBRARY -----------------------------------------------------------------------
library("tidyverse")
library("readr")
library("RColorBrewer")
library("tidytext")
library("wordcloud")
library("stringr")
library("stringi")
library("XML")
library("RCurl")
library("httr")
library("tm")
library("SnowballC")
library("dplyr")
library("sentimentr")
library("rtweet")
library("ggplot2")
library("data.table")
library("magrittr")
library("quanteda")
library("quanteda.textstats") # loads function textstat_frequency to name space
library("quanteda.textplots")

reviews <- read_tsv("amazon_alexa.tsv")

reviews<- reviews%>%
  distinct()


# EXPLORATORY ANALYSIS ---------------------------------------------------------
reviews %>% 
  group_by(variation) %>% 
  count() %>% 
  arrange(desc(n))

avg_rating<- reviews%>%
  group_by(variation)%>%
  summarize(rating_average=mean(rating))%>%
  arrange(desc(rating_average))


# Load the data as a corpus
alexa_reviews <- VectorSource(reviews$verified_reviews) 
reviews_corpus <- VCorpus(alexa_reviews)

#cleaning
clean_reviews <- tm_map(reviews_corpus, content_transformer(tolower))
clean_reviews <- tm_map(clean_reviews, removeNumbers)
clean_reviews <- tm_map(clean_reviews, removeWords, stopwords("en"))
clean_reviews <- tm_map(clean_reviews, removeWords, c("Alexa", "alexa", "Amazon", "amazon"))
clean_reviews <- tm_map(clean_reviews, removePunctuation)
clean_reviews <- tm_map(clean_reviews, stripWhitespace)

# Building a term-document matrix
clean_reviews_dtm <- TermDocumentMatrix(clean_reviews)
clean_reviews_m <- as.matrix(clean_reviews_dtm)

#sorting the words by the most frequent words
v <- sort(rowSums(clean_reviews_m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 50)

#after seeing the most common words, we see some words that interfere with our analys and doesn't bring any importance, that's why we need to delite them.
clean_reviews <- tm_map(clean_reviews, removeWords, c("music","speaker","dot","device","devices","product","can", "one", "use","just","get","still","bought","will","really","even","far","also"))
clean_reviews_dtm <- TermDocumentMatrix(clean_reviews)
clean_reviews_m <- as.matrix(clean_reviews_dtm)


#sorting the words by the most frequent words (cleaned)
frequent_words <- rowSums(clean_reviews_m) %>% 
  sort(decreasing = TRUE)



#the barchart of the 30 most frequent words
barplot(frequent_words[1:20], las = 2, col="orange", main ="Top 20 most frequent words",
        ylab = "Word frequencies")

#wordcloud
cloud <- reviews %>%  unnest_tokens(word, verified_reviews)

cloud <- cloud %>% 
  group_by(word) %>% 
  mutate(freq = n()) %>% 
  select(rating, variation, feedback, word, freq) 

cloud<- cloud %>%  inner_join(get_sentiments("bing"), by = "word")

cloud <- cloud %>%
  count(word, sentiment) %>%
  mutate(color = ifelse(sentiment == "positive", "darkgreen", "red"))

wordcloud(cloud$word, cloud$n, random.order = FALSE, colors = cloud$color, ordered.colors = TRUE)

# Set as a data table
setDT(reviews)

# Bar cahrt of the general rating
reviews %>%
  count(rating) %>%
  ggplot(aes(x = factor(rating), y = n, fill = factor(rating))) +
  geom_col(color = "black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer("Blues") +
  labs(x = "Alexa Rating", y = "Number of Ratings") +
  ggtitle("Bar Chart of Alexa Ratings") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold"))


# SENTIMENT ANALYSIS  ----------------------------------------------------------
require(sentimentr)

# Generate a table with the sentiment score for each review.
sentiment_by_review = 
  reviews$verified_reviews %>% get_sentences %>% sentiment_by()

# Add these scores to the table of all reviews
# Make sure that the columns of sentiment_by_review have not already been added to reviews
reviews[,colnames(sentiment_by_review) :=NULL]
# then add them to reviews
reviews_new = cbind(reviews,sentiment_by_review)
view(reviews_new)

# 10 most negatively charged tweets
negative_reviews <- reviews_new[,.(rating, verified_reviews,  word_count, ave_sentiment)][order(ave_sentiment)] %>% head(10)
view(negative_reviews)
summary(reviews_new$word_count)

# 10 most positively charged tweets
positive_reviews <- reviews_new[,.(rating, verified_reviews, word_count, ave_sentiment)][order(-ave_sentiment)] %>% head(10)
view(positive_reviews)

# Average sentiment overall:
hist(reviews_new$ave_sentiment, main = "Average sentiment per review", xlab = "Average sentiment", col = "light blue")
mean(reviews_new$ave_sentiment)

# The 10 most frequent negative terms
require(data.table)
require(magrittr)
require(sentimentr)

reviews_new[,list(verified_reviews),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

# The 10 most frequent positive terms:
reviews_new[,.(verified_reviews),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,positive] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

# Investigation of the word alarm
# Some reviews mentioning the alarm
reviews_new$verified_reviews[sample(which(str_detect(reviews_new$verified_reviews, pattern = "alarm")), size = 5)]

# Most frequent words in the negative reviews mentionning the alarm
neg_rating_reviews <- reviews_new %>%
  filter(rating <= 2) %>%
  unnest_tokens(word, verified_reviews) %>%
  inner_join(get_sentiments(lexicon = "bing")) %>%
  filter(sentiment == "negative") %>%
  count(word)
wordcloud(neg_rating_reviews$word, neg_rating_reviews$n)

# Find associations 
findAssocs(clean_reviews_dtm, terms = c("issues","issue","problems","problem","disappointing","difficult", "frustrating", "limitations"), corlimit = 0.30)	

reviews %>%
  filter(str_detect(verified_reviews, 'trigger|repeats|unsettling'))

reviews %>%
  filter(str_detect(verified_reviews, 'trouble|internet'))

# Brouillon
# Corelation between rating and average sentiment
cor.test(reviews$rating, reviews$ave_sentiment)
summary(reviews_new)
as.Date(reviews$date, "%d-%b-%Y")

# WORDS FREQUENCIES ------------------------------------------------------------- 
#sorting the words by the most frequent words
frequent_words <- rowSums(clean_reviews_m) %>%
  sort(decreasing = TRUE)

#the barchart of the 15 most frequent words
barplot(frequent_words[1:15], las = 2, col='lightblue', main ='Top 15 most frequent words',
        ylab = 'Word frequencies')

#words to remove 
words.to.remove = c(stopwords("english"),'Love',"love","Great‘s","Great!")
dfmat_corp_alexa = reviews$verified_reviews %>% corpus() %>% 
  tokens( what = "word",
          remove_punct = TRUE,
          remove_url=TRUE,
          remove_symbols = TRUE) %>% 
  tokens_remove(words.to.remove) %>% 
  tokens_wordstem(language = "en") %>% 
  dfm()

## Plot the frequencies using ggpot
library(quanteda.textstats) # loads function textstat_frequency to name space
datafreq_alexa = textstat_frequency(dfmat_corp_alexa) %>%
  as.data.table

ggplot(datafreq_alexa[1:20,], aes(x=(feature, -rank), y=frequency)) + 
  geom_col(fill = 'green') +
  coord_flip() +
  theme_minimal()

# sorting words in order
datafreq_alexa[1:7,]

# other way to see stemmed words 
datafreq_alexa_long_top20 = datafreq_alexa[rank <= 20] %>% 
  melt(id.vars = c("feature","group","rank"),
       measure.vars = c("frequency","docfreq")
  )

ggplot(datafreq_alexa_long_top20, aes(x=reorder(feature,-rank), y=value, fill = variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete() + 
  labs(x = "", y = "Occurances", fill = "") +
  coord_flip() +
  theme_minimal()

# Words, 2-grams, 3-grams

install.packages('quanteda.textstats')
library(quanteda.textstats)

tok_tweets = datafreq_alexa$feature %>% 
  gsub("#","", . ) %>% 
  corpus %>% 
  tokens(what="word",
         remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_symbols=TRUE,
         remove_separators=TRUE,
         remove_url=TRUE)

head(tok_tweets,n=2)

stopwords(language = "en")[1:10]

tok_tweets = tokens_remove(tok_tweets,stopwords(language = "en"))
head(tok_tweets,n=2)

TokensStemmed = tokens_remove(tok_tweets, words.to.remove)

datafreq_alexa = dfm(tokens_ngrams(TokensStemmed,n=2))

TokensStemmed = textstat_frequency(dfm2)

ggplot(datafreq_alexa[1:40,], aes(x=reorder(feature, frequency), y=frequency)) + 
  geom_col(fill='orange') +
  coord_flip() +
  scale_x_discrete(name = "2 gram") +
  theme(text=element_text(size=12))










