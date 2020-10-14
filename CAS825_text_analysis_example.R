######### CAS 825 - COVID19 project
######### This code is developed to explore and analyze COVID19 Twitter data (Text data) in CAS 825 class

# install packages
#install.packages("RColorBrewer")
#install.packages("topicmodels")
#install.packages("twitteR")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("NLP")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("tidytext")

# load packages
library(RColorBrewer)
library(topicmodels)
library(twitteR)
library(tm)
library(wordcloud)
library(NLP)
library(ggplot2)
library(tidyverse)
library(tidytext)

# setting working directory
setwd("/Users/Ralf/Desktop/sanguk")

# loading the data
covid_df <- read.csv("COVID19_sample.csv")

# check variables
names(covid_df)

# check first a few rows
head(covid_df)

# check data type
str(covid_df)

# select English text only
unique(covid_df$lang)
covid_df_english <- covid_df[which(covid_df$lang == "en"),]
# check if data includes only English texts
unique(covid_df_english$lang)

################# 1. Tokenizing Text 
#covid_df$text <- iconv(covid_df$text, "UTF-8", "ASCII")
tweets_tokens <- covid_df_english %>%
  unnest_tokens(word, text)
names(tweets_tokens)
tweets_tokens$word

# count tokens
tweets_tokens %>%
  count(word) %>%
  arrange(desc(n))

################# 2. Text Preprocessing (stopwords = words are not informative)
# stop_words are pre-loaded from a package
stop_words

# let's remove stopwords using anti_join function
tweets_rm_stop <- tweets_tokens %>%
  anti_join(stop_words)

# count cleaned tokens
tweets_rm_stop %>%
  count(word) %>%
  arrange(desc(n))

# Word tokens seems cleaner than before, but there are still a few words that are not that informative
# let's remove those words by customizing our own word dictionary
# customizing stopwords
custom_stop_words <- tribble(
  ~word, ~lexicon,
  "rt", "CUSTOM",
  "https", "CUSTOM",
  "t.co", "CUSTOM"
)

# integrate customized stopwords into the existing stopwords
stop_words2 <- stop_words %>%
  bind_rows(custom_stop_words)

# let's remove those words using anti_join function again
tweets_rm_stop2 <- tweets_tokens %>%
  anti_join(stop_words2)

# let's see if tokenized words look even cleaner
tweets_rm_stop2 %>%
  count(word) %>%
  arrange(desc(n))

################# 3. Visualization (word count, word cloud)
# 3-1) Visualize word counts
word_counts <- tweets_rm_stop2 %>%
  count(word) %>%
  arrange(desc(n))

word_counts

ggplot(word_counts, aes(x = word, y = n)
) + 
  geom_col()

# Using filtre function to plot only high freequencty words (n > 20)
word_counts2 <- tweets_rm_stop2 %>%
  count(word) %>%
  filter(n > 20) %>%
  arrange(desc(n))

ggplot(word_counts2, aes(x = word, y = n)
) + 
  geom_col()

# flip x and y to make the graph more visible
ggplot(word_counts2, aes(x = word, y = n)
) + 
  geom_col() +
  coord_flip() +
  ggtitle("COVID19 Word Counts")

# ordering by the frequency number
word_counts2 <- tweets_rm_stop2 %>%
  count(word) %>%
  filter(n > 20) %>%
  mutate(word2 = fct_reorder(word, n)) # reorder word according to n (frequency)

ggplot(word_counts2, aes(x = word2, y = n)
) + 
  geom_col() +
  coord_flip() +
  ggtitle("COVID19 Word Counts")

# 3-2) plot wordcloud
wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  min.freq = 10,
  max.word = 200,
  random.order = FALSE,
  rot.per = 0.15,
  colors = brewer.pal(8, "Dark2")
)

################# 4. Topic Modeling

# 4-1) Generate document term matrix
# check the structure of document term matrix
names(tweets_rm_stop2)

tweets_rm_stop2 %>%
  count(word, id) %>%
  cast_dtm(id, word, n)

# Generate document term matrix
dtm1 <- tweets_rm_stop2 %>%
  count(word, id) %>%
  cast_dtm(id, word, n) %>%
  as.matrix()

dtm1[1:5, 1:10]
dtm1[1:5, 10:20]
dtm1[1:5, 1000:1010]

# there are 1233776 elements. This is 
464*2659

# 4-2) Apply LDA Topic Modeling 
# Two topics
LDA_out <- LDA(
  dtm1,
  k = 2,
  method = 'Gibbs',
  control = list(seed =42)
)

# screening LDA output
LDA_out
glimpse(LDA_out)

# preperation for visualization
LDA_topics <- LDA_out %>%
  tidy(matrix = 'beta')

LDA_topics %>%
  arrange(desc(beta))

word_probs <- LDA_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Visualizing two topic model
ggplot(
  word_probs,
  aes(
    term2, 
    beta, 
    fill = as.factor(topic)
  )
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

# Four topics 
LDA_out <- LDA(
  dtm1,
  k = 4,
  method = 'Gibbs',
  control = list(seed =42)
)

# screening LDA output
LDA_out
glimpse(LDA_out)

# preperation for visualization
LDA_topics <- LDA_out %>%
  tidy(matrix = 'beta')

LDA_topics %>%
  arrange(desc(beta))

word_probs <- LDA_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Visualizing four topic model
ggplot(
  word_probs,
  aes(
    term2, 
    beta, 
    fill = as.factor(topic)
  )
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
