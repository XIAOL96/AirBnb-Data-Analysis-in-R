library(dplyr)
library(tm)
library(tidytext)
library(stringr)
library(wordcloud)
setwd("~/Downloads")
reviews = read.csv("reviews.csv", header = TRUE, na.strings = c(" ", "", "NA"), stringsAsFactors = FALSE)
listing = read.csv("listings 2.csv", header = TRUE, na.strings = c(" ", "", "NA"), stringsAsFactors = FALSE)
reviews =  reviews %>% left_join(listing, by=c("listing_id"="id")) 

reviews = reviews %>% select(c(6,45))
reviews = reviews %>% group_by(neighbourhood_cleansed)

colSums(is.na(reviews))
reviews = na.omit(reviews)

# Get the most reviewed neighborhoods
most_reviews = 
  reviews %>% group_by(neighbourhood_cleansed) %>% summarise(counts = n()) %>% arrange(desc(counts))%>% top_n(10)

reviews = reviews %>% inner_join(most_reviews) %>% select(c(1,2))

# text mining
neighbourhood_words <- reviews %>% unnest_tokens(word, comments) 
neighbourhood_words = as.data.frame(neighbourhood_words)
stop_words = add_row(stop_words, word = c("apartment", "stay", "chicago", "airbnb"))
neighbourhood_words = neighbourhood_words %>% filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))
# AFINN lexicon, which provides a positivity score for each word, from -5 (most negative) to 5 (most positive). 
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

reviews_sentiment <- neighbourhood_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(sentiment = mean(afinn_score))

# plot word cloud

word.freq = neighbourhood_words %>% group_by(neighbourhood_cleansed, word)  %>% summarise(counts = n()) %>% 
  arrange(desc(counts)) 

word.freq.WT = word.freq %>% filter(neighbourhood_cleansed == 'West Town')
word.freq.LS = word.freq %>% filter(neighbourhood_cleansed == 'Logan Square')
word.freq.LV = word.freq %>% filter(neighbourhood_cleansed == 'Lake View')
word.freq.NNS = word.freq %>% filter(neighbourhood_cleansed == 'Near North Side')
word.freq.LP = word.freq %>% filter(neighbourhood_cleansed == 'Lincoln Park')
word.freq.NWS = word.freq %>% filter(neighbourhood_cleansed == 'Near West Side')
word.freq.L = word.freq %>% filter(neighbourhood_cleansed == 'Loop')


wordcloud(words = word.freq.WT$word, freq = word.freq.WT$counts, min.freq = 1000,
          random.order = F)
wordcloud(words = word.freq.LS$word, freq = word.freq.LS$counts, min.freq = 1000,
          random.order = F)
wordcloud(words = word.freq.LV$word, freq = word.freq.LV$counts, min.freq = 1000,
          random.order = F)
wordcloud(words = word.freq.NNS$word, freq = word.freq.NNS$counts, min.freq = 1000,
          random.order = F)
wordcloud(words = word.freq.LP$word, freq = word.freq.LP$counts, min.freq = 1000,
          random.order = F)
wordcloud(words = word.freq.NWS$word, freq = word.freq.NWS$counts, min.freq = 700,
          random.order = F)
wordcloud(words = word.freq.L$word, freq = word.freq.L$counts, min.freq = 300,
          random.order = F)

