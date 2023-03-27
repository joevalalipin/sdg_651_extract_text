library(wordcloud)
library(tm)
library(SentimentAnalysis)
library(syuzhet)
library(tidytext)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr) 

load("txt_full.rdata")

files <- list.files("word_files/")[c(1,2,6,8,9,10,11,12,14,16)]

txt_full <- txt_full %>% 
  filter(file %in% files)

# clean data and produce document term matrix
txt_full_shrink <- txt_full %>% 
  mutate(q_num = gsub("[a-z]", "", q_num)) %>% 
  group_by(q_num) %>% 
  summarise(status_description = paste(status_description, collapse = " "),
            way_forward = paste(way_forward, collapse = " ")) %>% 
  mutate(status_description = str_remove_all(status_description, "Status description:"),
         status_description = tolower(status_description),
         status_description = removePunctuation(status_description),
         status_description = removeNumbers(status_description),
         status_description = removeWords(status_description, stopwords("english")),
         status_description = stemDocument(status_description),
         status_description = str_replace_all(status_description, "\\s+", " "))

corpus <- SimpleCorpus(VectorSource(txt_full_shrink$status_description))
corpus[[1]]$content

dtm <- DocumentTermMatrix(corpus)
inspect(dtm)

# simple word cloud

# dtm_df <- as_tibble(as.matrix(dtm)) %>% 
#   bind_cols(txt_full_shrink %>% select(q_num)) %>% 
#   select(q_num, everything()) %>% 
#   gather(key = "word", value = "freq", 2:ncol(.)) 

dtm_df <- txt_full_shrink %>%
  select(-way_forward) %>% 
  mutate(status_description = strsplit(status_description, " ")) %>% 
  unnest(status_description) %>% 
  count(status_description, q_num) %>% 
  bind_tf_idf(status_description, q_num, n)

# tf is n / sum(n) in each q_num
# idf is ln(number of unique q_num / number of unique q_num containing that word)

wordcloud(words = dtm_df %>% filter(q_num == "1.1") %>% pull(status_description),
          freq = dtm_df %>% filter(q_num == "1.1") %>% pull(n),
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

# word cloud filtering our extremely common words
wordcloud(words = dtm_df %>% filter(q_num == "1.1", tf_idf > 0) %>% pull(status_description),
          freq = dtm_df %>% filter(q_num == "1.1", tf_idf > 0) %>% pull(n),
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))

# sentiment analysis
sentiment <- analyzeSentiment(dtm) %>% 
  bind_cols(txt_full_shrink %>% select(q_num))

ggplot(sentiment,
       aes(x = q_num, y = SentimentGI)) +
  geom_col()

emotions <- get_nrc_sentiment(txt_full_shrink$status_description) %>% 
  bind_cols(txt_full_shrink %>% select(q_num)) %>% 
  select(q_num, everything()) %>% 
  gather(key = "emotion", value = "count", 2:ncol(.))
  
ggplot(emotions,
       aes(x = emotion, y = count)) +
  geom_col() +
  facet_wrap(~ q_num, ncol = 2) +
  theme(axis.text.x = element_text(angle = 90))

