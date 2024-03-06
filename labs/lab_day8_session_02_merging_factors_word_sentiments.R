#-------------------------------------------------- 
# Merging data frames
#--------------------------------------------------
library(tidyverse)

artist_DF <- data.frame(
  artist_name = c("Justin Bieber",
             "Drake",
             "The Smiths"),
  artist_ID = 1:3
)

album_DF <- data.frame(
  album_name = c("Thank Me Later", "Take Care",
                 "Nothing Was The Same", "Views",
                 "Scorpion", 
                 "Louder Than Bombs",
                 "Thank U Next"),
  album_ID = 1:7,
  artist_ID = c(rep(2,5),3,5)
)

library('dplyr')

DF <- full_join(x = artist_DF, 
                 y = album_DF,
                 by = "artist_ID")

DF

#right_join()

DF <- anti_join(x = artist_DF,
                 y = album_DF, 
                 by = "artist_ID")

# inner join
merged_DF <- inner_join(x = artist_DF,
                 y = album_DF,
                 by = "artist_ID")

merged_DF

artist_DF <- artist_DF %>% rename(`artist_ID` = 2)

 # full join
merged_DF <- 
  full_join(x = artist_DF,
            y = album_DF,
            by = "artist_ID")

merged_DF

# left join
merged_DF <- 
  left_join(x = artist_DF,
            y = album_DF,
            by = "artist_ID")

merged_DF

# right join
merged_DF <- 
  right_join(x = artist_DF,
            y = album_DF,
            by = "artist_ID")

merged_DF


#-------------------------------------------------------------------------------
# Text Sentiment
#-------------------------------------------------------------------------------
library('devtools')
devtools::install_github("trinker/sentimentr")


# average sentiment score
library('sentimentr')
library('tidyverse')

sentiment_by("I am very scared of the dark")

sentiment('I was very scared of the dark. 
          I am very extremely overjoyed with this joyous product on my birthday the best')


"I am very scared of the dark" %>% 
  extract_sentiment_terms()

"I am very extremely overjoyed with this joyous product on my birthday the best" %>% 
extract_emotion_terms()

"Now I glow at night and life is amazing" %>% 
  extract_sentiment_terms()

"and I was like baby, baby, baby oh" %>% 
  extract_sentiment_terms()

# more examples here: https://www.tidytextmining.com/usenet.html?q=sentiment#sentiment-analysis


#-------------------------------------------------------------------------------
# Lab (Time Permitting)
#-------------------------------------------------------------------------------

# We will be following along with Chapter 2 of the Book 
# Text Mining with R: https://www.tidytextmining.com/sentiment.html

# 1. Load the following libraries
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)

# 2. Create a tibble (fancy data frame) where each row is a different 
#    word of a Jane Austen novel

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


# 3. Calculate word sentiments for each line of Jane Austen's novels 
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


# 4. plot the sentiment for each line of Jane Austen's novels
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# 5. Calculate word clouds for all the words
library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


# 6. Create a word cloud colored by sentiment 
library(reshape2)
# create a word cloud
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)


# 7. What are the most common positive and negative sentiment words? 
# miss
# well
# good
# great

# 8. Can you create any interesting plots using this data? 
# a histogram could create interesting plots

