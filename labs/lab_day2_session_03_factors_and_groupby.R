#-------------------------------------------------- 
# Factors
#--------------------------------------------------
# see more on forcats https://forcats.tidyverse.org/

library('dplyr')
library('readr')

# install.packages('forcats')
library('forcats')

movies <- read_csv("datasets/IMDB_movies.csv")

# Turn content rating into a factor
movies <- 
  movies %>% 
  mutate(content_rating = 
           as_factor(content_rating))

class(movies$content_rating)

# see the levels of the factors
levels(movies$content_rating)

# generate a frequency table of the factor levels
table(movies$content_rating)

# manually relabel some factor levels
movies <- 
  movies %>%  
  mutate(rating_simple = 
           fct_recode(content_rating,  
                      "Other" = "Approved",
                      "Other" = "NC-17",
                      "Other" = "X",
                      "Other" = "M",
                      "Other" = "GP",
                      "Other" = "Passed"))

table(movies$rating_simple)

# use fct_lump_n to replace infrequent factor levels as "other"
movies <- 
  movies %>% 
  # only keep top 4 levels as explicit
  mutate(rating_simple = 
           fct_lump_n(content_rating, n = 4))

table(movies$rating_simple)

# use fct_infreq to re-order factors by frequency of occurence
movies <- 
  movies %>% 
  # only keep top 4 levels as explicit
  mutate(content_rating2 = 
           fct_infreq(content_rating))

ggplot(movies, aes(x = content_rating)) + 
  geom_histogram(stat = "count") +
  coord_flip()


# relevel factors by sorting along another variable
movies %>% 
  mutate(rating_simple2 = 
           fct_reorder(rating_simple, gross)) %>% 
  ggplot(aes())
ggplot(movies, 
       aes(x = fct_reorder(rating_simple, gross),
           y = gross)) + 
  geom_bar(gross ~ rating_simple)
  

#-------------------------------------------------- 
# Factor Lab 
#-------------------------------------------------- 
# 1. Which variables in the movies dataset might we want to convert to factors?
content_rating
language
country
color

# 2. Use the table command against the language variable
table(movies$language)

# 3. Use the as_factor function inside of a mutate statement to create a new 
#    variable language_factor 

movies <- 
  movies %>% 
  mutate(language_factor = 
           as_factor(language))

class(movies$content_rating)

# 4. Use the fct_lump_n() function to create a variable "language_simple" that 
#    creates 6 explicit language levels and the rest labeled as "Other".

movies <-
  movies %>% 
  mutate(language_simple
         = fct_lump_n(language_factor, n = 6))
table(movies$language_simple)

#-------------------------------------------------- 
# Group by and summarize
#-------------------------------------------------- 
# group movies by color and then count the groups
movies %>% 
  group_by(color) %>% 
  count()

# create a bar graph of the above
movies %>% 
  group_by(color) %>% 
  count %>% 
  ggplot(aes(x = color, y = n)) + 
  geom_bar(stat = "identity")

# groupby and arrange to see the frequency table of movies by country
movies %>% 
  group_by(country) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  print(n = 10)

# group by and summarize to create statistics by group
movies_country <- 
  movies %>% 
  group_by(country) %>% 
  summarize(avg_gross = mean(gross), 
            num = n(),
            max_gross = max(gross))

# print out countries with most number of films in the top 5000 IMDB list
movies_country %>% 
  arrange(desc(num))

#-------------------------------------------------- 
# Lab - Group by and summarize 
#-------------------------------------------------- 

# 1. Group movies by title year and print out the number of movies by year
#    (since 2000)

movies %>% 
  filter(title_year >= 2000) %>% 
  group_by(title_year) %>% 
  count()


# 2. Which year produced the most movies in the top 5000 IMDB list?
movies_year <- movies_title_year %>% 
  group_by(title_year) %>% 
  arrange(desc(num))

# 3. Use group_by and summarize to create a dataset that calculate the count 
#    and average gross by director
movies_director <- 
  movies %>% 
  group_by(director) %>% 
  summarize(avg_gross = mean(gross), 
            num = n(),
            max_gross = max(gross))

# 4. Which director has the highest average grossing movies in the dataset?
highest_director <- movies_director %>% 
  filter(avg_gross == max(avg.gross))

# 5. Which director has the most movies in the dataset? 
movies_director %>% 
  arrange(desc(num))



