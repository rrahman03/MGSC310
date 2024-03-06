# ------------------------------------------------
# Loading data 
# ------------------------------------------------
library('dplyr')
library('readr')

movies <- read_csv("datasets/IMDB_movies.csv")

# ------------------------------------------------
# Data frame basics
# ------------------------------------------------

# see metadata associated with data frame 
attributes(movies)

# column or variable names
names(movies)

# Inspect/view the raw datafile
View(movies)

# number of rows
nrow(movies)

# see number of columns
ncol(movies)

# see both dimensions
dim(movies)

# "$" operator to select a column
movies$movie_title


# ------------------------------------------------
# GLIMPSE to summarize data
# ------------------------------------------------
# let's summarize the data using the glimpse function
glimpse(movies)


# ------------------------------------------------
# Pipe Operator!  
# ------------------------------------------------
# The pipe operator "%>%" is super useful!
# It allows us to execute a series of functions on an object in stages
# The general recipe is Data_Frame %>% function1() %>% function2() etc
# Functions are applied right to left

movies %>% glimpse() %>% write_csv('movies.csv')
# write_csv(glimpse(movies))
# command shift m %>% %>% %>% %>% %>% %>% 

# cmd/ctrl + shift as a shortcut create the pipe operator 





# ------------------------------------------------
# Slice function: to select ROWS 
# ------------------------------------------------
# SLICE: slice to view only the first 10 rows
movies %>% slice(1:10)

# SLICE to view only rows 300 to 310 
movies %>% slice(300:310)

# movies %>% slice(1)

# ------------------------------------------------
# Arrange function: to ORDER dataset
# ------------------------------------------------
movies %>% arrange(-budget)
movies %>% arrange(desc(budget))

# arrange the dataframe in descening order by budget, and store this back as movies
movies <- movies %>% arrange(desc(budget))

# arrange via multiple columns, by budget and title year, then output rows 1 to 10
movies %>% 
  arrange(desc(country), desc(budget)) %>% 
  slice(1:10)

movies_new =
movies %>% 
  arrange(-budget) #-> movies_new

# ------------------------------------------------
# SELECT columns of the dataset using the 'select' function
# ------------------------------------------------
# selecting columns using the select() function
# here we create a subset of the original dataset that only contains 
# director_name and movie title
movies_keys <- movies %>%  select(director_name, movie_title)
glimpse(movies_keys)

movies %>% sekect(-director_name, -country) %>%  glimpse()
# using select to programmatically select several variables that 
# 'start with' a certain string
movies_actors <- movies %>% select(starts_with("actor"))
glimpse(movies_actors)

movies %>% select(ends_with("reviews")) %>% names()
# movies %>% select(-starts_with("reviews")) %>% names()

# ------------------------------------------------
# RENAME variables using the RENAME function
# ------------------------------------------------

# use the rename function to rename variables
movies <- 
  movies %>%  
  rename(director = director_name)
  # syntax is new name = old name
  # if you want to change it back: (director_name = director)

glimpse(movies)

# ------------------------------------------------
# FILTER and ONLY allow certain rows using the FILTER function
# ------------------------------------------------
# filter removes any rows that DO NOT meet the logical operator


# ONLY select large budget movies and store this as a new data frame
movies_big <- movies %>% filter(budget > 100000000)
glimpse(movies_big)
# filter command is a logical condition


# ONLY select english language films and store this as a new data frame
movies_eng <- movies %>% filter(language == "English")
glimpse(movies_eng)
dim(movies_eng)


# select both ENGLIGH and SPANISH films
movies_lang <- movies %>% filter(language == "Hindi" | language == "Spanish")
dim(moviesSub)


# ------------------------------------------------
# MUTATE to Transform variables in your dataset
# ------------------------------------------------

# adding new variables using mutate()
# let's create new varibles log budget and log gross that are
# budget and gross transformed by logarithm
movies <- 
  movies %>% 
  mutate(log_budget = log(budget),
         log_gross = log(gross)
         budget_sq = budget * budget,
         # var_true = 1,
         # big_budget = if_else(budget > 100000000, 1, 0))
        # big_budget = if_else(budget > 100000000, "big boi", "small boi"))

movies %>% slice(100) %>% glimpse()


# ------------------------------------------------
# Lab exercises - to upload 
# ------------------------------------------------

# 1. What are the highest grossing David Fincher films?
         Gone Girl

# 2. Print a dataframe that only lists the films with the highest 10 budgets, 
#    the movie title, and the country of origin (hint, use select).
         
         movies <- %>% movies arrange(desc(budget))
         top_10 <- movies %>% slice(1:10) %>% select(movie_title, country, budget)
         print(top_10)
         

# 3. How many "PG-13" movies are there in the database? (hint: use nrow())
         movies <- movies %>% arrange(rating)
         pg13_count <- nrow(filter(movies, rating == "PG-13"))
         print(pg13_count)


# 4. Change the name of the variable “content_rating” to “rating”
         movies <- 
           movies %>%  
           rename(rating = content_rating)


# 5. Create a new dataframe called “movies_actors” that contains all the actor variables, 
#    and the movie title. (hint use select(starts_with(…))
         
         movies_actors <- movies %>% select(movie_title, starts_with("actor"))
         glimpse(movies_actors)





