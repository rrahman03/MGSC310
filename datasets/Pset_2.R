# QUESTION ONE:

# Load the IMDB movies dataset using the function read_csv() 
# (Note, you will need to load the package readr to load the package.)

library (readr)
library(dplyr)

setwd("/Users/raneemrahman/Desktop/MGSC_310/datasets")
movies <- read_csv("IMDB_movies.csv")

# Use the select() function to remove the variable plot_keywords. 
# Store the smaller dataset as movies_sub. 
# Run the dimension command to ensure that movies_sub 
# has one fewer variable than movies.

movies_sub <- select(movies, -plot_keywords)
dim(movies_sub)

# Use the filter command (using the original movies dataset) 
# to see how many movies in the dataset had a budget in excess of $300M USD
high_budget <- filter(movies, budget >300000000)
n_high_budget <- nrow(high_budget)
print(n_high_budget)

# Use the mutate command to create two new variables, 
# budgetM and grossM that report budget and gross in millions of USD. 
# (E.g., if budget equals 3500000, budgetM would equal 3.5.) 
# Store this as movies_clean.

movies_clean <- mutate(movies, budgetM = budget /1000000, grossM = gross/1000000)
# ^^ IDK IF I DID THIS RIGHT I HAD CHAT GPT HELP ME

# Use the glimpse command over the movies_clean dataset to ensure
# the variables have been transformed appropriately.
glimpse(movies_clean)

#QUESTION TWO
# Use the package ggplot2 to create a scatter plot of IMDB score on the 
# x axis and movie gross on the y-axis. 
# (Use the movies_clean dataset.)

library(ggplot2)

ggplot(movies_clean, aes(x = imdb_score, y = grossM)) +
  geom_point() +
  labs (x = "IMDB Score", y = "Movie Gross (in millions)")

# This looks okay, but there are so many points
# it is hard to see how much underlying data each point represents. 
# Use the option alpha = 1/10 within the function geom_point() 
# to reduce the transparency of each data point.

ggplot(movies_clean, aes(x = imdb_score, y = grossM)) +
  geom_point(alpha = 1/10) +
  labs (x = "IMDB Score", y = "Movie Gross (in millions)")

# Create a scatter plot of imdb_score against grossM
# and use the geom_smooth function to make a smoothing line.
# Is there a relationship between movie gross and IMDB score?

ggplot(data = movies_clean, aes(x=imdb_score, y = grossM))+
  geom_point(alpha = 1/10) +
  geom_smooth() +
  labs (x = "IMDB Score", y = "Movie Gross (in millions)")

# Use the filter command to only include movies by the director Martin Scorsese.
# Plot these movies as points using the geom_point() function.
# Change the shape of the points to any shape other than the default shape.

specific_director <- filter(movies_clean, director_name == "Martin Scorsese")
ggplot(specific_director, aes(x = imdb_score, y = grossM)) +
  geom_point(size = 3, shape = 15) +  
  labs(x = "IMDB Score", y = "Movie Gross (in millions USD)")

# Install the package ggrepel and load it using the library function.
# Add to the plot geom_text_repel(aes(label = movie_title))
# to add labels to the scatter plot showing the names of the movie titles.

install.packages("ggrepel")
library(ggrepel)

specific_director <- filter(movies_clean, director_name == "Martin Scorsese")
ggplot(specific_director, aes(x = imdb_score, y = grossM)) +
  geom_point(size = 3, shape = 15) + 
  geom_text_repel(aes(label = movie_title)) +
  labs(x = "IMDB Score", y = "Movie Gross (in millions USD)")

# Add x and y axes titles using the function labs.
# Also add the option theme(text = element_text(size = 16))
# to increase the base font size of the plot.

specific_director <- filter(movies_clean, director_name == "Martin Scorsese")
ggplot(specific_director, aes(x = imdb_score, y = grossM)) +
  geom_point(size = 3, shape = 15) + 
  geom_text_repel(aes(label = movie_title)) +
  labs(x = "IMDB Score", y = "Movie Gross (in millions USD)") + 
  theme(text = element_text(size = 16))

