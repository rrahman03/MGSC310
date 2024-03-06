# ------------------------------------------------
# Lab exercises - to upload 
# ------------------------------------------------

# 1. Download the RMarkdown problem set template in the assignments folder 


# 2. Open the document and change the title to "Lab Day 1 Session 3" and the author 
#    with your name


# 3. "Knit" or compile the document to an html file


# 4. Upload the knitted or compiled HTML file to the lab portion of canvas 



# ------------------------------------------------
# Reading Data
# ------------------------------------------------
# install the package readr if necessary using install.packages('readr')
# load the package 'readr' into memory
library('readr')

# download the csv file IMDB_movies from the datasets portion of canvas

# read the csv file into memory using the read_csv function and
# store it as the object movies_df
movies_df <- read_csv("datasets/IMDB_movies.csv")

# view the variable types the columns were converted into 
spec(movies_df)
