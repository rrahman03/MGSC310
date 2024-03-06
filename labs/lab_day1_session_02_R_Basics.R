#--------------------------------------------------
# Lab Day 1 Session 2: Introduction to R 
#--------------------------------------------------

# this is a comment 


# You can think of R like a fancy calculator!
# executing each of these lines generates output
# to the console (the screen)
1 / 300
3 * 45
4^3
sin(3*pi)

# To create variables and other re-useable objects 
# you can either use "<-" or "="
# by convention R uses "<-" but if you find it weird, you can use "="

x = 1
x <- 2
2 -> x
# assign the value of 1 to the variable x
x <- 1
x = 1

# see the type or class of an object you created
class(x)
str(x)

# There are five basic data types in R: 
# character, double, integer, logical, and factors

x <- 1

### Vectors # 
# vectors are a collection of a single kind of basic type of data
# We can include all strings or all numerics, but not one of each

# create a vector 

c(1, 3, 4, 5, 6, 7, 69393, 59)

my_vec <- c(1, 4, 6, 3, 56)
my_vec
chr_vec <- c("R", "is", "my", "favorite")
chr_vec

# This will give an error
# Make sure you know why ;) 
c("hi", 1)

myVec <- c(5, 3, 7)
print(myVec)
is.vector(myVec)


# We can create a function in the following way
thank_you_next <- function(){
  print("Thank you")
}

# execute the function
thank_you_next()

#--------------------------------------------------
# installing and loading external packages
#--------------------------------------------------
# R has many number of helpful packages created by other users. 
# to use these packages we first need to install them using the command 
# install.packages()

# read the help on install.packages
?install.packages()

# to see where the packages are installed
.libPaths()

# The only argument this function takes is the name of the package 
# we want to install
install.packages('readr')

# Installing the package doesn't load it into the current working session.
# To do that we type library('packagename') or require('packagename')
library('readr')

# see if the package has loaded using the sessionInfo() command
sessionInfo()


#--------------------------------------------------
# File path and directories
#--------------------------------------------------
# download the IMDB movies dataframe from Canvas and move it 
# into the "datasets" folder in your home directory

# What is my current "home" directory?
getwd()

# any command where we read in a file starts at this directory
# if we want to open a file in the "datasets" subdirectory 
# of MGSC_310 we need to reference the datasets folder 
# and then point to the file we want to open
# (To go 'up' a directory we type '../' before the file name)

# To load the IMDB_movies data frame execute the following 
library('readr')
movies <- read_csv("datasets/IMDB_movies.csv")
movies <- read_csv("/Users/raneemrahman/Desktop/MGSC_310/MGSC_310/datasets/IMDB_movies.csv")
