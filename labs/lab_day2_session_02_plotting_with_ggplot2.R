# ------------------------------------------------
# plotting with ggplot2
# ------------------------------------------------
# install.packages('ggplot2')
library('ggplot2')
data(mpg)

# layers 1-2-3
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point()


ggplot(data = mpg, aes(x = displ, y = hwy, shape = factor(cyl))) + 
  geom_point()

# add layer 4: facets
# add factor for class
ggplot(data = mpg, aes(x = displ, y = hwy, color = factor(cyl))) + 
  geom_point() +
  facet_wrap(~ class)

# layer 5 statistics
ggplot(data = mpg, aes(x = cyl, y = hwy)) + 
  geom_point() + 
  stat_summary(fun = "median", color = "red", size = 1) 

# layer 6 coordinates
  # layer 5 statistics
  ggplot(data = mpg, aes(x = cyl, y = hwy)) + 
  geom_point() + 
  stat_summary(fun = "median", color = "red", size = 1) +
  coord_flip()

  ggplot(data = mpg, aes(x = model)) +
    geom_histogram(stat = "count") +
    coord_flip()
  
  # library('forcats')
  # ggplot(data = mph.aes(x = fct_lump_n(model))) +
    # geom_histogram(stat = "count") +
    # coord_flip()

# layer 7 themes
ggplot(data = mpg, aes(x = cyl, y = hwy)) + 
  geom_point() + 
  stat_summary(fun = "median", color = "red", size = 1) +
  theme_bw()
# you can switch between different themes with your plot

# add axes titles and change font size 
ggplot(data = mpg, aes(x = cyl, y = hwy)) + 
  geom_point() + 
  stat_summary(fun = "median", color = "red", size = 1) +
  theme_bw(base_size = 16) + 
  labs(x = "highway", y = "cylinder")
  

# one way densities
ggplot(data = mpg, aes(x = hwy)) + geom_histogram()


# -------------------------------------
# Lab to upload
# -------------------------------------


# 1. Produce a scatter plot with highway mile per gallon (hwy) on the 
#    x axis and city mile per gallon on the y axis (cty)

ggplot(data = mpg, aes(x = hwy, y = cty)) + 
  geom_point() +
  labs(x = "Highway MPG", y = "City MPG")

# 2. Identify the three elements of the plot -- data, aesthetic and geometry
data = ggplot()
aesthetic = aes()
geometry = geom_point()

# 3. Add a facet to produce subplots by class of the car
ggplot(data = mpg, aes(x = displ, y = hwy, color = factor(cyl))) + 
  geom_point() +
  facet_wrap(~ class)

# 4. Add a fitted line statistic element using the function 'geom_smooth()' 
ggplot(data = mpg, aes(x = displ, y = hwy, color = factor(cyl))) + 
  geom_point() +
  facet_wrap(~ class) +
  geom_smooth()

# 5. add a dark theme using the "theme_dark()" option 
ggplot(data = mpg, aes(x = displ, y = hwy, color = factor(cyl))) + 
  geom_point() +
  facet_wrap(~ class) +
  geom_smooth() +
  theme_dark()

# 6. Change the base text size to 16 and add informative axes labels using the "labs" command
ggplot(data = mpg, aes(x = cyl, y = hwy)) + 
  geom_point() + 
  facet_wrap(~ class) +
  geom_smooth() +
  theme_dark(base_size = 16) + 
  labs(x = "Highway MPG", y = "City MPG")
