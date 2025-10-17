#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

irisv <- subset(iris, Species == "versicolor")

list_results <- list()
regression_variables <- c(irisv$Sepal.Length ~ irisv$Sepal.Width, irisv$Petal.Length ~ irisv$Petal.Width, irisv$Petal.Length ~ irisv$Sepal.Length)

for (match in 1:length(regression_variables)) {
  fit <- lm(regression_variables[[match]])
  list_results[[match]] <- fit
  
}

head(list_results)

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

new <- dplyr::left_join(iris, height, by=NULL, copy=FALSE, 
          suffix=c(".x",".y"), keep=FALSE, na_matches="na")


#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(shape = 1, size = 2) + theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
ggplot(iris, aes(Sepal.Length, Sepal.Width, size = Petal.Length, color = Species)) +
  geom_point() + theme_classic() + 
  labs(x = "Sepal Length", y = "Sepal Width", title = "Sepal Lengths and Widths")


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

#Both systems have different syntax. Ggplot2, being a part of the tidyverse suite, has different commands from base R.
#     I like the + feature within ggplot2, as it allows for the continuous addition of features line-by-line. Such a feature
#     makes it easier to actively develop a map, rather than having to have everything planned when executing a big batch of code.

