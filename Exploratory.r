# Load packages
library(dplyr)

# Load data
data(iris)
summary(iris)

# Split dataset into different species
#setosa, virginica, versicolor
names(iris) <- tolower(names(iris))
setosa <- filter(iris, species == 'setosa')
virginica <- filter(iris, species == 'virginica')
versicolor <- filter(iris, species == 'versicolor')

plot(setosa)
plot(setosa$petal.length, setosa$petal.width)
