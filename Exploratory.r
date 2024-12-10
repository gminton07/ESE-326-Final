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

# Split species into petal & sepal length/width
setosa.sepal.length = setosa$sepal.length
setosa.sepal.width = setosa$sepal.width
setosa.petal.length = setosa$petal.length
setosa.petal.width = setosa$petal.width
versicolor.sepal.length = versicolor$sepal.length
versicolor.sepal.width = versicolor$sepal.width
versicolor.petal.length = versicolor$petal.length
versicolor.petal.width = versicolor$petal.width
virginica.sepal.length = virginica$sepal.length
virginica.sepal.width = virginica$sepal.width
virginica.petal.length = virginica$petal.length
virginica.petal.width = virginica$petal.width

#plot(setosa)
#plot(setosa$petal.length, setosa$petal.width)
# Colors:
# Setosa  Versicolor  Virginica
# Blue    Green       Red

# PLOTS:
# Plot sepal lengths
png('sepal_length.png')
plot(setosa.sepal.length, col='blue', xlab='', ylab='Length (cm)', ylim=c(4.1, 8.1))
points(versicolor.sepal.length, col='green')
points(virginica.sepal.length, col='red')
legend('bottomright', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c('blue', 'green', 'red'), lty = 1:2, cex = 0.8) 
title('Sepal Lengths', line=1)
dev.off()

# plot sepal widths
png('sepal_width.png')
plot(setosa.sepal.width, col='black', xlab='', ylab='Width (cm)', ylim=c(1.8, 4.6))
points(versicolor.sepal.width, col='red')
points(virginica.sepal.width, col='blue')
legend('bottomright', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c('black', 'red', 'blue'), lty = 1:2, cex = 0.8) 
title('Sepal Widths', line=1)
dev.off()

# Plot petal lengths
png('petal_length.png')
plot(setosa.petal.length, col='black', xlab='', ylab='Length (cm)', ylim=c(0.8, 7.1))
points(versicolor.petal.length, col='red')
points(virginica.petal.length, col='blue')
legend('bottomright', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c('black', 'red', 'blue'), lty = 1:2, cex = 0.8) 
title('Petal Lengths', line=1)
dev.off()

# plot petal widths
png('petal_width.png')
plot(setosa.petal.width, col='black', xlab='', ylab='Width (cm)', ylim=c(-0.1, 2.7))
points(versicolor.petal.width, col='red')
points(virginica.petal.width, col='blue')
legend('bottomright', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c('black', 'red', 'blue'), lty = 1:2, cex = 0.8) 
title('Petal Widths', line=1)
dev.off()