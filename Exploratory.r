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
colors <- c('Setosa'='blue', 'Versicolor'='green', 'Virginica'='red')

# *********************
# PART 1: Plots
# *********************

# IRIS Template
png('iris_gray.png')
plot(iris)
dev.off()

# Plot sepal lengths
png('old_sepal_length.png')
plot(setosa.sepal.length, col=colors['Setosa'], xlab='', ylab='Length (cm)', ylim=c(4.1, 8.1))
points(versicolor.sepal.length, col=colors['Versicolor'])
points(virginica.sepal.length, col=colors['Virginica'])
legend('bottomright', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c(colors['Setosa'], colors['Versicolor'], colors['Virginica']), lty = 1:2, cex = 0.8) 
title('Sepal Lengths', line=1)
dev.off()

# plot sepal widths
png('old_sepal_width.png')
plot(setosa.sepal.width, col=colors['Setosa'], xlab='', ylab='Width (cm)', ylim=c(1.8, 4.6))
points(versicolor.sepal.width, col=colors['Versicolor'])
points(virginica.sepal.width, col=colors['Virginica'])
legend('bottomright', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c(colors['Setosa'], colors['Versicolor'], colors['Virginica']), lty = 1:2, cex = 0.8) 
title('Sepal Widths', line=1)
dev.off()

# Plot petal lengths
png('old_petal_length.png')
plot(setosa.petal.length, col=colors['Setosa'], xlab='', ylab='Length (cm)', ylim=c(0.8, 7.1))
points(versicolor.petal.length, col=colors['Versicolor'])
points(virginica.petal.length, col=colors['Virginica'])
legend('bottomright', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c(colors['Setosa'], colors['Versicolor'], colors['Virginica']), lty = 1:2, cex = 0.8) 
title('Petal Lengths', line=1)
dev.off()

# plot petal widths
png('old_petal_width.png')
plot(setosa.petal.width, col=colors['Setosa'], xlab='', ylab='Width (cm)', ylim=c(-0.1, 2.7))
points(versicolor.petal.width, col=colors['Versicolor'])
points(virginica.petal.width, col=colors['Virginica'])
legend('bottomright', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c(colors['Setosa'], colors['Versicolor'], colors['Virginica']), lty = 1:2, cex = 0.8) 
title('Petal Widths', line=1)
dev.off()


# IRIS Colors
png('iris_colored.png', width = 800, height = 640)
#title()
par(mfrow=c(4, 5), mar=c(2, 2, 2, 2))
plot(x = 0:10, y = 0:10, ann=F, bty='o', type='n', xaxt='n', yaxt='n')
text(x = 5, y = 5, 'Sepal Length', cex=2)
par(mar=c(2, 2, 2, 2))
plot(iris$sepal.width, iris$sepal.length, col=colors[iris$species])
plot(iris$petal.length, iris$sepal.length, col=colors[iris$species])
plot(iris$petal.width, iris$sepal.length, col=colors[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='n', type='n', xaxt='n', yaxt='n')

plot(iris$sepal.length, iris$sepal.width, col=colors[iris$species])
#par(mar=c(0.5, 0.5, 0.5, 0.5))
plot(x = 0:10, y = 0:10, ann=F, bty='o', type='n', xaxt='n', yaxt='n')
text(x = 5, y = 5, 'Sepal Width', cex=2)
#par(mar=c(2, 2, 2, 2))
plot(iris$petal.length, iris$sepal.width, col=colors[iris$species])
plot(iris$petal.width, iris$sepal.width, col=colors[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='n', type='n', xaxt='n', yaxt='n')

plot(iris$sepal.length, iris$petal.length, col=colors[iris$species])
plot(iris$sepal.width, iris$petal.length, col=colors[iris$species])
#par(mar=c(0, 0, 0, 0))
plot(x = 0:10, y = 0:10, ann=F, bty='o', type='n', xaxt='n', yaxt='n')
text(x = 5, y = 5, 'Petal Length', cex=2)
par(mar=c(2, 2, 2, 2))
plot(iris$petal.width, iris$petal.length, col=colors[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='n', type='n', xaxt='n', yaxt='n')

plot(iris$sepal.length, iris$petal.width, col=colors[iris$species])
plot(iris$sepal.width, iris$petal.width, col=colors[iris$species])
plot(iris$petal.length, iris$petal.width, col=colors[iris$species])
#par(mar=c(0, 0, 0, 0))
plot(x = 0:10, y = 0:10, ann=F, bty='o', type='n', xaxt='n', yaxt='n')
text(x = 5, y = 5, 'Petal Width', cex=2)
par(mar=c(2, 2, 2, 2))
plot(x = 0:10, y = 0:10, ann=F, bty='n', type='n', xaxt='n', yaxt='n')
legend('bottom', legend=c('Setosa', 'Versicolor', 'Virginica'), col=c(colors['Setosa'], colors['Versicolor'], colors['Virginica']), lty = 1:2, cex = 2)
dev.off()


# *********************
# PART 2: Boxplots
# *********************

# Sepal Length
png('box_sepal_length.png')
boxplot(sepal.length ~ species, data=iris, col=colors,
        main='Boxplot of Sepal Lengths', xlab='Species', ylab='Sepal Length (cm)')
dev.off()


# Sepal Width
png('box_sepal_width.png')
boxplot(sepal.width ~ species, data=iris, col=colors,
        main='Boxplot of Sepal Widths',
        xlab='Species', ylab='Sepal Width (cm)')
dev.off()

# Petal Length
png('box_petal_length.png')
boxplot(petal.length ~ species, data=iris, col=colors,
        main='Boxplot of Petal Lengths',
        xlab='Species', ylab='Petal Length (cm)')
dev.off()

# Petal Width
png('box_petal_width.png')
boxplot(petal.width ~ species, data=iris, col=colors,
        main='Boxplot of Petal Widths',
        xlab='Species', ylab='Petal Width (cm)')
dev.off()