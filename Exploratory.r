# Load packages
library(dplyr)

# Load data
data(iris)
summary(iris)

# Split dataset into different species
names(iris) <- tolower(names(iris))

# Colors:
# Setosa  Versicolor  Virginica
# Blue    Green       Red
# Shapes:
# Circle  Triangle    +
colors <- c('Setosa'='blue', 'Versicolor'='green', 'Virginica'='red')
shapes <- c('Setosa'=1,      'Versicolor'=2,       'Virginica'=3)

# *********************
# PART 1: Plots
# *********************

# IRIS Template
png('iris_gray.png')
plot(iris)
dev.off()

## IRIS Colors
png('iris_colored.png', width = 800, height = 640)
#title()
par(mfrow=c(4, 5), mar=c(2, 2, 2, 2))
plot(x = 0:10, y = 0:10, ann=F, bty='o', type='n', xaxt='n', yaxt='n')
text(x = 5, y = 5, 'Petal Length', cex=2)
plot(iris$petal.width, iris$petal.length, col=colors[iris$species], pch=shapes[iris$species])
plot(iris$sepal.length, iris$petal.length, col=colors[iris$species], pch=shapes[iris$species])
plot(iris$sepal.width, iris$petal.length, col=colors[iris$species], pch=shapes[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='n', type='n', xaxt='n', yaxt='n')

plot(iris$petal.length, iris$petal.width, col=colors[iris$species], pch=shapes[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='o', type='n', xaxt='n', yaxt='n')
text(x = 5, y = 5, 'Petal Width', cex=2)
plot(iris$sepal.length, iris$petal.width, col=colors[iris$species], pch=shapes[iris$species])
plot(iris$sepal.width, iris$petal.width, col=colors[iris$species], pch=shapes[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='n', type='n', xaxt='n', yaxt='n')


plot(iris$petal.length, iris$sepal.length, col=colors[iris$species], pch=shapes[iris$species])
plot(iris$petal.width, iris$sepal.length, col=colors[iris$species], pch=shapes[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='o', type='n', xaxt='n', yaxt='n')
text(x = 5, y = 5, 'Sepal Length', cex=2)
plot(iris$sepal.width, iris$sepal.length, col=colors[iris$species], pch=shapes[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='n', type='n', xaxt='n', yaxt='n')

plot(iris$petal.length, iris$sepal.width, col=colors[iris$species], pch=shapes[iris$species])
plot(iris$petal.width, iris$sepal.width, col=colors[iris$species], pch=shapes[iris$species])
plot(iris$sepal.length, iris$sepal.width, col=colors[iris$species], pch=shapes[iris$species])
plot(x = 0:10, y = 0:10, ann=F, bty='o', type='n', xaxt='n', yaxt='n')
text(x = 5, y = 5, 'Sepal Width', cex=2)
plot(x = 0:10, y = 0:10, ann=F, bty='n', type='n', xaxt='n', yaxt='n')

legend('bottom', legend=c('Setosa', 'Versicolor', 'Virginica'), col=colors, pch=shapes, cex = 2)
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