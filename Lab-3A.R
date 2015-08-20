load(url("http://www.openintro.org/stat/data/ames.RData"))
names(ames)

area <- ames$Gr.Liv.Area
price <- ames$SalePrice

summary(area)
hist(area, breaks = 10)

samp0 <- sample(area, 50)
samp1 <- sample(area, 50)
mean(samp1)
mean(samp0)
mean(sample(area, 100))
mean(sample(area, 1000))

#take 5000 samples of size 50 and store the mean of each sample
sample_means50 <- rep(NA, 5000) #initializes a vector with 5000 NA entries
for (i in 1:5000) {
  samp <- sample(area, 50)
  sample_means50[i] <- mean(samp)
}
hist(sample_means50, breaks = 25) #near normal distribution
mean(sample_means50)

#take 100 samples of size 50 and store the mean of each sample
sample_means_small<- rep(NA, 100)
for (i in 1:100) {
  samp <- sample(area, 50)
  sample_means_small[i] <- mean(samp)
}
sample_means_small
hist(sample_means_small, breaks = 25) #ugly distribution

#creates two more sampling distributions
sample_means10 <- rep(NA, 5000)
sample_means100 <- rep(NA, 5000)
for (i in 1:5000) {
  samp <- sample(area, 10)
  sample_means10[i] <- mean(samp)
  samp <- sample(area, 100)
  sample_means100[i] <- mean(samp)
}

#let's plot 3 distributions on top of one another
par(mfrow = c(3,1)) #specify plot to be divided in 3 lines and 1 column
xlimits = range(sample_means10)
hist(sample_means10, breaks = 20, xlim=xlimits)
hist(sample_means50, breaks = 20, xlim=xlimits)
hist(sample_means100, breaks = 20, xlim=xlimits)

#let's now work with the price data
summary(price)
samp2 <- sample(price, 50)
mean(samp2)

sample_means50 <- rep(NA, 5000)
for (i in 1:5000) {
  samp <- sample(price, 50)
  sample_means50[i] <- mean(samp)
}

sample_means150 <- rep(NA, 5000)
for (i in 1:5000) {
  samp <- sample(price, 150)
  sample_means150[i] <- mean(samp)
}

par(mfrow = c(2,1)) #specify plot to be divided in 2 lines and 1 column
xlimits = range(sample_means50)
hist(sample_means50, breaks = 20, xlim=xlimits)
hist(sample_means150, breaks = 20, xlim=xlimits)

mean(sample_means50)
mean(sample_means150)
mean(price)
