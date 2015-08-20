load(url("http://www.openintro.org/stat/data/ames.RData"))

population <- ames$Gr.Liv.Area
samp <- sample(population, 60)

sample_mean <- mean(samp)
se <- sd(samp)/sqrt(60) #calculate std error of sample
lower <- sample_mean - 1.96 * se
upper <- sample_mean + 1.96 * se
c(lower, upper) #confidence interval at 95%

qnorm(0.025)
pnorm(-1.96)

samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60

for (i in 1:50) {
  samp <- sample(population, n)
  samp_mean[i] <- mean(samp)
  samp_sd[i] <- sd(samp)
}

lower <- samp_mean - 1.96 * samp_sd/sqrt(n)
upper <- samp_mean + 1.96 * samp_sd/sqrt(n)

c(lower[1], upper[1])

plot_ci(lower, upper, mean(population))

