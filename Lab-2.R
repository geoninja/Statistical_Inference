load(url("http://www.openintro.org/stat/data/kobe.RData"))
names(kobe)
head(kobe)
kobe$basket[1:9]
kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak))
table(kobe_streak)
IQR(kobe_streak)

outcomes <- c("heads", "tails")
sample(outcomes, size = 10, replace = TRUE) #sampling modeling
sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)
sim_fair_coin
table(sim_fair_coin) #counts the occurrence of each element in sim_fair_coin

#simulate flipping unfair coin with 20% prob of heads
sim_unfair_coin <- sample(outcomes, size=100, replace=TRUE, prob= c(0.2, 0.8))
table(sim_unfair_coin)

#simulate independent basketball shooter
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size=133, replace=TRUE, prob= c(0.45, 0.55))
sim_basket
kobe$basket

sim_streak <- calc_streak(sim_basket)
barplot(table(sim_streak))
summary(sim_streak)
summary(kobe_streak)
table(sim_streak)
table(kobe_streak)

#Binomial distribution calculations
sum(dbinom(1:10, 10, 0.07))
sum(dbinom(35:3000000, 3000000, 0.00001))
