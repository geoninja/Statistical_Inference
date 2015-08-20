#Lab 4, part A

load(url("http://bit.ly/dasi_nc"))
names(nc)
summary(nc)
hist(nc$weeks, breaks=20)
hist(nc$weight, breaks=10)

#clean the gained weight data
gained_clean = na.omit(nc$gained)
n = length(gained_clean)
hist(gained_clean,breaks = 10)

#obtain a bootstrap confidence interval
boot_means = rep(NA, 100) #create variable to store 100 bootstrap samples
for(i in 1:100){
  boot_sample=sample(gained_clean, n, replace = TRUE)
  boot_means[i] = mean(boot_sample)
}

hist(boot_means, breaks=50)
dotchart(boot_means)
summary(boot_means)
sd(boot_means)

#download and run inference function
source("http://bit.ly/dasi_inference")
inference(gained_clean, type="ci", method="simulation", conflevel=0.9, est="median", boot_method="se")
inference(nc$fage, type="ci", method="simulation", conflevel=0.95, est="mean", boot_method="se")

#evaluating relationship b/w two variables
summary(nc$habit)
summary(nc$weight)
boxplot(nc$weight ~ nc$habit)
by(nc$weight, nc$habit, summary)
by(nc$weight, nc$habit, IQR)
by(nc$weight, nc$habit, length)
inference(y=nc$weight, x=nc$habit, type="ci", null=0, method="theoretical", est="mean", alternative="twosided", order=c("smoker", "nonsmoker"))

by(nc$mage, nc$mature, summary)
