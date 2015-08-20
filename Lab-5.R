#Lab 5

#load the inference function
source("http://bit.ly/dasi_inference")

#load data
load(url("http://www.openintro.org/stat/data/atheism.RData"))

names(atheism)
summary(atheism$nationality)

#calculate ci for atheism data for USA in 2012
us12 = subset(atheism, atheism$nationality == "United States" & atheism$year == "2012")
us12_data = droplevels(us12)
table(us12_data)

inference(us12_data$response, est="proportion", type="ci", method="theoretical", success="atheist")

#plot the relationship between proportion and ME to demonstrate how prop affects ME
n <- 1000
p <- seq(0, 1, 0.01)
me <- 2 * sqrt(p*(1-p)/n)
plot(me ~ p)

#Question 10: hypothesis test for change in atheism in Spain
data = subset(atheism, atheism$nationality == "Spain")
spain_data = droplevels(data)
table(spain_data)
dim(spain_data)
summary(spain_data$year)
#by(atheism$nationality, atheism$response, summary)
inference(spain_data$response, as.factor(spain_data$year), est = "proportion", type = "ht", null = 0, alternative = "twosided", method = "theoretical", success = "atheist")

#Question 11: hypothesis test for change in atheism in the US
data = subset(atheism, atheism$nationality == "United States")
us_data = droplevels(data)
table(us_data)
dim(us_data)

inference(us_data$response, as.factor(us_data$year), est = "proportion", type = "ht", null = 0, alternative = "twosided", method = "theoretical", success = "atheist")

