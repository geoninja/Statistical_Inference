load(url("http://www.openintro.org/stat/data/mlb11.RData"))

# scatterplot of the relationship between runs and at_bat variables
plot(mlb11$at_bats, mlb11$runs) # use at_bat as explanatory (x) variable
cor(mlb11$runs, mlb11$at_bats)
cor(mlb11$at_bats, mlb11$runs)

# function to manually fit the linear model: best sum of sqs = 139819
plot_ss(x = mlb11$at_bats, y = mlb11$runs,showSquares = TRUE) #awesome!

# function to automatically fit the model
m1 <- lm(runs ~ at_bats, data = mlb11)
summary(m1)

# re-run the above script for variables homeruns and runs
plot(mlb11$homeruns, mlb11$runs)
cor(mlb11$homeruns, mlb11$runs)

m2 <- lm(runs ~ homeruns, data = mlb11)
summary(m2)

# create a plot with the least squares line on top
plot(mlb11$runs ~ mlb11$at_bats)
abline(m1) #plots a line based on its slope and intercept

mlb11$at_bats[16] #5579
mlb11$runs[16] #713 vs 728.32 prediction from y=-2789 + (0.6305*5579)

# check for linearity
plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3) # adds a horizontal dashed line (line type lty) at y = 0

# check for nearly normal residuals
hist(m1$residuals)
qqnorm(m1$residuals) #theoritical vs sample quantiles
qqline(m1$residuals) #adds a line to normalization plot

##test correlation of runs with other variables

plot(mlb11$wins, mlb11$runs)
cor(mlb11$wins, mlb11$runs)

# runs vs new_onbase
plot(mlb11$hits, mlb11$runs)
cor(mlb11$hits, mlb11$runs)
m3 <- lm(runs ~ hits, data = mlb11)
summary(m3)
plot(m3$residuals ~ mlb11$hits)
abline(h = 0, lty = 3)
hist(m3$residuals)
qqnorm(m3$residuals)
qqline(m3$residuals)

# runs vs batting average
plot(mlb11$bat_avg, mlb11$runs)
cor(mlb11$bat_avg, mlb11$runs)
m4 <- lm(runs ~ bat_avg, data = mlb11)
summary(m4)
plot(m4$residuals ~ mlb11$bat_avg)
abline(h = 0, lty = 3)
hist(m4$residuals)
qqnorm(m4$residuals)
qqline(m4$residuals)

# runs vs new_onbase
plot(mlb11$new_onbase, mlb11$runs)
cor(mlb11$new_onbase, mlb11$runs)
m5 <- lm(runs ~ new_onbase, data = mlb11)
summary(m5)
plot(m5$residuals ~ mlb11$new_onbase)
abline(h = 0, lty = 3)
hist(m5$residuals)
qqnorm(m5$residuals)
qqline(m5$residuals)

# runs vs new_slug
plot(mlb11$new_slug, mlb11$runs)
cor(mlb11$new_slug, mlb11$runs)
m6 <- lm(runs ~ new_slug, data = mlb11)
summary(m6)
plot(m6$residuals ~ mlb11$new_slug)
abline(h = 0, lty = 3)
hist(m6$residuals)
qqnorm(m6$residuals)
qqline(m6$residuals)

# runs vs new_obs - we have a winner! Moneyball!!
plot(mlb11$new_obs, mlb11$runs)
cor(mlb11$new_obs, mlb11$runs)
m7 <- lm(runs ~ new_obs, data = mlb11)
summary(m7)
plot(m7$residuals ~ mlb11$new_obs)
abline(h = 0, lty = 3)
hist(m7$residuals)
qqnorm(m7$residuals)
qqline(m7$residuals)

