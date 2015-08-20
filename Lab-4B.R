#Lab 4 part B

load(url("http://bit.ly/dasi_gss_ws_cl"))
summary(gss)
boxplot(gss$wordsum ~ gss$class)

inference(y=gss$wordsum, x=gss$class, est="mean", type="ht", alternative="greater", method="theoretical" )

load(url("http://bit.ly/dasi_gss_data"))
summary(gss$educ)
hist(gss$educ)
