source("http://www.openintro.org/stat/data/cdc.R")
names(cdc)
dim(cdc)
cdc$height
cdc$gender == "m"
summary(cdc)
cdc$genhlth == "excellent"/cdc$genhlth
summary(cdc$genhlth)
4657/20000
?table

#take a look at the first few, or the last few data entries:
head(cdc)
tail(cdc)

#create a mosaic plot (need to create contigency table)
mosaicplot(table(cdc$smoke100, cdc$gender))

#stats for numerical variables
summary(cdc$gender)
var(cdc$weight)
mean(cdc$weight)
median(cdc$weight)

#stats for categorical variables
table(cdc$smoke100)/20000
barplot(table(cdc$smoke100))

#subsetting
cdc[1000, 6] #row 1000, column 6
cdc$weight[1000] #same as above
cdc[1:10, 6] #weight of first 10 data entries
cdc$weight[1:10] #same as above

#conditioning
cdc$gender == "m"
cdc$age > 30

#extract a set of data
mdata = subset(cdc, cdc$age > 30 & cdc$gender == "m")
dim(mdata)
head(mdata)
m_or_over30 = subset(cdc, cdc$gender == "m" | cdc$age > 30)
under23_and_smoke = subset(cdc, cdc$age<23 & cdc$smoke100 == 1)
dim(under23_and_smoke)

#data analysis
boxplot(cdc$height)
summary(cdc$height)
boxplot(cdc$height ~ cdc$gender) #boxplot of height versus gender

bmi = (cdc$weight/cdc$height ^ 2) * 703
boxplot(bmi ~ cdc$genhlth)
head(bmi)
summary(bmi)

hist(cdc$age)
hist(bmi)
hist(bmi, breaks = 50)

plot(cdc$weight, cdc$wtdesire)
