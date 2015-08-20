source("http://www.openintro.org/stat/data/present.R")
present
dim(present)
present$year
present$girls
names(present)
plot(x = present$year, y = present$girls, type = "l")
plot(present$year, present$girls + present$boys, type = "l")
?plot
present$girls + present$boys
?which.max
which.max(present$girls + present$boys)
present$year[22]
plot(present$year, present$boys/(present$boys + present$girls), type = "l")
present$boys > present$girls
plot(present$year, present$boys/present$girls, type = "l")
which.max(present$boys - present$girls)
present$year[24]
