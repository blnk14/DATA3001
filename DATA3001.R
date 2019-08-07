nsw <- read.csv(file = file.choose(), header = TRUE)
qld <- read.csv(file = file.choose(), header = TRUE)
vic <- read.csv(file = file.choose(), header = TRUE)
sa <- read.csv(file = file.choose(), header = TRUE)
tas <- read.csv(file = file.choose(), header = TRUE)


nsw <- within(nsw, Public.Holiday <- relevel(Public.Holiday, ref = "Nil"))
nsw_lm <- lm(Total.Demand~Public.Holiday, data = nsw)
summary(nsw_lm)
anova(nsw_lm)
par(mfrow = (c(2,2)))
plot(nsw_lm)

nsw <- within(nsw, Day.of.week <- relevel(Day.of.week, ref = "Monday"))
nsw_lm2 <- lm(Total.Demand~Day.of.week, data = nsw)
summary(nsw_lm2)
anova(nsw_lm2)

nsw_total <- lm(Total.Demand~Public.Holiday+Day.of.week+Time, data = nsw)
summary(nsw_total)
anova(nsw_total)
par(mfrow = (c(2,2))) 
plot(nsw_total)

nsw_temp <- lm(Total.Demand~Temperature, data = nsw)
summary(nsw_temp)
anova(nsw_temp)

nsw_tree <- rpart(Total.Demand~Public.Holiday+Day.of.week+Time+Year+Month+Day, data = nsw, method = "anova")
