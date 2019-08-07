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

library(rpart)

#Grow Regression Tree
nsw_tree <- rpart(Total.Demand~Public.Holiday+Day.of.week+Time+Year+Month+Day, method = "anova", data = nsw)

#Display results
printcp(nsw_tree)

#Visualize cross-validation results
plotcp(nsw_tree)

#Detailed summary of splits
summary(nsw_tree)

#Create additional plots
par(mfrow=c(1,2)) 
rsq.rpart(nsw_tree) # visualize cross-validation results 

#Plot tree
plot(nsw_tree, uniform=TRUE, 
     main="Regression Tree for Demand ")
text(nsw_tree, use.n=TRUE, all=TRUE, cex=.8)

#Create attractive postcript plot of tree
post(nsw_tree, file = "C:/Users/z5160496/Downloads/DATA3001-master/DATA3001-master/tree.ps", 
     title = "Regression Tree for Demand ")

#Prune the tree 
pnsw_tree<- prune(nsw_tree, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree2.ps", 
     title = "Pruned Regression Tree for Mileage")
