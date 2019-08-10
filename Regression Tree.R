nsw <- read.csv(file = 'nsw.csv', header = TRUE)
qld <- read.csv(file = 'qld.csv', header = TRUE)
vic <- read.csv(file = 'vic.csv', header = TRUE)
sa <- read.csv(file = 'sa.csv', header = TRUE)
tas <- read.csv(file = 'tas.csv', header = TRUE)

#install
install.packages('rattle')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('RColorBrewer')
install.packages('Metrics')

#load
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(ggplot2)
library(Metrics)

#Graphical ggplot
theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))

#Make factors
factor(x = nsw$Year)
factor(x = nsw$Month)
factor(x = nsw$Day)

#Relevel
nsw <- within(nsw, Year <- relevel(Year, ref = "2014"))
nsw <- within(nsw, Month <- relevel(Month, ref = "1"))
nsw <- within(nsw, Day <- relevel(Day, ref = "1"))
nsw <- within(nsw, Public.Holiday <- relevel(Public.Holiday, ref = "Nil"))
nsw <- within(nsw, Day.of.week <- relevel(Day.of.week, ref = "Monday"))

#Split Training and Testing sets
nsw_train <- subset(nsw, Year != 2018)
nsw_test <- subset(nsw, Year == 2018)

#Grow Regression Tree
#No variables included
nsw_controltree <- rpart(Total.Demand~count, method = "anova", data = nsw_train)

nsw_tree <- rpart(Total.Demand~Time.N+Off.Peak+Temperature+Weekend+Peak+Shoulder+dayofyear+Month.N+Is.Public.Holiday+Year, method = "anova", data = nsw_train)
nsw_timtree <- rpart(Total.Demand~Time.N, method = "anova", data = nsw_train)
nsw_opeaktree <- rpart(Total.Demand~Off.Peak, method = "anova", data = nsw_train)
nsw_temptree <- rpart(Total.Demand~Temperature, method = "anova", data = nsw_train)
nsw_wkdtree <- rpart(Total.Demand~Weekend, method = "anova", data = nsw_train)
nsw_peaktree <- rpart(Total.Demand~Peak, method = "anova", data = nsw_train)
nsw_shrtree <- rpart(Total.Demand~Shoulder, method = "anova", data = nsw_train)
nsw_montree <- rpart(Total.Demand~Month.N, method = "anova", data = nsw_train)
nsw_doytree <- rpart(Total.Demand~dayofyear, method = "anova", data = nsw_train)
nsw_dowtree <- rpart(Total.Demand~Day.of.week, method = "anova", data = nsw_train)
nsw_pubtree <- rpart(Total.Demand~Is.Public.Holiday, method = "anova", data = nsw_train)
nsw_yeartree <- rpart(Total.Demand~Year, method = "anova", data = nsw_train)

#Weekend and Day.of.week are the same impact
nsw_tree2 <- rpart(Total.Demand~Temperature+Off.Peak+Time.N, method = "anova", data = nsw_train)

#Display results
printcp(nsw_tree)
printcp(nsw_tree2)

#Visualize cross-validation results
plotcp(nsw_tree)

#Detailed summary of splits
summary(nsw_tree)

#Create additional plots
par(mfrow=c(1,2)) 
rsq.rpart(nsw_tree) # visualize cross-validation results 

#Plot tree
fancyRpartPlot(nsw_tree, uniform=TRUE, 
               main="Regression Tree for Demand")
text(nsw_tree, use.n=TRUE, all=TRUE, cex=.8)

#Prune the tree 
#Find cp based on minimising cross-validated error
nsw_tree$cptable[which.min(nsw_tree$cptable[,"xerror"]),"CP"]
pnsw_tree<- prune(nsw_tree, cp=nsw_tree$cptable[which.min(nsw_tree$cptable[,"xerror"]),"CP"]) # from cptable   

# plot the pruned tree 
fancyRpartPlot(pnsw_tree, uniform=TRUE, 
               main="Pruned Regression Tree for Demand")
text(pnsw_tree, use.n=TRUE, all=TRUE, cex=.8)

summary(pnsw_tree)
predict_tree <- predict(pnsw_tree, nsw_test, type = 'vector')
actual_value <- nsw_test$Total.Demand
accuracy_test <- mape(actual_value, predict_tree)
print(paste('Accuracy for test', 100-accuracy_test*100,'%'))

#Function to tune tree

accuracy_tune <- function(tree) {
  predict_tree <- predict(tree, nsw_test, type = 'vector')
  actual_value <- nsw_test$Total.Demand
  accuracy_test <- mape(actual_value, predict_tree)
  print(paste('Accuracy for tree', 100-accuracy_test*100,'%'))
}

accuracy_tune(nsw_tree2)
accuracy_tune(nsw_tree)
pred_acc <- c(accuracy_tune(nsw_controltree), accuracy_tune(pnsw_tree), accuracy_tune(nsw_pubtree),accuracy_tune(nsw_temptree),accuracy_tune(nsw_wkdtree),accuracy_tune(nsw_opeaktree),accuracy_tune(nsw_peaktree),accuracy_tune(nsw_shrtree),accuracy_tune(nsw_timtree),accuracy_tune(nsw_montree),accuracy_tune(nsw_doytree),accuracy_tune(nsw_dowtree),accuracy_tune(nsw_yeartree))
sort(pred_acc)
order(pred_acc)
