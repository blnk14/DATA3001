nsw <- read.csv(file = 'nsw.csv', header = TRUE)
qld <- read.csv(file = 'qld.csv', header = TRUE)
vic <- read.csv(file = 'vic.csv', header = TRUE)
sa <- read.csv(file = 'sa.csv', header = TRUE)
tas <- read.csv(file = 'tas.csv', header = TRUE)

#install
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

#load
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

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
fancyRpartPlot(nsw_tree, uniform=TRUE, 
               main="Regression Tree for Demand ")
text(nsw_tree, use.n=TRUE, all=TRUE, cex=.8)

#Prune the tree 
#Find cp based on minimising cross-validated error
pnsw_tree<- prune(nsw_tree, cp=0.010000) # from cptable   

# plot the pruned tree 
fancyRpartPlot(pnsw_tree, uniform=TRUE, 
               main="Pruned Regression Tree for Demand")
text(pnsw_tree, use.n=TRUE, all=TRUE, cex=.8)

summary(pnsw_tree)
