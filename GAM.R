nsw <- read.csv(file = 'nsw.csv', header = TRUE)
qld <- read.csv(file = 'qld.csv', header = TRUE)
vic <- read.csv(file = 'vic.csv', header = TRUE)
sa <- read.csv(file = 'sa.csv', header = TRUE)
tas <- read.csv(file = 'tas.csv', header = TRUE)

library(gam)
library(ggplot2)
library(SparseM)
library(quantreg)
library(gridExtra)
library(mgcv) #More options for gam
library(mgcViz) #Visuals for gam

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

nsw_gam_total <- gam(Total.Demand~Public.Holiday+Day.of.week+Time+Year+s(Temperature)+s(Month)+s(Day), data = nsw_train)
summary(nsw_gam_total)

#Temperature, Month and Time
ggplot(nsw_train, aes(x = Temperature, y = Total.Demand)) + geom_point() + geom_smooth() + geom_quantile(color = "SlateBlue3", size = 0.8) + theme_ts
ggplot(nsw_train, aes(x = Month, y = Total.Demand)) + geom_point() + geom_smooth() + geom_quantile(color = "SlateBlue3", size = 0.8) + theme_ts
ggplot(nsw_train, aes(x = Time.N, y = Total.Demand)) + geom_point() + geom_smooth() + geom_quantile(color = "SlateBlue3", size = 0.8) + theme_ts
ggplot(nsw_train, aes(x = Day, y = Total.Demand)) + geom_point() + geom_smooth() + geom_quantile(color = "SlateBlue3", size = 0.8) + theme_ts


#ggplot(nsw_train, aes(x = Temperature, y = Total.Demand)) + geom_point() + geom_smooth(method = gam, formula = y ~ splines::bs(x, 3)) + theme_ts
#ggplot(nsw_train, aes(x = Temperature, y = Total.Demand)) + geom_point() + geom_smooth(method = gam, formula = y ~ splines::ns(x, 3)) + theme_ts
#ggplot(nsw_train, aes(x = Month, y = Total.Demand)) + geom_point() + geom_smooth(method = gam, formula = y ~ splines::ns(x, 3)) + theme_ts
#ggplot(nsw_train, aes(x = Month, y = Total.Demand)) + geom_point() + geom_smooth(method = gam, formula = y ~ splines::bs(x, 3)) + theme_ts
#ggplot(nsw_train, aes(x = Time.N, y = Total.Demand)) + geom_point() + geom_smooth(method = gam, formula = y ~ splines::ns(x, 3)) + theme_ts
#ggplot(nsw_train, aes(x = Time.N, y = Total.Demand)) + geom_point() + geom_smooth(method = gam, formula = y ~ splines::bs(x, 3)) + theme_ts

#grid.arrange(plot3, plot4, ncol=(2))

nsw_gam_month <- gam(Total.Demand~s(Month), data = nsw_train)
summary(nsw_gam_month)
plot.gam(nsw_gam_month)
predict.gam()

nsw_gam_temp <- gam(Total.Demand~s(Temperature), data = nsw_train)
summary(nsw_gam_temp)
plot.gam(nsw_gam_temp)

nsw_gam_time <- gam(Total.Demand~s(Time.N), data = nsw_train)
summary(nsw_gam_time)
plot.gam(nsw_gam_time)

nsw_gam <- gam(Total.Demand~s(Temperature) + s(Month), data = nsw_train)
plot.gam(nsw_gam)
nsw_gam <- getViz(nsw_gam)
par(mfrow=c(2,2))
check.gamViz(nsw_gam,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 10))

#Find min and max points
min(nsw_train$Temperature)
max(nsw_train$Temperature)

grid <- list(Temperature = seq(from = -1.3, to = 43.9, length = 20),
             Month = seq(from = 0, to = 12, length = 20))
nsw_pr <- mgcv::predict.gam(nsw_gam, newdata = expand.grid(grid))
nsw_pr <- matrix(nsw_pr, nrow = 20, ncol = 20)
persp(grid$Temperature, grid$Month, nsw_pr, xlab = "Temperature", ylab = "Month",
      zlab = "Total Demand", theta = -45, phi = 20, d = 2.0, tick = "detailed")

vis.gam(nsw_gam, n.grid = 50, theta = -45, phi = 20, zlab = "Total Demand",
        ticktype = "detailed", color = "heat", main = "Temp vs Month vs Demand")

nsw_gam2 <- gam(Total.Demand~s(Temperature) + s(Time.N), data = nsw_train)
plot.gam(nsw_gam2)
nsw_gam2 <- getViz(nsw_gam2)
par(mfrow=c(2,2))
check.gamViz(nsw_gam2,
             a.qq = list(method = "tnorm", 
                         a.cipoly = list(fill = "light blue")), 
             a.respoi = list(size = 0.5), 
             a.hist = list(bins = 10))
vis.gam(nsw_gam2, n.grid = 50, theta = -45, phi = 20, zlab = "Total Demand",
        ticktype = "detailed", color = "heat", main = "Temp vs Time vs Demand")

nsw_gam3 <- gam(Total.Demand~s(Month) + s(Time.N), data = nsw_train)
plot.gam(nsw_gam3)
nsw_gam3 <- getViz(nsw_gam3)
par(mfrow=c(2,2))
check.gamViz(nsw_gam3,
             a.qq = list(method = "tnorm", 
                         a.cipoly = list(fill = "light blue")), 
             a.respoi = list(size = 0.5), 
             a.hist = list(bins = 10))
vis.gam(nsw_gam3, n.grid = 50, theta = -45, phi = 20, zlab = "Total Demand",
        ticktype = "detailed", color = "heat", main = "Month vs Time vs Demand")

nsw_gam4 <- gam(Temperature~s(Month) + s(Time.N), data = nsw_train)
plot.gam(nsw_gam4)
nsw_gam3 <- getViz(nsw_gam4)
par(mfrow=c(2,2))
check.gamViz(nsw_gam4,
             a.qq = list(method = "tnorm", 
                         a.cipoly = list(fill = "light blue")), 
             a.respoi = list(size = 0.5), 
             a.hist = list(bins = 10))
vis.gam(nsw_gam4, n.grid = 50, theta = -45, phi = 20, zlab = "Temp",
        ticktype = "detailed", color = "heat", main = "Month vs Time vs Temp")

nsw_gam_predict <- gam()