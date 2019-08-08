nsw <- read.csv(file = 'nsw.csv', header = TRUE)
qld <- read.csv(file = 'qld.csv', header = TRUE)
vic <- read.csv(file = 'vic.csv', header = TRUE)
sa <- read.csv(file = 'sa', header = TRUE)
tas <- read.csv(file = 'tas', header = TRUE)

library(ggplot2)

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

nsw_lm <- lm(Total.Demand~Public.Holiday, data = nsw)
summary(nsw_lm)
par(mfrow = (c(2,2)))
plot(nsw_lm)

nsw_lm2 <- lm(Total.Demand~Day.of.week, data = nsw)
summary(nsw_lm2)
anova(nsw_lm2)

nsw_total <- lm(Total.Demand~Temperature+Public.Holiday+Day.of.week+Time+Year+Month+Day, data = nsw)
summary(nsw_total)
par(mfrow = (c(2,2))) 
plot(nsw_total)

nsw_temp <- lm(Total.Demand~Temperature, data = nsw)
summary(nsw_temp)






