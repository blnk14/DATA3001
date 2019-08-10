nsw <- read.csv(file = 'nsw.csv', header = TRUE)
qld <- read.csv(file = 'qld.csv', header = TRUE)
vic <- read.csv(file = 'vic.csv', header = TRUE)
sa <- read.csv(file = 'sa.csv', header = TRUE)
tas <- read.csv(file = 'tas.csv', header = TRUE)

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

nsw_lm3 <- lm(Total.Demand~Temperature + Month, data = nsw)
summary(nsw_lm3)

nsw_lm4 <- lm(Total.Demand~Time, data = nsw)
summary(nsw_lm4)

nsw_total <- lm(Total.Demand~Temperature+Is.Public.Holiday+Day.of.week+Time+Year+Month+Day+dayofyear+Weekend+Off.Peak+Peak+Shoulder, data = nsw)
summary(nsw_total)
par(mfrow = (c(2,2))) 
plot(nsw_total)

predict_lm <- predict.lm(nsw_total, nsw_test)
accuracy_lm <- function(lm) {
  predict_lm <- predict.lm(lm, nsw_test)
  actual_value <- nsw_test$Total.Demand
  accuracy_test <- mape(actual_value, predict_lm)
  print(paste('Accuracy for LM', 100-accuracy_test*100,'%'))
}
accuracy_lm(nsw_total)

lm_error <- vector(mode="integer", length=17494)
for(i in 1:17494){
  lm_error[i] <- ape(actual_value[i], predict_lm[i])
  lm_error[i] <- lm_error[i]*100
}
ggplot(nsw_test, aes(dayofyear, lm_error)) + geom_smooth() + xlab("Day") + ylab("Error (%)") + ggtitle("Linear Model Mean Absolute Error Percentage") + theme_ts

nsw_temp <- lm(Total.Demand~Temperature, data = nsw)
summary(nsw_temp)


#Demand ~ Temperature Plot Separated By Different Types of Public Holidays
min(nsw$Temperature)
max(nsw$Temperature)
min(nsw$Total.Demand)
max(nsw$Total.Demand)

check_nil <- dplyr::filter(nsw, grepl('Nil', Public.Holiday))
check_obs <- dplyr::filter(nsw, grepl('Observed', Public.Holiday))
check_week <- dplyr::filter(nsw, grepl('Weekend', Public.Holiday))
check_pub <- dplyr::filter(nsw, !grepl('Observed|Weekend|Nil', Public.Holiday))

par(mfrow = (c(2,2)))
plot(Total.Demand ~ Temperature, data = check_nil, xlim = range(-5:45), ylim = range(5000:14000), main = "No Holiday")
plot(Total.Demand ~ Temperature, data = check_obs, xlim = range(-5:45), ylim = range(5000:14000), main = "Observed Holiday")
plot(Total.Demand ~ Temperature, data = check_week, main = "Weekend Holiday", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_pub, main = "Public Holiday", xlim = range(-5:45), ylim = range(5000:14000))

#Demand ~ Temperature Plot Separated By Seasonality
nsw$Month.N <- month.abb[nsw$Month]

#Season
check_sum <- dplyr::filter(nsw, grepl('Jan|Feb|Dec', Month.N))
check_aut <- dplyr::filter(nsw, grepl('Mar|Apr|May', Month.N))
check_win <- dplyr::filter(nsw, grepl('Jun|Jul|Aug', Month.N))
check_spr <- dplyr::filter(nsw, grepl('Sep|Oct|Nov', Month.N))

par(mfrow = (c(2,2)))
plot(Total.Demand ~ Temperature, data = check_sum, main = "Summer", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_aut, main = "Autumn", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_win, main = "Winter", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_spr, main = "Spring", xlim = range(-5:45), ylim = range(5000:14000))

#Month
check_jan <- dplyr::filter(nsw, grepl('Jan', Month.N))
check_feb <- dplyr::filter(nsw, grepl('Feb', Month.N))
check_mar <- dplyr::filter(nsw, grepl('Mar', Month.N))
check_apr <- dplyr::filter(nsw, grepl('Apr', Month.N))
check_may <- dplyr::filter(nsw, grepl('May', Month.N))
check_jun <- dplyr::filter(nsw, grepl('Jun', Month.N))
check_jul <- dplyr::filter(nsw, grepl('Jul', Month.N))
check_aug <- dplyr::filter(nsw, grepl('Aug', Month.N))
check_sep <- dplyr::filter(nsw, grepl('Sep', Month.N))
check_oct <- dplyr::filter(nsw, grepl('Oct', Month.N))
check_nov <- dplyr::filter(nsw, grepl('Nov', Month.N))
check_dec <- dplyr::filter(nsw, grepl('Dec', Month.N))

#By season
par(mfrow=c(1,3))

#By Year
par(mfrow=c(4,3))
plot(Total.Demand ~ Temperature, data = check_dec, main = "December", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_jan, main = "January", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_feb, main = "February", xlim = range(-5:45), ylim = range(5000:14000))

plot(Total.Demand ~ Temperature, data = check_mar, main = "March", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_apr, main = "April", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_may, main = "May", xlim = range(-5:45), ylim = range(5000:14000))

plot(Total.Demand ~ Temperature, data = check_jun, main = "June", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_jul, main = "July", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_aug, main = "August", xlim = range(-5:45), ylim = range(5000:14000))

plot(Total.Demand ~ Temperature, data = check_sep, main = "September", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_oct, main = "October", xlim = range(-5:45), ylim = range(5000:14000))
plot(Total.Demand ~ Temperature, data = check_nov, main = "November", xlim = range(-5:45), ylim = range(5000:14000))

