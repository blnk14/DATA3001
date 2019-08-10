nsw <- read.csv(file = 'nsw.csv', header = TRUE)
qld <- read.csv(file = 'qld.csv', header = TRUE)
vic <- read.csv(file = 'vic.csv', header = TRUE)
sa <- read.csv(file = 'sa.csv', header = TRUE)
tas <- read.csv(file = 'tas.csv', header = TRUE)
nsw1 <- read.csv(file = 'nsw1.csv', header = TRUE)

nsw<-nsw[,-2]
nsw$Day.of.week <- nsw1$Day.of.week
nsw$Weekend <- nsw$Day.of.week == "Saturday" | nsw$Day.of.week == "Sunday"
nsw$Month.N <- month.abb[nsw$Month]
nsw$Off.Peak <- (nsw$Time == "22:00" | nsw$Time == "22:30" | nsw$Time == "23:00" | nsw$Time == "23:30" | nsw$Time == "00:00" | nsw$Time == "00:30" | nsw$Time == "01:00" | nsw$Time == "01:30" | nsw$Time == "02:00" | nsw$Time == "02:30" | nsw$Time == "03:00" | nsw$Time == "03:30" | nsw$Time == "04:00" | nsw$Time == "04:30" | nsw$Time == "05:00" | nsw$Time == "05:30" | nsw$Time == "06:00" | nsw$Time == "06:30" | nsw$Time == "07:00")
nsw$Peak <- nsw$Weekend == "FALSE"  & (nsw$Time == "14:00" | nsw$Time == "14:30" | nsw$Time == "15:00" | nsw$Time == "15:30" | nsw$Time == "16:00" | nsw$Time == "16:30" | nsw$Time == "17:00" | nsw$Time == "17:30" | nsw$Time == "17:00" | nsw$Time == "18:30" | nsw$Time == "19:00" | nsw$Time == "19:30" | nsw$Time == "20:00")
nsw$Shoulder <- nsw$Off.Peak == "FALSE" & nsw$Peak == "FALSE"
nsw$Time.N <- (as.numeric(nsw$Time) - 1)/2
for (i in 1:nrow(nsw)) {
  nsw$count[i] <- i
}
nsw$Is.Public.Holiday <- nsw$Public.Holiday != "Nil"
nsw_train <- subset(nsw, Year != 2018)
nsw_test <- subset(nsw, Year == 2018)

