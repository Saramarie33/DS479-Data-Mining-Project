getwd()
flightdelays <- read.csv("2019_cleaned_data_na_omit.csv", stringsAsFactors = TRUE)
View(flightdelays)

str(flightdelays)

summary(flightdelays$DEP_DELAY)
hist(flightdelays$DEP_DELAY)

table(flightdelays$ORIGIN)

cor(flightdelays[c("DEP_TIME", "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", 
                   "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY","DEP_DELAY")])

fd_model <- lm(DEP_DELAY ~ ., data = flightdelays)
options(max.print = 2000)
fd_model 
summary(fd_model)

plot(flightdelays$DEP_DELAY, flightdelays$WEATHER_DELAY,
     main = 'Flight Delays From Weather',
     xlab = 'Weather', ylab = 'Flight Delays')
plot(flightdelays$DEP_DELAY, flightdelays$SECURITY_DELAY,
     main = 'Flight Delays Due to Security',
     xlab = 'Security', ylab = 'Flight Delays')

flightdelays$predict <- predict(fd_model, flightdelays)
cor(flightdelays$predict, flightdelays$DEP_DELAY)

plot(flightdelays$predict, flightdelays$DEP_DELAY)
abline (a = 0, b = 1, col = "red", lwd = 3, lty = 2)


