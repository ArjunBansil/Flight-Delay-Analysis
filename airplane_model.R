library(h2o)
h2o.init(nthreads=-1)

print("Importing dataset atm.....")
raw = h2o.importFile(path = "airlines_all.05p.csv", parse=FALSE)
setup = h2o.parseSetup(raw)
setup$column_types[which(setup$column_names %in% "AirTime")] <- "Numeric"
setup$column_types[which(setup$column_names %in% "AirDelay")] <- "Numeric"
airlines.hex = h2o.parseRaw(raw, col.types=setup$column_types)

summary(airlines.hex)

h2o.hist(airlines.hex$Year)
h2o.hist(airlines.hex$Month)

scatter_plot <- function(data, x, y, max_points = 1000, fit = T) {
  if (fit) {
    lr <- h2o.glm(x = x, y = y, training_frame = data, family = "gaussian")
    coeff <- lr@model$coefficients_table$coefficients
  }
  
  df <- data[,c(x, y)]
  
  runif <- h2o.runif(df)
  df.subset <- df[runif < max_points/nrow(data),]
  df.R <- as.data.frame(df.subset)
  h2o.rm(df.subset)
  if (fit) h2o.rm(lr@model_id)
  
  plot(x = df.R[,x], y = df.R[,y], col = "yellow", xlab = x, ylab = y)
  if (fit) abline(coef = coeff, col = "black")
}

scatter_plot(data = airlines.hex, x = "Distance", y = "AirTime", fit = T)
scatter_plot(data = airlines.hex, x = "UniqueCarrier", y = "ArrDelay", max_points = 5000, fit = F)

print("Splitting data into groups of 12 month and aggregating on two columns...")
flightByMonth   <- h2o.group_by(data = airlines.hex, by = "Month", nrow("Month"), sum("Cancelled"))
flightByMonth.R <- as.data.frame(flightByMonth)

airlines.hex$Year      <- as.factor(airlines.hex$Year)
airlines.hex$Month     <- as.factor(airlines.hex$Month)
airlines.hex$DayOfWeek <- as.factor(airlines.hex$DayOfWeek)
airlines.hex$Cancelled <- as.factor(airlines.hex$Cancelled)

hour1 <- airlines.hex$CRSArrTime %/% 100
mins1 <- airlines.hex$CRSArrTime %% 100
arrTime <- hour1*60+mins1

hour2 <- airlines.hex$CRSDepTime %/% 100
mins2 <- airlines.hex$CRSDepTime %% 100
depTime <- hour2*60+mins2

travelTime <- ifelse(arrTime - depTime > 0, arrTime - depTime, NA)
airlines.hex$TravelTime <- travelTime
scatter_plot(airlines.hex, "Distance", "TravelTime")

h2o.impute(data = airlines.hex, column = "Distance", by = c("Origin","Dest"))
scatter_plot(airlines.hex, "Distance", "TravelTime")













