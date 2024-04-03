# Section 1: Loading the required modules, data and data cleaning ----

if (!require(pacman)) {
  install.packages("pacman")
}

library("pacman")

pacman::p_load(RSocrata,
               lubridate,
               forecast,
               tseries,
               dplyr,
               ggplot2,
               RColorBrewer,
               patchwork)


# Fetch data from CDS, USA using the API they provided

# Data for 2014 - 2019: we need this for prediction
data1419 <- read.socrata("https://data.cdc.gov/resource/3yf8-kanr.json", )

# Data from 2020 - present
data20p <- read.socrata("https://data.cdc.gov/resource/muzy-jte6.json", )


# Let us now prepare the data

# Data for 2014 - 2019

x <- data1419
x <- x[x$jurisdiction_of_occurrence == "United States", ] #We only need data for the US as a whole
x <- x[, c("weekendingdate", "allcause")] #We only need these two columns
colnames(x) <- c("Week Ending Date", "Deaths") #Rename coluimns
data1419c <- x #Cleaned Data

# Repeat for Data from 2020

x <- data20p
x <- x[x$jurisdiction_of_occurrence == "United States", ] #We only need data for the US as a whole
x <- x[, c("week_ending_date", "all_cause")] #We only need these two columns
colnames(x) <- c("Week Ending Date", "Deaths") #Rename coluimns
data20pc <- x #Cleaned Data


# From the 2020-present data, we also need COVID death counts

x <- data20p
x <- x[x$jurisdiction_of_occurrence == "United States", ] #We only need data for the US as a whole
x <- x[, c("week_ending_date", "covid_19_u071_multiple_cause_of_death")] #We only need these two columns
colnames(x) <- c("Week Ending Date", "COVID Deaths")
coviddata <- x

# Putting the 2014-2019 and 2020-present datasets together

data <- rbind(data1419c, data20pc)

# Clean up the environment
rm(data1419, data1419c, data20p, data20pc, x)

# Converting data into time series data

deaths <- as.numeric(data$Deaths)
deaths_start <- decimal_date(ymd(data$`Week Ending Date`[1]))
covid_deaths <- as.numeric(coviddata$`COVID Deaths`)
covid_deaths_start <- decimal_date(ymd(coviddata$`Week Ending Date`[1]))

deaths <- ts(data = deaths,
             start = deaths_start,
             freq = 365.25 / 7)
covid_deaths <- ts(data = covid_deaths,
                   start = covid_deaths_start,
                   freq = 365.25 / 7)


# Section 2: Model Validation --------------------------------------------
# Forecast Deaths using Seasonal ARIMA 


# Since we observe seasonality, we will use a seasonal ARIMA model to forecast deaths
# The auto.arima() function tries to detect seasonality automatically

# First, let's split the dataset into train and test data sets so that we can first see if our model is good enough to be used for this exercise

train <- data[data$`Week Ending Date` < as.Date("2019-01-01"), ]
test <- data[data$`Week Ending Date` > as.Date("2018-12-31"), ]

train <- as.numeric(train$Deaths)
test <- as.numeric(test$Deaths)

train <- ts(data = train,
            start = decimal_date(ymd("2014-01-04")),
            freq = 365.25 / 7)
test <- ts(data = test,
           start = decimal_date(ymd("2019-01-05")),
           freq = 365.25 / 7)

# Fit an ARIMA model on the train data

model <- auto.arima(train)
model #Note that our auto-fitted ARIMA model does include a term for seasonality

predictions <- forecast(model, h = length(test), level = c(90, 95))
autoplot(predictions) #Our model accurately picks up seasonality

# Let's compare the predicted death counts to actual death counts

predictions <- as.numeric(predictions$mean)
test <- as.numeric(test)
df <- data.frame(predictions, test)
df$diff <- abs(predictions - test) / test
mean(df$diff)

# We have a MAPE (Mean Absolute Percentage Error) of 11%

# Section 3: Forecasting deaths in 2020 and 2021, using data up to 2019 ----

# Fit an ARIMA model to the data
# Because we want to compare forecasted deaths to actual deaths,
# I train the model upto 2019 and forecast for 2020 to 2021

deaths_u2019 <- data[data$`Week Ending Date` < as.Date("2020-01-01"), "Deaths"]
deaths_u2019 <- as.numeric(deaths_u2019)
deaths_u2019 <- ts(data = deaths_u2019,
                   start = decimal_date(ymd("2014-01-04")),
                   freq = 365.25 / 7)

deaths_a2019 <- data[data$`Week Ending Date` > as.Date("2019-12-31"), "Deaths"]
deaths_a2019 <- as.numeric(deaths_a2019)
deaths_a2019 <- ts(data = deaths_a2019,
                   start = decimal_date(ymd("2020-01-04")),
                   freq = 365.25 / 7)


arima <- auto.arima(deaths_u2019)
forecasted_deaths <- forecast(arima, h = length(covid_deaths), level = c(90, 95))
autoplot(forecasted_deaths)


# Section 4: A simpler exercise ------

# "Data" contains weekly data from 2014. We're going to aggregate by month.

data$Year <- year(data$`Week Ending Date`)
data$Month <- month(data$`Week Ending Date`)

# Group by and sum
data$Deaths <- as.numeric(data$Deaths)
monthly_data <- data %>% group_by(Year, Month) %>% dplyr::summarise(Deaths = sum(Deaths, na.rm = T))
monthly_data <- monthly_data[, c(2, 1, 3)]
monthly_data$Month <- factor(month.abb[monthly_data$Month], levels = month.abb)
monthly_data$Year <- as.character(monthly_data$Year)

# To make the graph neater, let's only keep the data from 2018
monthly_data <- monthly_data[monthly_data$Year == "2018" |
                               monthly_data$Year == "2019" |
                               monthly_data$Year == "2020" | monthly_data$Year == "2021", ]

ggplot(monthly_data, aes(fill = Year, y = Deaths, x = Month)) +
  geom_bar(position = "dodge", stat = "identity") + scale_fill_brewer(palette =
                                                                        "Blues")

monthly_data2 <- monthly_data
monthly_data2$Year[monthly_data2$Year == "2018"] <- "2019"

baseline <- monthly_data2 %>% group_by(Month) %>% dplyr::summarise(Baseline = mean(Deaths))

monthly_data3 <- monthly_data[monthly_data$Year == "2020" |
                                monthly_data$Year == "2021", ]
monthly_data3 <- monthly_data3[1:17, ]
monthly_data3$Deaths <- monthly_data3$Deaths - baseline$Baseline
excess_deaths <- monthly_data3
colnames(excess_deaths) <- c("Month", "Year", "Excess Deaths")
p <- ggplot(excess_deaths, aes(fill = Year, y = `Excess Deaths`, x = Month)) +
  geom_bar(position = "dodge", stat = "identity") + scale_fill_brewer(palette =
                                                                        "Reds")

p
# Adding official COVID deaths to the plot

# Group by and sum

coviddata$Year <- year(coviddata$`Week Ending Date`)
coviddata$Month <- month(coviddata$`Week Ending Date`)

coviddata$`COVID Deaths` <- as.numeric(coviddata$`COVID Deaths`)
monthly_coviddata <- coviddata %>% group_by(Year, Month) %>% dplyr::summarise(Deaths = sum(`COVID Deaths`, na.rm = T))
monthly_coviddata <- monthly_coviddata[, c(2, 1, 3)]
monthly_coviddata$Month <- factor(month.abb[monthly_coviddata$Month], levels =
                                    month.abb)
monthly_coviddata$Year <- as.character(monthly_coviddata$Year)

monthly_covid_2020 <- filter(monthly_coviddata, Year == "2020")
monthly_covid_2021 <- filter(monthly_covid_2021, Year == "2021")
monthly_covid_2020$`Year & Type` <- "2020 Official Count"
monthly_covid_2021$`Year & Type` <- "2021 Official Count"
monthly_covid_2021 <- filter(monthly_covid_2021, Month != "Jun")
excess_deaths$`Year & Type` <- paste(excess_deaths$Year, "Excess Deaths")
colnames(excess_deaths) <- c("Month", "Year", "Deaths", "Year & Type")

exvoff <- rbind(excess_deaths, monthly_covid_2020, monthly_covid_2021)

pp <- ggplot(exvoff, aes(fill = `Year & Type`, y = `Deaths`, x = Month)) +
  geom_bar(position = "dodge", stat = "identity") + scale_fill_brewer(palette =
                                                                        "Reds")

pp
