if (!dir.exists("data")) {
        dir.create("data")
}

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip_path <- "data/activity.zip"

download.file(url, destfile = zip_path, method = "curl")
unzip(zip_path, exdir = "data")

data <- read.csv("data/activity.csv")
head(data)
dim(data)
summary(data)
str(data)
data$date <- as.Date(data$date)

steps <- aggregate(steps ~ date, data = data, FUN = sum, na.rm = TRUE)
head(steps)



hist(steps$steps, 
     main = "Histogram of Total Steps Per Day",
     xlab = "Number of Steps", 
     col = "wheat")
mean(steps$steps)
median(steps$steps)

count(data(is.na))

sum(is.na(data$date))
sum(is.na(data$steps))
sum(is.na(data$interval))
colSums(is.na(data))

# Show the first few rows where steps are NA
na_rows <- data[is.na(data$steps), ]

head(na_rows)
View(na_rows)

```{r}
# 1. Create a copy of the original dataset
imputed_data <- data

# 2. Find rows where steps are NA, find their matching interval mean, and swap them in
imputed_data$steps <- ifelse(is.na(imputed_data$steps), 
                             intervalSteps$steps[match(imputed_data$interval, intervalSteps$interval)], 
                             imputed_data$steps)
imputed_data
summary(imputed_data)
intervalSteps



intervalSteps <- aggregate (steps ~ interval, data, mean, na.rm = TRUE)
head(intervalSteps)
library(ggplot2)
ggplot(intervalSteps,
       aes(x = interval, y = steps)) +
        geom_line() +
        theme_bw() +
        labs(
                title = "Average Daily Activity Pattern",
                x = "5-minute interval",
                y = "The average number of steps taken across all days"
        )


data.frame(
        Original   = c(mean_steps, median_steps),
        Imputed    = c(mean_imsteps, median_imsteps),
        Difference = c(mean_imsteps - mean_steps, median_imsteps - median_steps),
        row.names  = c("Mean", "Median")
)



data$date <- as.Date(data$date)
days <- weekdays(data$date)
data$weekdays_end <- ifelse(days %in% c("Saturday", "Sunday"),
                                "weekend",
                                "weekday")
data$weekdays_end <- as.factor(data$weekdays_end)

week_de_average <- aggregate(steps ~ interval + weekdays_end,
                             data,
                             mean)

xyplot(steps ~ interval | weekdays_end,
       week_de_average,
       type = "l",
       layout = c(1,2),
       xlab = "5-Minute Interval",
       ylab = "Average number of Steps",
       main = "Weekday vs. Weekend Patterns")
