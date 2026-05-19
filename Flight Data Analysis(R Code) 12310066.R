#setting home directory
getwd()
list.files() 

setwd("C:/Users/USER/Downloads/R & Python")

##problem with source
.libPaths()
.libPaths("C:/Users/USER/Downloads/R & Python/pakage")

install.packages("ggplot2")
library(ggplot2)

# Load dataset
flight <- read.csv("C:/Users/USER/Downloads/R & Python/flight_data.csv")

# Random sample using student ID
set.seed(66)
flight_sample <- flight[sample(nrow(flight), 10000), ]


##Question 1 :
#What do coach ticket prices look like? What are the high and low values? What would be considered
#the average? Does $500 seem like a good price for a coach ticket?”

# Summary statistics
summary(flight_sample$coach_price)

# Mean
mean(flight_sample$coach_price)

# Histogram
ggplot(flight_sample, aes(x=coach_price)) +
  geom_histogram(bins=30, fill="skyblue", color="black") +
  labs(
    title="Distribution of Coach Ticket Prices",
    x="Coach Ticket Price",
    y="Frequency"
  )

#Boxplot
boxplot(flight_sample$coach_price,
        main = "Boxplot of Coach Ticket Price",
        ylab = "Coach Ticket Price",
        col = "red",
        border = "black")

##Q2: What are the coach prices for flights that are 8 hours long? Does a $500 ticket seem more reasonable for these flights? 
#8-hour flights

flight_8 <- subset(flight_sample, hours == 8)

# Summary statistics
summary(flight_8$coach_price)


##Q3: What does the distribution of flight delays look like? Are there any outliers?
library(ggplot2)



# Histogram
ggplot(flight_sample, aes(x=delay)) +
  geom_histogram(bins=40, fill="orange", color="black") +
  labs(
    title="Distribution of Flight Delays",
    x="Delay Time",
    y="Frequency"
  )


# Summary
summary(flight_sample$delay)


##Q4: How are coach ticket prices related to flight distance (miles)?
library(ggplot2)

# Scatter plot
ggplot(flight_sample,
       aes(x=miles, y=coach_price)) +
  geom_point(alpha=0.3) +
  labs(
    title="Coach Price vs Flight Distance",
    x="Miles",
    y="Coach Price",
    col="blue"
  )

# Correlation
cor(flight_sample$miles,
    flight_sample$coach_price)

##Q5: How are flight hours related to coach ticket prices?
library(ggplot2)

# Scatter plot
ggplot(flight_sample,
       aes(x=hours, y=coach_price)) +
  geom_point(alpha=0.3) +
  labs(
    title="Flight Hours vs Coach Price",
    x="Flight Hours",
    y="Coach Price"
  )

# Correlation
cor(flight_sample$hours,
    flight_sample$coach_price)

##Q6: How are coach ticket prices related to first-class ticket prices?
library(ggplot2)

# Scatter plot
ggplot(flight_sample,
       aes(x=coach_price,
           y=firstclass_price)) +
  geom_point(alpha=0.3) +
  labs(
    title="Coach vs First-Class Prices",
    x="Coach Price",
    y="First-Class Price"
  )

# Correlation
cor(flight_sample$coach_price,
    flight_sample$firstclass_price)


##Q7: How do in-flight features affect coach ticket prices?
# Meal
aggregate(coach_price ~ inflight_meal,
          data=flight_sample,
          mean)

# Entertainment
aggregate(coach_price ~ inflight_entertainment,
          data=flight_sample,
          mean)

# Wifi
aggregate(coach_price ~ inflight_wifi,
          data=flight_sample,
          mean)

##Q8: How do weekend flights compare to weekday flights in terms of ticket prices?
library(ggplot2)
install.packages("dplyr")
library(dplyr)

# Average prices
weekend_price <- flight_sample %>%
  group_by(weekend) %>%
  summarise(
    avg_coach = mean(coach_price),
    avg_firstclass = mean(firstclass_price)
  )

print(weekend_price)

# Bar plot
library(ggplot2)
ggplot(weekend_price,
       aes(x=weekend,
           y=avg_coach,
           fill=weekend)) +
  geom_bar(stat="identity") +
  labs(
    title="Weekend vs Weekday Coach Prices",
    x="Weekend",
    y="Average Coach Price"
  )

##Q9: How do redeye flights compare to non-redeye flights in terms of coach ticket prices?
library(ggplot2)
library(dplyr)


# Average prices
redeye_price <- flight_sample %>%
  group_by(redeye) %>%
  summarise(
    avg_coach = mean(coach_price)
  )

print(redeye_price)

# Bar plot
ggplot(redeye_price,
       aes(x=redeye,
           y=avg_coach,
           fill=redeye)) +
  geom_bar(stat="identity") +
  labs(
    title="Redeye vs Non-Redeye Coach Prices",
    x="Redeye Flight",
    y="Average Coach Price"
  )

##Q10: Perform the analysis for appropriate variables
#Summary statisti
summary(flight_sample)

#(b) Visualization
library(ggplot2)

# Histogram
ggplot(flight_sample,
       aes(x=coach_price)) +
  geom_histogram(bins=30)

# Boxplot
ggplot(flight_sample,
       aes(y=coach_price)) +
  geom_boxplot()

# Bar chart
ggplot(flight_sample,
       aes(x=weekend)) +
  geom_bar()

##(c) Hypothesis Testing (Independent t-test)
t.test(
  coach_price ~ weekend,
  data=flight_sample
)

##(d) Correlation Analysis
cor(
  flight_sample[, c(
    "coach_price",
    "miles",
    "hours",
    "delay",
    "firstclass_price"
  )]
)

##(e) Linear Regression
model <- lm(
  coach_price ~ miles + hours + delay,
  data=flight_sample
)

summary(model)

#10(f) Logistic Regression
flight_sample$redeye_binary <-
  ifelse(
    flight_sample$redeye=="Yes",
    1,0
  )

log_model <- glm(
  redeye_binary ~ hours + coach_price,
  data=flight_sample,
  family="binomial"
)

summary(log_model)

