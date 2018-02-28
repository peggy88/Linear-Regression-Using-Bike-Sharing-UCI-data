#Load data
library(readr)
df <- read_csv("C:/Users/Peggy/Desktop/Bike-Sharing-Dataset/hour.csv")

#Check summary
summary(df)

#check datatypes
str(df)

#Convert the numeric days of the week (0 to 6) to  their equivalent names (Sun to Sat)
df$weekday <- factor(df$weekday, levels = c(0,1,2,3,4,5,6), labels =  c("Sun", "Mon", "Tue", "Wed", "Thr", "Fri", "Sat"))

#create a new column 'days' to get no of days elapsed from Day1
df$days <- df$instant/24

#Exclude columns
#exclude.cols <- c("instant", "dteday", "weekday" , "atemp", "casual", "registered")
new.df <- subset(df, select = -c(1,2,8,12,15,16))

#Normalize int columns

z.score <- function(x)
{
(x-mean(x)) / sd(x)
}

new.df$temp <- z.score(new.df$temp)
new.df$hum <- z.score(new.df$hum)
new.df$windspeed <- z.score(new.df$windspeed)

#df<- new.df
#Visualize data
numCols <- c("temp", "hum", "windspeed", "hr")
bike.scatter <- function(df, cols){
  require(ggplot2)
  for(col in cols){
    p1 <- ggplot(df, aes_string(x = col, y = "cnt")) +
      geom_point(aes(alpha = 0.001, color = "blue")) +
      geom_smooth(method = "loess") +
      ggtitle(paste('Count of bikes rented vs. ', col)) +
      theme(text = element_text(size=16))
    print(p1)
  }
}
catCols <- c('season', 'yr', 'mnth', 'hr', 'holiday', 'workingday',
             'weathersit', 'dayOfWeek')
bike.box <- function(df, cols){
  require(ggplot2)
  for(col in cols){
    p1 <- ggplot(df, aes_string(x = col, y = 'cnt', group = col)) +
      geom_boxplot()+
      ggtitle(paste('Count of bikes rented vs. ', col)) +
      theme(text = element_text(size=16))
    print(p1)
  }
}
pltTimes = c(6, 8, 10, 12, 14, 16, 18, 20)
bike.series <- function(df, tms){
  require(ggplot2)
  ylims = c(min(df$cnt), max(df$cnt))
  for(t in tms){
    temp = df[df$hr == t, ]
    p1 <- ggplot(temp, aes(x = days, y = cnt)) +
      geom_line() +
      ylim(ylims) +
      ylab('Bikes rented') +
      ggtitle(paste('Count of bikes rented vs. time for', t, 'hour of
                    the day')) +
      theme(text = element_text(size=16))
    print(p1)
  }
}
histCols <- c("temp", "hum", "windspeed", "cnt")
bike.hist <- function(df, cols){
  require(ggplot2)
  for(col in cols){
    p1 <- ggplot(df, aes_string(x = col)) +
      geom_histogram() +
      ggtitle(paste('Density of', col)) +
      theme(text = element_text(size=16))
    print(p1)
  }
}
bike.hist.cond <- function(df, tms){
  require(ggplot2)
  # require(gridExtra)
  par(mfrow = c(2,4))
  for(i in 1:length(tms)){
    temp = df[df$hr == tms[i], ]
    p <- ggplot(temp, aes(x = cnt)) +
      geom_histogram() +
      ggtitle(paste('Density of bike rentals at time',
                    as.character(tms[i]))) +
      theme(text = element_text(size=16))
    print(p)
  }
  # grid.arrange(grobs = p, ncol = 3)
  par(mfrow = c(1,1))
}

bike.scatter(new.df, numCols)
bike.box(new.df, catCols)
bike.box(new.df, catCols)
bike.hist.cond(new.df, pltTimes)


#Split data into train and test data

require(caTools)
set.seed(123) 
sample = sample.split(new.df, SplitRatio = .70)
train = subset(new.df, sample == TRUE)
test  = subset(new.df, sample == FALSE)


#OLS regression

fit <- lm(cnt ~ ., data = train)
pred <- predict(fit, test)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

rmse(fit$residuals)

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
mae(fit$residuals)
