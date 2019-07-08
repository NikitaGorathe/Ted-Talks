library(anytime)
library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(chron)

Ted.df <- read.csv("ted.csv")
View(Ted.df)
#print(summary(Ted.df))
Ted.df$date_pub = anydate(Ted.df$published_date)
Ted.df$month = month(Ted.df$date_pub)
Ted.df$year = year(Ted.df$date_pub)
Ted.df$day = weekdays(Ted.df$date_pub,abbreviate = TRUE)
View(Ted.df)
Ted.df$tags <- str_count(Ted.df$tags, ",") +1
Ted.df <- Ted.df[,-c(2,4:5,7:13,15:16)]
Ted.df$weekend <- as.numeric(is.weekend(Ted.df$date_pub))
View(Ted.df)
summary(Ted.df)

ggplot(Ted.df) + geom_boxplot(aes(y = views)) + xlab("views")
ggplot(Ted.df) + geom_boxplot(aes(y = views)) + xlab("views")


#par(mfcol = c(1, 5)) 
#boxplot(Ted.df$comments ~ Ted.df$views, xlab = "Views", ylab = "Comments")
#boxplot(Ted.df$duration ~ Ted.df$views, xlab = "Views", ylab = "Duartion")
#boxplot(Ted.df$languages ~ Ted.df$views, xlab = "Views", ylab = "Languages")
#boxplot(Ted.df$tags ~ Ted.df$views, xlab = "Views", ylab = "No of Tags")
#boxplot(Ted.df$weekend ~ Ted.df$views, xlab = "Views", ylab = "Weekend")

boxplot(Ted.df$views, xlab = "Views")
#boxplot(Ted.df$views ~ Ted.df$duration, ylab = "Views", xlab = "Duartion")
#boxplot(Ted.df$views ~ Ted.df$languages, ylab = "Views", xlab = "Languages")
boxplot(Ted.df$views ~ Ted.df$tags, ylab = "Views", xlab = "No of Tags")
boxplot(Ted.df$views ~ Ted.df$weekend, ylab = "Views", xlab = "Weekend")

Ted_numeric = Ted.df[,c(1:5,10)]
pairs(Ted_numeric)
cor_matrix <- cor(Ted_numeric)
library(corrplot)
corrplot::corrplot(cor_matrix,method="shade",bg="white",titke="Correltaion Matrix (by color) and significane(*)",)

#median_duration <- median(Ted.df$duration)
#cat("Median number of duration:", median(Ted.df$duration))
#cat("Mean number of duration:", mean(Ted.df$duration))
#hist(Ted.df$duration)

Duration_hist = ggplot(Ted.df, aes(duration,..count..))+geom_histogram(fill="turquoise")+labs(x="Duration",y="Talks in that duration")
Duration_hist
ggplot(Ted.df, aes(tags,..count..)) + geom_histogram(fill="turquoise")+labs(x = "Tags",y="Views")


data.for.plot <- aggregate(Ted.df$views, by = list(Ted.df$day), FUN = mean) 
data.for.plot$DayOfWeekPublished <- factor(data.for.plot$DayOfWeekPublished, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
names(data.for.plot) <- c("DayOfWeekPublished", "Meanviews") 
#barplot(data.for.plot$Meanviews, names.arg = data.for.plot$DayOfWeekPublished, xlab = "DayOfWeekPublished", ylab = "Avg. MEDV") 
ggplot(data.for.plot) + geom_bar(aes(x = DayOfWeekPublished, y = Meanviews), stat = "identity")


fit1 <- lm(views ~ comments, data=Ted.df)
summary(fit1)
fit2 <- lm(views ~ languages, data=Ted.df)
fit3 <- lm(views ~ duration, data=Ted.df)
fit4 <- lm(views ~ weekend, data=Ted.df)
fit5 <- lm(views ~ tags, data=Ted.df)
fit6 <- lm(views ~ comments+languages, data=Ted.df)
fit7 <- lm(views ~ comments+languages+tags, data=Ted.df)
fit10 <- lm(views ~ comments+languages+weekend, data=Ted.df)
fit8 <- lm(views ~ comments+languages+tags+weekend, data=Ted.df)
fit9 <- lm(views ~ comments+languages+tags+duration+weekend, data=Ted.df)

fit <-lm(formula = views ~ comments+languages+tags+I(tags^2)+I(tags^3)+weekend+duration+I(duration^2)+I(duration^3)+comments*languages, data=Ted.df)
summary(fit)

library(stargazer)
stargazer(fit1,fit2,fit3,fit4,fit5,type="text",title="Individual Variables Compared",align=TRUE,no.space=TRUE,font.size="footnotesize")

stargazer(fit6,fit7,fit10, fit8,fit9,type="text",title="Combined variables Compared",align=TRUE,no.space=TRUE,font.size="footnotesize")


# partition the data 
selected.var <- c(1,3,5)
set.seed(1)  # set seed for reproducing the partition
numberOfRows <- nrow(Ted.df)
train.index <- sample(numberOfRows, numberOfRows*0.6)  
train.df <- Ted.df[train.index, selected.var]
valid.df <- Ted.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the
# use . after ~ to include all the remaining columns in train.df as predictors.
#Ted.lm <- lm(Price ~ ., data = train.df)
fit9 <- lm(views ~ comments+languages, data=train.df)

#  use options(scipen = TRUE) to display in scientific notation
# use options(scipen = 999) to not use scientific notation.
options(scipen = 999)
summary(fit9)


# use predict() to make predictions on a new set. 
fit9.pred <- predict(fit9, valid.df)
options(scipen=999, digits = 0)
summary(fit9.pred)

residuals <- valid.df$views - fit9.pred
squaredResiduals <- residuals*residuals
df <- data.frame("Predicted" = fit9.pred, "Actual" = valid.df$views,
                 "Residual" = residuals, "Squared Residuals" = residuals*residuals)
rmse <- sqrt(mean(df$Squared.Residuals))
print(rmse)

library(forecast)
library(leaps)

# use accuracy() from forecast package to compute common accuracy measures including rmse.
# From help file (??accuracy) the measures calculated are:
#  ME: Mean Error
#  RMSE: Root Mean Squared Error
#  MAE: Mean Absolute Error
#  MPE: Mean Percentage Error
#  MAPE: Mean Absolute Percentage Error
#  MASE: Mean Absolute Scaled Error
#options(scipen=999, digits = 3)

accuracy(fit9.pred, valid.df$views)

#plot residuals to examine
hist(df$Residual, breaks = 25, xlab = "Residuals", main = "")

# use regsubsets() in package leaps to run an exhaustive search. 
# regsubsets() will calculate adjusted r2 for every possible combination of predictors
# unlike with lm(), categorical predictors must be turned into dummies manually.


search <- regsubsets(views ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)
summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2


#par(mfrow=c(1,1))
plot(search, scale="r2")

#best 4
Ted.best.lm <- lm(views ~ comments + languages, data = train.df)
summary(Ted.best.lm)
Ted.best.lm.pred <- predict(Ted.best.lm, valid.df)
accuracy(Ted.best.lm.pred, valid.df$views)


#library(gains)
library(dplyr)
library(Hmisc)
library(ggplot2)

###plot predicted price vs target price for range of prices
df <- df[order(-df$Actual),] 

df$bin = as.numeric(cut2(df$Actual, g = 21))
table(df$bin)

bin_stats = df %>%
  group_by(bin) %>% summarise(mean_Actual = mean(Actual), mean_Predicted = mean(Predicted), min_Actual = min(Actual), min_Predicted = min(Predicted), max_Actual = max(Actual), max_Predicted = max(Predicted) )

##Plotting actual vs predicted values for Training and Validation data
p1<- ggplot(bin_stats, aes(bin)) + 
  geom_line(aes(y = bin_stats$mean_Predicted, color ="Predicted Price" )) + 
  geom_line(aes(y = bin_stats$mean_Actual, color = "Actual Price")) 

p1
