install.packages("strucchange")
library(strucchange)

library(dplyr)
library(ggpubr)
library(ggplot2)
library(lmtest)

#2.1
selected_columns <- c("temp", "atemp","hum", "windspeed", "registered","casual")
bike.sharing1 <- Bike.Sharing[, selected_columns]

cor(bike.sharing1) 

correlation_test <- cor.test(bike.sharing1$hum, bike.sharing1$casual)
p_value <- correlation_test$p.value

correlation_test <- cor.test(bike.sharing1$windspeed, bike.sharing1$casual)
p_value <- correlation_test$p.value
# removes hum
Bike.Sharing <- Bike.Sharing[, !colnames(Bike.Sharing) %in% "hum"]

#2.2
# Bike.Sharing <- Bike.Sharing %>%
#   mutate(temp_diff = atemp - temp)
# lmtest <- lm(formula = casual ~ temp_diff, data = Bike.Sharing)
# summary(lmtest)
# 
# selected_columns <- c("temp", "atemp","temp_diff", "casual" )
# selected_data <- Bike.Sharing[, selected_columns]
# cor(selected_data)

# copy the data set
Bike.Sharing.Part2.Avi2 <- Bike.Sharing

# 1 - add weekend
Bike.Sharing.Part2.Avi2 <- Bike.Sharing.Part2.Avi2 %>% mutate(weekend = ifelse(weekday %in% c(5, 6), 1, 0))
selected_columns <- c("weekday","weekend", "casual" )
selected_data <- Bike.Sharing.Part2.Avi2[, selected_columns]
cor(selected_data)
# removes weekday
Bike.Sharing <- Bike.Sharing[, !colnames(Bike.Sharing) %in% "weekday"]


#2.3

# Create a new column named 'weekend_help' and initialize it with 0
Bike.Sharing.Part2.Avi2$weekend_help <- 0
# Update the values to 1 where the weekend index is 1 
Bike.Sharing.Part2.Avi2$weekend_help[Bike.Sharing.Part2.Avi2$weekend == 1] <- 1

# Create a new column named 'holiday_help' and initialize it with 0
Bike.Sharing.Part2.Avi2$holiday_help <- 0
# Update the values to 1 where the holiday value is 'yes' 
Bike.Sharing.Part2.Avi2$holiday_help[Bike.Sharing.Part2.Avi2$holiday == "yes"] <- 1

# Create a new column named 'summer_help' and initialize it with 0
Bike.Sharing.Part2.Avi2$summer_help <- 0
# Update the values to 1 where the season value is summer
Bike.Sharing.Part2.Avi2$summer_help[Bike.Sharing.Part2.Avi2$season == "summer"] <- 1

# Create a new column named 'winter_help' and initialize it with 0
Bike.Sharing.Part2.Avi2$winter_help <- 0
# Update the values to 1 where the season value is summer
Bike.Sharing.Part2.Avi2$winter_help[Bike.Sharing.Part2.Avi2$season == "winter"] <- 1

# Create a new column named 'spring_help' and initialize it with 0
Bike.Sharing.Part2.Avi2$spring_help <- 0
# Update the values to 1 where the season value is spring
Bike.Sharing.Part2.Avi2$spring_help[Bike.Sharing.Part2.Avi2$season == "springer"] <- 1




weekend_help <- factor(Bike.Sharing.Part2.Avi2$weekend_help, labels=c("1", "0"))
holiday_help <- factor(Bike.Sharing.Part2.Avi2$holiday_help, labels=c("1", "0"))
summer_help <- factor(Bike.Sharing.Part2.Avi2$summer_help, labels=c("1", "0"))
winter_help <- factor(Bike.Sharing.Part2.Avi2$winter_help, labels=c("1", "0"))
spring_help <- factor(Bike.Sharing.Part2.Avi2$spring_help, labels=c("1", "0"))


# 2.4 1
# interaction var Using multiplication:
Bike.Sharing.Part2.Avi2$registered_weekend <- Bike.Sharing.Part2.Avi2$registered * Bike.Sharing.Part2.Avi2$weekend_help

# Fit the linear regression models
model_without_interaction <- lm(casual ~ registered , data = Bike.Sharing.Part2.Avi2)
model_with_interaction <- lm(casual ~ registered_weekend, data = Bike.Sharing.Part2.Avi2)

#Modeliv1 <- Im(formula=Bike.Sharing.Part2.Avi2$casual ~ (Bike.Sharing.Part2$registered) * (Bike.Sharing.Part2.Avi2$weekend_help))
summary(model_without_interaction)
summary(model_with_interaction)

plot(Bike.Sharing.Part2.Avi2$registered[Bike.Sharing.Part2.Avi2$weekend_help==0],
     Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$weekend_help==0],
     xlab="Registed", ylab="Casual", main="Weekend Rents", col="blue1")

points(Bike.Sharing.Part2.Avi2$registered[Bike.Sharing.Part2.Avi2$weekend_help==1],
       Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$weekend_help==1],
       col="red")

abline(model_without_interaction, col = "blue")
abline(model_with_interaction, col = "red")

# Adding a legend
legend("topright", legend = c("Weekday", "Weekend"),
       col = c("blue", "red3"), pch = 1)

# 2.4 2
# interaction var temp_holidy Using multiplication:
Bike.Sharing.Part2.Avi2$temp_holiday <- Bike.Sharing.Part2.Avi2$temp * Bike.Sharing.Part2.Avi2$holiday_help

# Fit the linear regression models
model_without_interaction <- lm(casual ~ temp , data = Bike.Sharing.Part2.Avi2)
model_with_interaction <- lm(casual ~ temp_holiday, data = Bike.Sharing.Part2.Avi2)

summary(model_without_interaction)
summary(model_with_interaction)

plot(Bike.Sharing.Part2.Avi2$temp[Bike.Sharing.Part2.Avi2$holiday_help==0],
     Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$holiday_help==0],
     xlab="Temp", ylab="Casual", main="Temp at Holidays", col="blue1")

points(Bike.Sharing.Part2.Avi2$temp[Bike.Sharing.Part2.Avi2$holiday_help==1],
       Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$holiday_help==1],
       col="red")

abline(model_without_interaction, col = "blue")
abline(model_with_interaction, col = "red")

# Adding a legend
legend("topright", legend = c("No Holiday", "Holiday"),
       col = c("blue", "red"), pch = 1)

# 2.4 3
# interaction var wind_summer Using multiplication:
Bike.Sharing.Part2.Avi2$wind_summer <- Bike.Sharing.Part2.Avi2$windspeed * Bike.Sharing.Part2.Avi2$summer_help

# Fit the linear regression models
model_without_interaction <- lm(casual ~ windspeed , data = Bike.Sharing.Part2.Avi2)
model_with_interaction <- lm(casual ~ wind_summer, data = Bike.Sharing.Part2.Avi2)

summary(model_without_interaction)
summary(model_with_interaction)

plot(Bike.Sharing.Part2.Avi2$windspeed[Bike.Sharing.Part2.Avi2$summer_help==0],
     Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$summer_help==0],
     xlab="Windspeed", ylab="Casual", main="Wind Of Summer", col="blue1")

points(Bike.Sharing.Part2.Avi2$windspeed[Bike.Sharing.Part2.Avi2$summer_help==1],
       Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$summer_help==1],
       col="red")

abline(model_without_interaction, col = "blue")
abline(model_with_interaction, col = "red")

# Adding a legend
legend("topright", legend = c("No Summer", "Summer"),
       col = c("blue", "red"), pch = 1)

# 2.4 4
# interaction var temp_holidy Using multiplication:
Bike.Sharing.Part2.Avi2$wind_winter <- Bike.Sharing.Part2.Avi2$windspeed * Bike.Sharing.Part2.Avi2$winter_help

# Fit the linear regression models
model_without_interaction <- lm(casual ~ windspeed , data = Bike.Sharing.Part2.Avi2)
model_with_interaction <- lm(casual ~ wind_winter, data = Bike.Sharing.Part2.Avi2)

summary(model_without_interaction)
summary(model_with_interaction)

plot(Bike.Sharing.Part2.Avi2$windspeed[Bike.Sharing.Part2.Avi2$winter_help==0],
     Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$winter_help==0],
     xlab="Wind", ylab="Casual", main="Wind Of Winter", col="blue1")

points(Bike.Sharing.Part2.Avi2$windspeed[Bike.Sharing.Part2.Avi2$winter_help==1],
       Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$winter_help==1],
       col="red")

abline(model_without_interaction, col = "blue")
abline(model_with_interaction, col = "red")

# Adding a legend
legend("topright", legend = c("No Winter", "Winter"),
       col = c("blue", "red"), pch = 1)


# 2.4 5
# interaction var temp_holidy Using multiplication:
Bike.Sharing.Part2.Avi2$wind_spring <- Bike.Sharing.Part2.Avi2$windspeed * Bike.Sharing.Part2.Avi2$spring_help

# Fit the linear regression models
model_without_interaction <- lm(casual ~ windspeed , data = Bike.Sharing.Part2.Avi2)
model_with_interaction <- lm(casual ~ wind_spring, data = Bike.Sharing.Part2.Avi2)

summary(model_without_interaction)
summary(model_with_interaction)

plot(Bike.Sharing.Part2.Avi2$windspeed[Bike.Sharing.Part2.Avi2$spring_help==0],
     Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$spring_help==0],
     xlab="Registed", ylab="Casual", main="Wind Of Spring", col="blue1")

points(Bike.Sharing.Part2.Avi2$windspeed[Bike.Sharing.Part2.Avi2$spring_help==1],
       Bike.Sharing.Part2.Avi2$casual[Bike.Sharing.Part2.Avi2$spring_help==1],
       col="red")

# Add regression lines
abline(model_without_interaction, col = "blue")
abline(model_with_interaction, col = "red")

# Adding a legend
legend("topright", legend = c("No Spring", "Spring"),
       col = c("blue", "red"), pch = 1)



#3.1

y<-Bike.Sharing.Part2.Avi2$casual
x1<-Bike.Sharing.Part2.Avi2$temp
x2<-Bike.Sharing.Part2.Avi2$atemp
x3<-Bike.Sharing.Part2.Avi2$windspeed
x4<-Bike.Sharing.Part2.Avi2$registered
x5 <- Bike.Sharing.Part2.Avi2$mnth
x6 <- Bike.Sharing.Part2.Avi2$weekend_help
x7 <- Bike.Sharing.Part2.Avi2$holiday_help
x8 <- Bike.Sharing.Part2.Avi2$summer_help
x9 <- Bike.Sharing.Part2.Avi2$winter_help
x10 <- Bike.Sharing.Part2.Avi2$spring_help

#reading the data
fit<-lm(y~x1+x2+x3+x4+x5+x6 + x6 *x4 + x7 + x7*x1 + x8 + x9 + x10  + (x8 + x9 + x10 )*x3)

dataset<-Bike.Sharing.Part2.Avi2
  

#Empty model
Emp <- lm (y ~ 1,data=dataset)
summary(Emp) 

#Full model
Full <- lm (y~x1+x2+x3+x4+x5+x6 + x6 *x4 + x7 + x7*x1 + x8 + x9 + x10  + (x8 + x9 + x10 )*x3,data=dataset)
summary(Full) 


### FORWARD ###
fwd.model <-  step(Emp, direction='forward', scope= ~ x1+x2+x3+x4+x5+x6 + x6 *x4 + x7 + x7*x1 + x8 + x9 + x10  + (x8 + x9 + x10 )*x3
summary(fwd.model)
### BACKWARD ###
bw.model <-  step(Full, direction='backward', scope=~1)
summary(bw.model)

#3.2

fit<-lm(y~x1+x3+x4+x6+x7+x8+x9+x10+x4*x6+x1*x7+x3*x8+x3*x9,data=dataset )

#Variance equality
dataset$fitted <- fitted(fit)
dataset$residuals <- residuals(fit)
se <- sqrt(var(dataset$residuals))
dataset$stan_residuals <- (residuals(fit)/se)
plot(dataset$stan_residuals~ dataset$fitted, main="Residual Plot", xlab= "Fitted value" ,ylab= "Stan residuals")%>% abline(a=0, b=0, col="Blue")

gqtest(fit, alternative = "two.sided",data=Dataset)

#NORMAL
qqnorm(dataset$stan_residuals)
abline(a=0, b=1)
hist(dataset$stan_residuals, xlab ="Normalized error", main="Histogram of normalized error")

ks.test(x=dataset$stan_residuals,y="pnorm",alternative = "two.sided",exact=NULL)
shapiro.test(dataset$stan_residuals)


#linear
sctest(fit,type ="Chow")







