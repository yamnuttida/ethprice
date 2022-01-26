library(lubridate)
library(MASS)
library(moments)

#import data
datClose5 <- read.csv('D:/study/Y3.1/AUCC/FE+SML+EDA/dataset/Priceclose_2y_5.csv',header = T)
head(as.data.frame(datClose5))

#change chr -> date
datClose5$Date <- mdy(datClose5$Date)
head(datClose5)
str(datClose5)

#split data
train <- datClose5[1:586,]      
test <- datClose5[587:732,] 

#model (not select)
reg <- lm(Close ~ Close_1+Open_1+High_1+Low_1+Vol_1+Change_1+Close_2+Open_2+High_2+Low_2+Vol_2+Change_2+Close_3+Open_3+High_3+Low_3+Vol_3+Change_3+Close_4+Open_4+High_4+Low_4+Vol_4+Change_4+Close_5+Open_5+High_5+Low_5+Vol_5+Change_5,data = train)
summary(reg)

par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

#transform
#model with log()
reg_l <- lm(log(Close)~ log(Close_1)+log(Open_1)+log(High_1)+log(Low_1)+log(Vol_1)+log(Close_2)+log(Open_2)+log(High_2)+log(Low_2)+log(Vol_2)+log(Close_3)+log(Open_3)+log(High_3)+log(Low_3)+log(Vol_3)+log(Close_4)+log(Open_4)+log(High_4)+log(Low_4)+log(Vol_4)+log(Close_5)+log(Open_5)+log(High_5)+log(Low_5)+log(Vol_5),data = train)
summary(reg_l)

par(mfrow=c(2,2))
plot(reg_l)
par(mfrow=c(1,1))

#model (selected features)
step.model <- stepAIC(reg_l, direction = "both", trace = FALSE)
summary(step.model)

par(mfrow=c(2,2))
plot(step.model)
par(mfrow=c(1,1))

shapiro.test(reg_l$residuals)
ad.test(reg_l$residuals)

#แทนค่า test ในสมการ
priceC_pred_5 <- c()

for (i in 1:nrow(train)) {
  d <- train[i,]
  
  #price predict
  coef <- as.numeric(step.model$coefficients[1:4])
  priceC_pred_5[i] <- coef[1]+(coef[2]*d$Close_1)+(coef[3]*d$High_1)+(coef[4]*d$Vol_1)
}

priceC_pred_5

#MSE/R^2
df_priceC_5 <- data.frame(train$Date,log(train$Close),priceC_pred_5)

MSE <- mean(summary(step.model)$residuals^2)
R2 <- cor(df_priceC_5$log.train.Close.,df_priceC_5$priceC_pred_5)^2 
RMSE <- sqrt(MSE)

MSE; R2; RMSE
