library(lubridate)
library(MASS)
library(moments)

#import data
datClose10 <- read.csv('D:/study/Y3.1/AUCC/FE+SML+EDA/dataset/Priceclose_2y_10.csv',header = T)
head(as.data.frame(datClose10))

#change chr -> date
datClose10$Date <- mdy(datClose10$Date)
head(datClose10)
str(datClose10)

#split data
train <- datClose10[1:586,]      
test <- datClose10[587:732,] 

#model (not select)
reg <- lm(Close ~ Close_1+Open_1+High_1+Low_1+Vol_1+Change_1+Close_2+Open_2+High_2+Low_2+Vol_2+Change_2+Close_3+Open_3+High_3+Low_3+Vol_3+Change_3+Close_4+Open_4+High_4+Low_4+Vol_4+Change_4+Close_5+Open_5+High_5+Low_5+Vol_5+Change_5+
            Close_6+Open_6+High_6+Low_6+Vol_6+Change_6+Close_7+Open_7+High_7+Low_7+Vol_7+Change_7+Close_8+Open_8+High_8+Low_8+Vol_8+Change_8+Close_9+Open_9+High_9+Low_9+Vol_9+Change_9+Close_10+Open_10+High_10+Low_10+Vol_10+Change_10,data = train)
summary(reg)

par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

#transform
#model with log()
reg_l <- lm(log(Close)~ log(Close_1)+log(Open_1)+log(High_1)+log(Low_1)+log(Vol_1)+log(Close_2)+log(Open_2)+log(High_2)+log(Low_2)+log(Vol_2)+log(Close_3)+log(Open_3)+log(High_3)+log(Low_3)+log(Vol_3)+log(Close_4)+log(Open_4)+log(High_4)+log(Low_4)+log(Vol_4)+
              log(Close_5)+log(Open_5)+log(High_5)+log(Low_5)+log(Vol_5)+log(Close_6)+log(Open_6)+log(High_6)+log(Low_6)+log(Vol_6)+log(Close_7)+log(Open_7)+log(High_7)+log(Low_7)+log(Vol_7)+log(Close_8)+log(Open_8)+log(High_8)+log(Low_8)+log(Vol_8)+
              log(Close_9)+log(Open_9)+log(High_9)+log(Low_9)+log(Vol_9)+log(Close_10)+log(Open_10)+log(High_10)+log(Low_10)+log(Vol_10),data = train)
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
priceC_pred_10 <- c()

for (i in 1:nrow(train)) {
  d <- train[i,]
  
  #price predict
  coef <- as.numeric(step.model$coefficients[1:4])
  priceC_pred_10[i] <- coef[1]+(coef[2]*d$Close_1)+(coef[3]*d$High_1)+(coef[4]*d$Vol_1)
}

priceC_pred_10

#MSE/R^2
df_priceC_10 <- data.frame(train$Date,log(train$Close),priceC_pred_10)

MSE <- mean(summary(step.model)$residuals^2)
R2 <- cor(df_priceC_10$log.train.Close.,df_priceC_10$priceC_pred_10)^2 
RMSE <- sqrt(MSE)

MSE; R2; RMSE
