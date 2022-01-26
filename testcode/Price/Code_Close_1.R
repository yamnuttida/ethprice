library(lubridate)
library(MASS)
library(moments)

eth = read.csv("C:/Users/nutti/junior/PROJECT_ETH/Price/Priceclose_2y_14.csv",header=TRUE)

## Change Date format and sort Date 
eth_df = eth %>% mutate(Date = mdy(Date)) %>% arrange(Date)

summary(eth)

## Split Train and Test
index <- nrow(eth_df)*0.8
train <- eth_df[(1:index),]     
test <- eth_df[-(1:index),] 

#model (not select)
reg <- lm(Close ~ Close_1+Open_1+High_1+Low_1+Vol_1+Change_1,data = train)
summary(reg)

par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

#transform
#model with log()
reg_l <- lm(log(Close)~ log(Close_1)+log(Open_1)+log(High_1)+log(Low_1)+log(Vol_1),data = train)
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
priceC_pred_1 <- c()

for (i in 1:nrow(train)) {
  d <- train[i,]
  
  #price predict
  coef <- as.numeric(step.model$coefficients[1:4])
  priceC_pred_1[i] <- coef[1]+(coef[2]*d$Close_1)+(coef[3]*d$High_1)+(coef[4]*d$Vol_1)
}

priceC_pred_1

#MSE/R^2
df_priceC_1 <- data.frame(train$Date,log(train$Close),priceC_pred_1)

MSE <- mean(summary(step.model)$residuals^2)
R2 <- cor(df_priceC_1$log.train.Close.,df_priceC_1$priceC_pred_1)^2 
RMSE <- sqrt(MSE)

MSE; R2; RMSE
