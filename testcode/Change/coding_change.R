library(tidyverse) ;library(lubridate) ;library(MASS)

## Loading the ETH-Change Closing Price data set
eth_change_close = read.csv("C:/Users/nutti/junior/PROJECT_ETH/Change/Change_High.csv",header=TRUE)

## Change Date format and sort Date 
change_close_df = eth_change_close %>% mutate(Date = dmy(Date)) %>% arrange(Date)

## Split Train and Test
index <- nrow(eth_change_close)*0.8
train_cc_df <- change_close_df[(1:index),]     
test_cc_df <- change_close_df[-(1:index),] 

library(ggcorrplot) 
#corr <- cor(change_close_df[,-c(1)])
#ggcorrplot(corr,type = "lower",lab = T, method = "square",digits = 1)


## Plot Train and Test data set
g1 <- change_close_df %>% 
  ggplot(aes(Date, Change.H)) + theme_bw()
g1 + geom_line(data=train_cc_df, aes(x = Date, y = Change.C)) + 
  geom_line(data=test_cc_df, aes(x = Date, y = Change.H),color ="red",linetype = "dashed")+ 
  labs(title = 'Change Closing Price', y = "", x = "") + 
  geom_vline(xintercept = ymd(test_cc_df$Date[1]), color = "red",size=1) + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change Closing Price DAY-1 Model 
model_cc <- lm(Change.H ~ C_1+O_1+H_1+L_1+V_1, data = train_cc_df)
summary(model_cc)


## Stepwise selection
model_cc_AIC <- stepAIC(model_cc, direction = "both", trace = FALSE)
summary(model_cc_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2))
plot(model_cc_AIC)
par(mfrow=c(1,1))

## Predicted values 
test.value.Cc <- test_cc_df$Change.H
pred.Cc <- predict(model_cc_AIC,newdata=test_cc_df) 
# Computed RMSE
residual.Cc <- test_cc_df$Change.H - pred.Cc
MSE.Cc  <- mean(residual.Cc^2)
RMSE.Cc <- sqrt(MSE.Cc)

price.test <- data.frame(test_cc_df$Date,test_cc_df$Change.H,pred.Cc )
names(price.test)[1] = 'Date'
names(price.test)[2] = 'Actual'
names(price.test)[3] = 'Change.H'

## Plot test Predict
g2 <- test_cc_df %>% 
  ggplot(aes(Date, Change.H)) + theme_bw()
g2 + geom_line(data=test_cc_df, aes(x = Date, y = Change.H)) + 
  geom_line(data=price.test, aes(x = Date, y = Change.H),color ="red")+ 
  labs(title = 'Change Closing Price', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

