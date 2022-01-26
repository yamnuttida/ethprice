library(tidyverse) ;library(lubridate) ;library(MASS)

#------------------------  CHANGE CLOSING PRICE - D1 ----------------------------  
## Loading the ETH-Change Closing Price data set
change_d1 = read.csv("C:/Users/nutti/junior/PROJECT_ETH/Change/Changeclose_2y_d14.csv",header=TRUE)

## Change Date format and sort Date 
change_d1_df = change_d1 %>% mutate(Date = mdy(Date)) %>% arrange(Date)

## Split Train and Test
index <- nrow(change_d1_df)*0.8
train_d1_df <- change_d1_df[(1:index),]     
test_d1_df <- change_d1_df[-(1:index),] 

#library(ggcorrplot) 
#corr <- cor(test_d1_df[,-c(1)])
#ggcorrplot(corr,type = "lower",lab = T, method = "square",digits = 3)

## Plot Train and Test data set
g_d1 <- change_d1_df %>% ggplot(aes(Date, Change)) + theme_bw()
g_d1 + geom_line(data=train_d1_df, aes(x = Date, y = Change)) + 
  geom_line(data=test_d1_df, aes(x = Date, y = Change),color ="red",linetype = "dashed") + 
  labs(title = 'Change Closing Price - D1', y = "", x = "") + 
  geom_vline(xintercept = ymd(test_d1_df$Date[1]), color = "red",size=1) + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change Closing Price - d1 Model 
model_d1 <- lm(Change ~ Close_1+Open_1+High_1+Low_1+Vol_1+Change_1, data = train_d1_df) ;summary(model_d1)

## Stepwise selection
model_d1_AIC <- stepAIC(model_d1, direction = "both", trace = FALSE) ;summary(model_d1_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_d1_AIC);mtext("Change Closing Price - d1", outer=TRUE,line=-2 ) ;par(mfrow=c(1,1))

## Predicted values 
pred.d1 <- predict(model_d1_AIC,newdata=test_d1_df) 
# Computed RMSE
residual.d1 <- test_d1_df$Change - pred.d1
MSE.d1  <- mean(residual.d1^2) ;RMSE.d1 <- sqrt(MSE.d1)

pred.d1.table <- data.frame(test_d1_df$Date,test_d1_df$Change, pred.d1)
names(pred.d1.table)[1] = 'Date' ;names(pred.d1.table)[2] = 'Actual' ;names(pred.d1.table)[3] = 'Predict'

## Plot test Predicted
gp_d1 <- test_d1_df %>% ggplot(aes(Date, Change)) + theme_bw()
gd1 <- gp_d1 + geom_line(data=test_d1_df, aes(x = Date, y = Change)) + 
  geom_line(data = pred.d1.table, aes(x = Date, y = Predict,color ="red"))+ 
  labs(title = 'Change Closing Price - d1', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

#------------------------  CHANGE CLOSING PRICE - D14 ---------------------------- 

## Loading the ETH-Change Closing Price data set
change_d14 = read.csv("C:/Users/nutti/junior/PROJECT_ETH/Change/Changeclose_2y_d14.csv",header=TRUE)

## Change Date format and sort Date 
change_d14_df = change_d14 %>% mutate(Date = mdy(Date)) %>% arrange(Date)

## Split Train and Test
index <- nrow(change_d14_df)*0.8
train_d14_df <- change_d14_df[(1:index),]     
test_d14_df <- change_d14_df[-(1:index),] 

#library(ggcorrplot) 
#corr <- cor(test_d14_df[,-c(1)])
#ggcorrplot(corr,type = "lower",lab = T, method = "square",digits = 3)

## Plot Train and Test data set
g_d14 <- change_d14_df %>% ggplot(aes(Date, Change)) + theme_bw()
g_d14 + geom_line(data=train_d14_df, aes(x = Date, y = Change)) + 
  geom_line(data=test_d14_df, aes(x = Date, y = Change),color ="red",linetype = "dashed") + 
  labs(title = 'Change Closing Price - d14', y = "", x = "") + 
  geom_vline(xintercept = ymd(test_d14_df$Date[1]), color = "red",size=1) + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change Closing Price - d14 Model 
model_d14 <- lm(Change ~ . -Date, data = train_d14_df) ;summary(model_d14)

## Stepwise selection
model_d14_AIC <- stepAIC(model_d14, direction = "both", trace = FALSE) ;summary(model_d14_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_d14_AIC);mtext("Change Closing Price - d14", outer=TRUE,line=-2 ) ;par(mfrow=c(1,1))

## Predicted values 
pred.d14 <- predict(model_d14_AIC,newdata=test_d14_df) 
# Computed RMSE
residual.d14 <- test_d14_df$Change - pred.d14
MSE.d14  <- mean(residual.d14^2) ;RMSE.d14 <- sqrt(MSE.d14)

pred.d14.table <- data.frame(test_d14_df$Date,test_d14_df$Change, pred.d14)
names(pred.d14.table)[1] = 'Date' ;names(pred.d14.table)[2] = 'Actual' ;names(pred.d14.table)[3] = 'Predict'

## Plot test Predicted
gp_d14 <- test_d14_df %>% ggplot(aes(Date, Change)) + theme_bw()
gd14 <- gp_d14 + geom_line(data=test_d14_df, aes(x = Date, y = Change)) + 
  geom_line(data = pred.d14.table, aes(x = Date, y = Predict,color ="red"))+ 
  labs(title = 'Change Closing Price - d14', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

R2_d1 <- cor(pred.d1.table$Actual,pred.d1.table$Predict)^2
R2_d14 <- cor(pred.d14.table$Actual,pred.d14.table$Predict)^2
