library(tidyverse) ;library(lubridate) ;library(MASS)

#------------------------  CHANGE CLOSING PRICE ----------------------------  
## Loading the ETH-Change Closing Price data set
eth_change_close = read.csv("C:/Users/nutti/junior/PROJECT_ETH/Change/Change_Close.csv",header=TRUE)

## Change Date format and sort Date 
change_close_df = eth_change_close %>% mutate(Date = dmy(Date)) %>% arrange(Date)

## Split Train and Test
index <- nrow(eth_change_close)*0.8
train_cc_df <- change_close_df[(1:index),]     
test_cc_df <- change_close_df[-(1:index),] 

pairs(change_close_df[,c(2:3)])
corr <- cor(change_close_df[,c(2:3)])
ggcorrplot(corr,type = "lower",lab = T,lab_size = 4, method = "square",digits = 4)

library(ggcorrplot) 
#corr <- cor(change_close_df[,-c(1)])
#ggcorrplot(corr,type = "lower",lab = T, method = "square",digits = 1)

## Plot Train and Test data set
g <- change_close_df %>% ggplot(aes(Date, Change.C)) + theme_bw()
g + geom_line(data=train_cc_df, aes(x = Date, y = Change.C)) + 
  geom_line(data=test_cc_df, aes(x = Date, y = Change.C),color ="red",linetype = "dashed")+ 
  labs(title = 'Change Closing Price', y = "", x = "") + 
  geom_vline(xintercept = ymd(test_cc_df$Date[1]), color = "red",size=1) + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change Closing Price DAY-1 Model 
model_cc1 <- lm(Change.C ~ .-Date, data = train_cc_df) ;summary(model_cc1)

## Stepwise selection
model_cc1_AIC <- stepAIC(model_cc1, direction = "both", trace = FALSE) ;summary(model_cc1_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_cc1_AIC);mtext("1", outer=TRUE,line=-2 ) ;par(mfrow=c(1,1))

## Predicted values 
test.value.cc <- test_cc_df$Change.C
pred.cc1 <- predict(model_cc1_AIC,newdata=test_cc_df) 
# Computed RMSE
residual.cc1 <- test_cc_df$Change.C - pred.cc1
MSE.cc1  <- mean(residual.cc1^2) ;RMSE.cc1 <- sqrt(MSE.cc1)

predict1c.test <- data.frame(test_cc_df$Date,test_cc_df$Change.C,pred.cc1 )
names(predict1c.test)[1] = 'Date' ;names(predict1c.test)[2] = 'Actual' ;names(predict1c.test)[3] = 'Change.C'

## Plot test Predicted
gc_1 <- test_cc_df %>%ggplot(aes(Date, Change.C)) + theme_bw()
gc1 <- gc_1 + geom_line(data=test_cc_df, aes(x = Date, y = Change.C)) + 
  geom_line(data=predict1c.test, aes(x = Date, y = Change.C),color ="red")+ 
  labs(title = 'Change Closing Price D-1', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

R2_d14_1 <- cor(predict1c.test$Actual,predict1c.test$Change.C)^2

## fit Change Closing Price DAY-3 Model --------
model_cc3 <- lm(Change.C ~ C_1+O_1+H_1+L_1+V_1 + C_2+O_2+H_2+L_2+V_2 + C_3+O_3+H_3+L_3+V_3, data = train_cc_df) ;summary(model_cc3)

## Stepwise selection
model_cc3_AIC <- stepAIC(model_cc3, direction = "both", trace = FALSE) ;summary(model_cc3_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_cc3_AIC);mtext("Change Closing Price D-3", outer=TRUE,line=-2 ) ;par(mfrow=c(1,1))

## Predicted values 
test.value.cc <- test_cc_df$Change.C
pred.cc3 <- predict(model_cc3_AIC,newdata=test_cc_df) 
# Computed RMSE
residual.cc3 <- test_cc_df$Change.C - pred.cc3
MSE.cc3  <- mean(residual.cc3^2) ;RMSE.cc3 <- sqrt(MSE.cc3)

predict3c.test <- data.frame(test_cc_df$Date,test_cc_df$Change.C,pred.cc3 )
names(predict3c.test)[1] = 'Date' ;names(predict3c.test)[2] = 'Actual' ;names(predict3c.test)[3] = 'Change.C'

## Plot test Predicted
gc_3 <- test_cc_df %>%ggplot(aes(Date, Change.C)) + theme_bw()
gc3 <- gc_3 + geom_line(data=test_cc_df, aes(x = Date, y = Change.C)) + 
  geom_line(data=predict3c.test, aes(x = Date, y = Change.C),color ="red")+ 
  labs(title = 'Change Closing Price D-3', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))


## fit Change Closing Price DAY-5 Model --------
model_cc5 <- lm(Change.C ~ C_1+O_1+H_1+L_1+V_1 + C_2+O_2+H_2+L_2+V_2 + C_3+O_3+H_3+L_3+V_3 + C_4+O_4+H_4+L_4+V_4 + C_5+O_5+H_5+L_5+V_5, data = train_cc_df) ;summary(model_cc5)

## Stepwise selection
model_cc5_AIC <- stepAIC(model_cc5, direction = "both", trace = FALSE) ;summary(model_cc5_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_cc5_AIC);mtext("Change Closing Price D-5", outer=TRUE,line=-2 ) ;par(mfrow=c(1,1))

## Predicted values 
test.value.cc <- test_cc_df$Change.C
pred.cc5 <- predict(model_cc5_AIC,newdata=test_cc_df) 
# Computed RMSE
residual.cc5 <- test_cc_df$Change.C - pred.cc5
MSE.cc5  <- mean(residual.cc5^2) ;RMSE.cc5 <- sqrt(MSE.cc5)

predict5c.test <- data.frame(test_cc_df$Date,test_cc_df$Change.C,pred.cc5 )
names(predict5c.test)[1] = 'Date' ;names(predict5c.test)[2] = 'Actual' ;names(predict5c.test)[3] = 'Change.C'

## Plot test Predicted
gc_5 <- test_cc_df %>%ggplot(aes(Date, Change.C)) + theme_bw()
gc5 <- gc_5 + geom_line(data=test_cc_df, aes(x = Date, y = Change.C)) + 
  geom_line(data=predict5c.test, aes(x = Date, y = Change.C),color ="red")+ 
  labs(title = 'Change Closing Price D-5', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))




#------------------------  CHANGE HIGH PRICE ---------------------------- 
## Loading the ETH-Change Closing Price data set
eth_change_high = read.csv("C:/Users/nutti/junior/PROJECT_ETH/Change/Change_High.csv",header=TRUE)

## Change Date format and sort Date 
change_high_df = eth_change_high %>% mutate(Date = dmy(Date)) %>% arrange(Date)

## Split Train and Test
index <- nrow(change_high_df)*0.8
train_ch_df <- change_high_df[(1:index),]     
test_ch_df <- change_high_df[-(1:index),] 

library(ggcorrplot) 
#corr <- cor(change_high_df[,-c(1)])
#ggcorrplot(corr,type = "lower",lab = T, method = "square",digits = 1)


## Plot Train and Test data set
g <- change_high_df %>% ggplot(aes(Date, Change.H)) + theme_bw()
g + geom_line(data=train_ch_df, aes(x = Date, y = Change.H)) + 
  geom_line(data=test_ch_df, aes(x = Date, y = Change.H),color ="red",linetype = "dashed") + 
  labs(title = 'Change High Price D-1', y = "", x = "") + 
  geom_vline(xintercept = ymd(test_ch_df$Date[1]), color = "red",size=1) + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change High Price DAY-1 Model --------
model_ch1 <- lm(Change.H ~ C_1+O_1+H_1+L_1+V_1, data = train_ch_df) ;summary(model_ch1)

## Stepwise selection
model_ch1_AIC <- stepAIC(model_ch1, direction = "both", trace = FALSE) ;summary(model_ch1_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_ch1_AIC);mtext("Change High Price D-1", outer=TRUE,line=-2 ) ;par(mfrow=c(1,1))

## Predicted values 
test.value.ch <- test_ch_df$Change.H
pred.ch1 <- predict(model_ch1_AIC,newdata=test_ch_df) 
# Computed RMSE
residual.ch1 <- test_ch_df$Change.H - pred.ch1
MSE.ch1  <- mean(residual.ch1^2) ;RMSE.ch1 <- sqrt(MSE.ch1)

predict1h.test <- data.frame(test_ch_df$Date,test_ch_df$Change.H,pred.ch1 )
names(predict1h.test)[1] = 'Date' ;names(predict1h.test)[2] = 'Actual' ;names(predict1h.test)[3] = 'Change.H'

## Plot test Predicted
gh_1 <- test_ch_df %>%ggplot(aes(Date, Change.H)) + theme_bw()
gh1 <- gh_1 + geom_line(data=test_ch_df, aes(x = Date, y = Change.H)) + 
  geom_line(data=predict1h.test, aes(x = Date, y = Change.H),color ="red")+ 
  labs(title = 'Change High Price D-1', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change High Price DAY-3 Model --------
model_ch3 <- lm(Change.H ~ C_1+O_1+H_1+L_1+V_1 + C_2+O_2+H_2+L_2+V_2 + C_3+O_3+H_3+L_3+V_3, data = train_ch_df) ;summary(model_ch3)

## Stepwise selection
model_ch3_AIC <- stepAIC(model_ch3, direction = "both", trace = FALSE) ;summary(model_ch3_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_ch3_AIC);mtext("Change High Price D-3", outer=TRUE,line=-2 )  ;par(mfrow=c(1,1))

## Predicted values 
test.value.ch <- test_ch_df$Change.H
pred.ch3 <- predict(model_ch3_AIC,newdata=test_ch_df) 
# Computed RMSE
residual.ch3 <- test_ch_df$Change.H - pred.ch3
MSE.ch3  <- mean(residual.ch3^2) ;RMSE.ch3 <- sqrt(MSE.ch3)

predict3h.test <- data.frame(test_ch_df$Date,test_ch_df$Change.H,pred.ch3 )
names(predict3h.test)[1] = 'Date' ;names(predict3h.test)[2] = 'Actual' ;names(predict3h.test)[3] = 'Change.H'

## Plot test Predicted
gh_3 <- test_ch_df %>%ggplot(aes(Date, Change.H)) + theme_bw()
gh3 <- gh_3 + geom_line(data=test_ch_df, aes(x = Date, y = Change.H)) + 
  geom_line(data=predict3h.test, aes(x = Date, y = Change.H),color ="red")+ 
  labs(title = 'Change High Price D-3', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change High Price DAY-5 Model --------
model_ch5 <- lm(Change.H ~ C_1+O_1+H_1+L_1+V_1 + C_2+O_2+H_2+L_2+V_2 + C_3+O_3+H_3+L_3+V_3 + C_4+O_4+H_4+L_4+V_4 + C_5+O_5+H_5+L_5+V_5, data = train_ch_df) ;summary(model_ch5)

## Stepwise selection
model_ch5_AIC <- stepAIC(model_ch5, direction = "both", trace = FALSE) ;summary(model_ch5_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_ch5_AIC);mtext("Change High Price D-5", outer=TRUE,line=-2 )  ;par(mfrow=c(1,1))

## Predicted values 
test.value.ch <- test_ch_df$Change.H
pred.ch5 <- predict(model_ch5_AIC,newdata=test_ch_df) 
# Computed RMSE
residual.ch5 <- test_ch_df$Change.H - pred.ch5
MSE.ch5  <- mean(residual.ch5^2) ;RMSE.ch5 <- sqrt(MSE.ch5)

predict5h.test <- data.frame(test_ch_df$Date,test_ch_df$Change.H,pred.ch5 )
names(predict5h.test)[1] = 'Date' ;names(predict5h.test)[2] = 'Actual' ;names(predict5h.test)[3] = 'Change.H'

## Plot test Predicted
gh_5 <- test_ch_df %>%ggplot(aes(Date, Change.H)) + theme_bw()
gh5 <- gh_5 + geom_line(data=test_ch_df, aes(x = Date, y = Change.H)) + 
  geom_line(data=predict5h.test, aes(x = Date, y = Change.H),color ="red")+ 
  labs(title = 'Change High Price D-5', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))


#------------------------  CHANGE LOW PRICE ---------------------------- 
## Loading the ETH-Change Closing Price data set
eth_change_low = read.csv("C:/Users/nutti/junior/PROJECT_ETH/Change/Change_Low.csv",header=TRUE)

## Change Date format and sort Date 
change_low_df = eth_change_low %>% mutate(Date = dmy(Date)) %>% arrange(Date)

## Split Train and Test
index <- nrow(change_low_df)*0.8
train_cl_df <- change_low_df[(1:index),]     
test_cl_df <- change_low_df[-(1:index),] 

library(ggcorrplot) 
#corr <- cor(change_high_df[,-c(1)])
#ggcorrplot(corr,type = "lower",lab = T, method = "square",digits = 1)


## Plot Train and Test data set
g <- change_low_df %>% ggplot(aes(Date, Change.L)) + theme_bw()
g + geom_line(data=train_cl_df, aes(x = Date, y = Change.L)) + 
  geom_line(data=test_cl_df, aes(x = Date, y = Change.L),color ="red",linetype = "dashed") + 
  labs(title = 'Change Low Price', y = "", x = "") + 
  geom_vline(xintercept = ymd(test_cl_df$Date[1]), color = "red",size=1) + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change Low Price DAY-1 Model --------
model_cl1 <- lm(Change.L ~ C_1+O_1+H_1+L_1+V_1, data = train_cl_df) ;summary(model_cl1)

## Stepwise selection
model_cl1_AIC <- stepAIC(model_cl1, direction = "both", trace = FALSE) ;summary(model_cl1_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_cl1_AIC);mtext("Change Low Price D-1", outer=TRUE,line=-2 )  ;par(mfrow=c(1,1))

## Predicted values 
test.value.cl <- test_ch_df$Change.L
pred.cl1 <- predict(model_cl1_AIC,newdata=test_cl_df) 
# Computed RMSE
residual.cl1 <- test_cl_df$Change.L - pred.cl1
MSE.cl1  <- mean(residual.cl1^2) ;RMSE.cl1 <- sqrt(MSE.cl1)

predict1l.test <- data.frame(test_cl_df$Date,test_cl_df$Change.L,pred.cl1 )
names(predict1l.test)[1] = 'Date' ;names(predict1l.test)[2] = 'Actual' ;names(predict1l.test)[3] = 'Change.L'

## Plot test Predicted
gl_1 <- test_cl_df %>%ggplot(aes(Date, Change.L)) + theme_bw()
gl1 <- gl_1 + geom_line(data=test_cl_df, aes(x = Date, y = Change.L)) + 
  geom_line(data=predict1l.test, aes(x = Date, y = Change.L),color ="red")+ 
  labs(title = 'Change Low Price D-1', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change Low Price DAY-3 Model --------
model_cl3 <- lm(Change.L ~ C_1+O_1+H_1+L_1+V_1 + C_2+O_2+H_2+L_2+V_2 + C_3+O_3+H_3+L_3+V_3, data = train_cl_df) ;summary(model_cl3)

## Stepwise selection
model_cl3_AIC <- stepAIC(model_cl3, direction = "both", trace = FALSE) ;summary(model_cl3_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_cl3_AIC);mtext("Change Low Price D-3", outer=TRUE,line=-2 ) ;par(mfrow=c(1,1))

## Predicted values 
test.value.cl <- test_cl_df$Change.L
pred.cl3 <- predict(model_cl3_AIC,newdata=test_cl_df) 
# Computed RMSE
residual.cl3 <- test_cl_df$Change.L - pred.cl3
MSE.cl3  <- mean(residual.cl3^2) ;RMSE.cl3 <- sqrt(MSE.cl3)

predict3l.test <- data.frame(test_cl_df$Date,test_cl_df$Change.L,pred.cl3 )
names(predict3l.test)[1] = 'Date' ;names(predict3l.test)[2] = 'Actual' ;names(predict3l.test)[3] = 'Change.L'

## Plot test Predicted
gl_3 <- test_cl_df %>%ggplot(aes(Date, Change.L)) + theme_bw()
gl3 <- gl_3 + geom_line(data=test_cl_df, aes(x = Date, y = Change.L)) + 
  geom_line(data=predict3l.test, aes(x = Date, y = Change.L),color ="red")+ 
  labs(title = 'Change Low Price D-3', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))

## fit Change Low Price DAY-5 Model --------
model_cl5 <- lm(Change.L ~ C_1+O_1+H_1+L_1+V_1 + C_2+O_2+H_2+L_2+V_2 + C_3+O_3+H_3+L_3+V_3 + C_4+O_4+H_4+L_4+V_4 + C_5+O_5+H_5+L_5+V_5, data = train_cl_df) ;summary(model_cl5)

## Stepwise selection
model_cl5_AIC <- stepAIC(model_cl5, direction = "both", trace = FALSE) ;summary(model_cl5_AIC)

## Plot Residual for check assumptions MLR
par(mfrow = c(2,2)) ;plot(model_cl5_AIC);mtext("Change Low Price D-5", outer=TRUE,line=-2 ) ;par(mfrow=c(1,1))

## Predicted values 
test.value.cl <- test_cl_df$Change.L
pred.cl5 <- predict(model_cl5_AIC,newdata=test_cl_df) 
# Computed RMSE
residual.cl5 <- test_cl_df$Change.L - pred.cl5
MSE.cl5  <- mean(residual.cl5^2) ;RMSE.cl5 <- sqrt(MSE.cl5)

predict5l.test <- data.frame(test_cl_df$Date,test_cl_df$Change.L,pred.cl5 )
names(predict5l.test)[1] = 'Date' ;names(predict5l.test)[2] = 'Actual' ;names(predict5l.test)[3] = 'Change.L'

## Plot test Predicted
gl_5 <- test_cl_df %>%ggplot(aes(Date, Change.L)) + theme_bw()
gl5 <- gl_5 + geom_line(data=test_cl_df, aes(x = Date, y = Change.L)) + 
  geom_line(data=predict5l.test, aes(x = Date, y = Change.L),color ="red")+ 
  labs(title = 'Change Low Price D-5', y = "", x = "") + 
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))
