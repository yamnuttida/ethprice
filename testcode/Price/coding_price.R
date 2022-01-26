library(tidyverse) ;library(lubridate) ;library(MASS)

#------------------------  CHANGE CLOSING PRICE ----------------------------  
## Loading the ETH-Change Closing Price data set
eth = read.csv("C:/Users/nutti/junior/PROJECT_ETH/Price/Price_High.csv",header=TRUE)
eth_n <- data.frame(eth[,2],eth[,3])
x <- matrix(eth)
eth$High <- scale(eth$High , center = T,scale = T)
eth$Close_1 <- scale(eth$Close_1 , center = T,scale = T)
eth$Open_1 <- scale(eth$Open_1 , center = T,scale = T)
eth$High_1 <- scale(eth$High_1 , center = T,scale = T)
eth$Low_1 <- scale(eth$Low_1 , center = T,scale = T)
eth$Vol_1 <- scale(eth$Vol_1 , center = T,scale = T)
eth$Change_1 <- scale(eth$Change_1 , center = T,scale = T)

eth$Close_2 <- scale(eth$Close_1 , center = T,scale = T)
eth$Open_2 <- scale(eth$Open_1 , center = T,scale = T)
eth$High_2 <- scale(eth$High_1 , center = T,scale = T)
eth$Low_1 <- scale(eth$Low_1 , center = T,scale = T)
eth$Vol_1 <- scale(eth$Vol_1 , center = T,scale = T)
eth$Change_1 <- scale(eth$Change_1 , center = T,scale = T)

eth$Close_1 <- scale(eth$Close_1 , center = T,scale = T)
eth$Open_1 <- scale(eth$Open_1 , center = T,scale = T)
eth$High_1 <- scale(eth$High_1 , center = T,scale = T)
eth$Low_1 <- scale(eth$Low_1 , center = T,scale = T)
eth$Vol_1 <- scale(eth$Vol_1 , center = T,scale = T)
eth$Change_1 <- scale(eth$Change_1 , center = T,scale = T)

eth$Close_1 <- scale(eth$Close_1 , center = T,scale = T)
eth$Open_1 <- scale(eth$Open_1 , center = T,scale = T)
eth$High_1 <- scale(eth$High_1 , center = T,scale = T)
eth$Low_1 <- scale(eth$Low_1 , center = T,scale = T)
eth$Vol_1 <- scale(eth$Vol_1 , center = T,scale = T)
eth$Change_1 <- scale(eth$Change_1 , center = T,scale = T)

eth$Close_1 <- scale(eth$Close_1 , center = T,scale = T)
eth$Open_1 <- scale(eth$Open_1 , center = T,scale = T)
eth$High_1 <- scale(eth$High_1 , center = T,scale = T)
eth$Low_1 <- scale(eth$Low_1 , center = T,scale = T)
eth$Vol_1 <- scale(eth$Vol_1 , center = T,scale = T)
eth$Change_1 <- scale(eth$Change_1 , center = T,scale = T)



## Change Date format and sort Date 
change_close_df = eth %>% mutate(Date = dmy(Date)) %>% arrange(Date)

## Split Train and Test
index <- nrow(eth_change_close)*0.8
train_cc_df <- change_close_df[(1:index),]     
test_cc_df <- change_close_df[-(1:index),] 

library(ggcorrplot) 
#corr <- cor(change_close_df[,-c(1)])
#ggcorrplot(corr,type = "lower",lab = T, method = "square",digits = 1)


## Plot Train and Test data set
g <- change_close_df %>% ggplot(aes(Date, High)) + theme_bw()
g + geom_line(data=train_cc_df, aes(x = Date, y = High)) + 
  geom_line(data=test_cc_df, aes(x = Date, y = High),color ="red",linetype = "dashed")+ 
  labs(title = 'ETH - High Price', y = "", x = "") + 
  geom_vline(xintercept = ymd(test_cc_df$Date[1]), color = "red",size=1) + 
  scale_y_continuous(breaks = c(0, 1250, 2500 , 3750, 5000), 
                     labels = c('$0', '$1,250','2,500', '$3,750', '$5,000'))
  scale_y_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40), 
                     labels = c('-40','-30','-20','-10','0','10','20','30','40'))
