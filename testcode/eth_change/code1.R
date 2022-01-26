library(tidyverse) ;library(lubridate) ;library(MASS)

#------------------------ PREPARE ----------------------------  
## Loading the ETH-Change Closing Price data set
eth <- read.csv("C:/Users/nutti/junior/AUCC/eth_change/Ethereum Historical Datathis.csv",header=TRUE,stringsAsFactors = T)

inc_df <- eth %>% filter(Change > 0) %>% mutate(y = "increasing")
dec_df <- eth %>% filter(Change < 0) %>% mutate(y = "decreasing")

eth_df <- rbind(inc_df ,dec_df )

## Change Date format and sort Date 
data <- eth_df %>% mutate(Date = mdy(Date)) %>% arrange(Date)

data$y <- ifelse(data$y == "increasing",1,0)

attach(data)

library(ggplot2)
# scatterplot
ggplot(data, aes(x=Change_Per, y=Price)) + 
  geom_point(size=2,aes(col=factor(Change)))

# Boxplot
plot(factor(Change),Open,col=c("lightblue","lightgreen"),xlab="Change",ylab="Open")
plot(factor(Change),High,col=c("lightblue","lightgreen"),xlab="Change",ylab="High")
plot(factor(Change),Low,col=c("lightblue","lightgreen"),xlab="Change",ylab="Low")
plot(factor(Change),Price,col=c("lightblue","lightgreen"),xlab="Change",ylab="Price")
plot(factor(Change),Vol,col=c("lightblue","lightgreen"),xlab="Change",ylab="Vol")

cor(data[,3:17])
pairs(data[,3:17])

## Split Train and Test
index <- nrow(data)*0.8
train <- data[(1:index),]     
test <- data[-(1:index),] 

#------------------------ logistic model ---------------------------- 
fit.lm <- glm(y~close+open+high+low+vol ,data = train, family = "binomial")
summary(fit.lm)

glm.probs <- predict(fit.lm,newdata=test,type="response")
#glm.probs[1:30]

glm.pred <- rep("0",nrow(test))
glm.pred[glm.probs>0.5] <- "1"
glm.pred[1:6]

#Confusion Matrix
cf <- table(glm.pred,test$y)
cf
rowSums(cf)
colSums(cf)
(Accuracy <- mean(glm.pred == test$y))   # accuracy
(Precision <- cf[2,2]/(cf[1,2]+cf[2,2])) # precision
(Recall <- cf[2,2]/(cf[2,1]+cf[2,2]))    # recall
(F1 <- 2*(Precision*Recall)/(Precision+Recall))

#------------------------ ANN model ---------------------------- 
library(neuralnet)

fit.nn <- neuralnet(y~close+open+high+low+vol ,data = train, hidden=3,act.fct = "logistic",linear.output = FALSE)
summary(fit.nn)
plot(fit.nn)

nn.probs <- compute(fit.nn,test)

nn.pred <- rep("0",nrow(test))
nn.pred[nn.probs$net.result>0.5] <- "1"
nn.pred[1:6]

#Confusion Matrix
cf <- table(nn.pred,test$y)
cf
rowSums(cf)
colSums(cf)
(Accuracy <- mean(nn.pred == test$y))   # accuracy
(Precision <- cf[2,2]/(cf[1,2]+cf[2,2])) # precision
(Recall <- cf[2,2]/(cf[2,1]+cf[2,2]))    # recall
(F1 <- 2*(Precision*Recall)/(Precision+Recall))