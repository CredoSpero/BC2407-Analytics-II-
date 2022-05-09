# =======================================================================================================
# ============================== DATA PREPARATION =======================================================
library(data.table)
library(rpart)
library(rpart.plot) 
library(caTools)
library(ggplot2)
library(neuralnet)
library(tibble)
library(randomForest)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(arules)

setwd('C:\\Users\\YI XUAN\\Desktop\\NTU\\AY2021-2022 SEM 2\\BC2407 Analytics II\\Group Project')
marketing_data = read.csv("marketing_data_cleaned.csv", stringsAsFactors = T)
str(marketing_data)

# Converting some variables to categorical 
marketing_data$Gender = as.factor(marketing_data$Gender)
marketing_data$SubscriptionPaymentMethod = as.factor(marketing_data$SubscriptionPaymentMethod)
marketing_data$Response = as.factor(marketing_data$Response)

# Re-leveling to make "Single" as baseline for MaritalStatus & "Basic" as baseline for Education
marketing_data$MaritalStatus = relevel(marketing_data$MaritalStatus, "Single")
marketing_data$Education = relevel(marketing_data$Education, "Basic")
summary(marketing_data)
str(marketing_data)

# =======================================================================================================
# =================================== DATA EXPLORATION ==================================================

# For Continuous Variables 
ggplot(data = marketing_data, aes(x =  Response,y = Age , fill=Response)) +
  geom_boxplot() + labs(title = "Age vs Response")
ggplot(data = marketing_data, aes(x =  Response,y = Income , fill=Response)) +
  geom_boxplot() + labs(title = "Income vs Response")
ggplot(data = marketing_data, aes(x =  Response, y = NumAdsViewed , fill=Response)) +
  geom_boxplot() + labs(title = "Num Ads Viewed vs Response")
ggplot(data = marketing_data, aes(x =  Response, y = NumArticlesFashion , fill=Response)) +
  geom_boxplot() + labs(title = "Num Articles Fashion vs Response")
ggplot(data = marketing_data, aes(x =  Response, y = NumArticlesSports , fill=Response)) +
  geom_boxplot() + labs(title = "Num Articles Sports vs Response")
ggplot(data = marketing_data, aes(x =  Response, y = NumArticlesTechnology , fill=Response)) +
  geom_boxplot() + labs(title = "Num Articles Technology vs Response")
ggplot(data = marketing_data, aes(x =  Response, y = NumArticlesWorldEvents , fill=Response)) +
  geom_boxplot() + labs(title = "Num Articles World Events vs Response")
ggplot(data = marketing_data, aes(x =  Response, y = NumOfTimesAppOpened , fill=Response)) +
  geom_boxplot() + labs(title = "Num Of Times App Opened vs Response")
ggplot(data = marketing_data, aes(x =  Response, y = TimeSpentOnStraitsTimes , fill=Response)) +
  geom_boxplot() + labs(title = "Time Spent On StraitsTimes vs Response")

# For Categorical Data
ggplot(data = marketing_data, aes( x = Gender,y=1.00, fill = Response)) +
  geom_col(position = "fill")+labs(title = "Gender vs Response",y="Proportion")
ggplot(data = marketing_data, aes( x = Education,y=1.00, fill = Response)) +
  geom_col(position = "fill")+labs(title = "Education vs Response",y="Proportion")
ggplot(data = marketing_data, aes( x = DeviceType ,y=1.00, fill = Response)) +
  geom_col(position = "fill")+labs(title = "Device Type vs Response",y="Proportion")
ggplot(data = marketing_data, aes( x = MaritalStatus ,y=1.00, fill = Response)) +
  geom_col(position = "fill")+labs(title = "Marital Status vs Response",y="Proportion")
ggplot(data = marketing_data, aes( x = ChildrenHome ,y=1.00, fill = Response)) +
  geom_col(position = "fill")+labs(title = "No. of Children Home vs Response",y="Proportion")
ggplot(data =marketing_data, aes( x = SubscriptionPaymentMethod ,y=1.00, fill = Response)) +
  geom_col(position = "fill")+labs(title = "Subscription Payment Method Type vs Response",y="Proportion")
ggplot(data = marketing_data, aes(x =  Response,y = NumArticlesHealth , fill=Response)) +
  geom_boxplot() + labs(title = "Num Articles Health vs Response")

# =======================================================================================================
# =================================== ASSOCIATION RULES =================================================

widedata2 <- read.csv("marketing_data_cleaned.csv")
quantile(widedata2$Income)
quantile(widedata2$Age)
quantile(widedata2$NumAdsViewed)
quantile(widedata2$NumOfTimesAppOpened)
quantile(widedata2$NumArticlesSports)
quantile(widedata2$NumArticlesHealth)
quantile(widedata2$NumArticlesTechnology)
quantile(widedata2$TimeSpentOnStraitsTimes)

ar_marketing_data <-widedata2%>% mutate(Incomelvl = case_when(
  Income <=  29385 ~ 1, 
  Income <= 47009 ~ 2,
  Income <=  64722 ~ 3,
  Income <=  162397 ~ 4),
  Fashlvl = case_when(
    NumArticlesFashion <= 1 ~1,
    NumArticlesFashion <= 2 ~2,
    NumArticlesFashion <= 5~3,
    NumArticlesFashion <= 12~4
  ),Agelvl = case_when(
    Age <= 36~1,
    Age <= 49~2,
    Age <= 60~3,
    Age <= 82~4
  ), Adslvl = case_when(
    NumAdsViewed <= 2~1,
    NumAdsViewed <= 4~2,
    NumAdsViewed <= 7~3,
    NumAdsViewed <= 27~4
  ),OpenApplvl = case_when(
    NumOfTimesAppOpened <= 2~1,
    NumOfTimesAppOpened <= 3~2,
    NumOfTimesAppOpened <= 4~3,
    NumOfTimesAppOpened <= 7~4
  ),Sportslvl = case_when(
    NumArticlesSports <= 1~1,
    NumArticlesSports <= 2~2,
    NumArticlesSports <= 4~3,
    NumArticlesSports <= 10~4
  ),Healthlvl = case_when(
    NumArticlesHealth <= 1~1,
    NumArticlesHealth <= 2~2,
    NumArticlesHealth <= 4~3,
    NumArticlesHealth <= 14~4
  ),Techlvl = case_when(
    NumArticlesTechnology <= 1~1,
    NumArticlesTechnology <= 2~2,
    NumArticlesTechnology <= 4~3,
    NumArticlesTechnology <= 11~4
  ),Timespentlvl = case_when(
    TimeSpentOnStraitsTimes <= 11~1,
    TimeSpentOnStraitsTimes <= 18~2,
    TimeSpentOnStraitsTimes <= 26~3,
    TimeSpentOnStraitsTimes <=50~4,
  ))

ar_marketing_data$Gender <- factor(ar_marketing_data$Gender)
ar_marketing_data$ChildrenHome <- factor(ar_marketing_data$ChildrenHome)
ar_marketing_data$Education <- factor(ar_marketing_data$Education)
ar_marketing_data$MaritalStatus <- factor(ar_marketing_data$MaritalStatus)
ar_marketing_data$DeviceType <- factor(ar_marketing_data$DeviceType)
ar_marketing_data$SubscriptionPaymentMethod <- factor(ar_marketing_data$SubscriptionPaymentMethod)
ar_marketing_data$Sportslvl <- factor(ar_marketing_data$Sportslvl)
ar_marketing_data$Adslvl <- factor(ar_marketing_data$Adslvl)
ar_marketing_data$Healthlvl <- factor(ar_marketing_data$Healthlvl)
ar_marketing_data$Techlvl <- factor(ar_marketing_data$Techlvl)
ar_marketing_data$Fashlvl <- factor(ar_marketing_data$Fashlvl)
ar_marketing_data$Agelvl<- factor(ar_marketing_data$Agelvl)
ar_marketing_data$Timespentlvl <- factor(ar_marketing_data$Timespentlvl)
ar_marketing_data$OpenApplvl <- factor(ar_marketing_data$OpenApplvl)
ar_marketing_data$Incomelvl <- factor(ar_marketing_data$Incomelvl)

summary(ar_marketing_data)

ar_marketing_data1 = subset(ar_marketing_data, select = -c(Age,Income,NumAdsViewed,NumArticlesFashion,NumArticlesHealth,NumArticlesTechnology,NumArticlesSports, NumArticlesWorldEvents,NumOfTimesAppOpened,TimeSpentOnStraitsTimes) )
View(ar_marketing_data1)

ar_marketing_data1[] <- lapply(ar_marketing_data1, factor) # all columns to factor datatype instead of integer & save as a dataframe.
summary(ar_marketing_data1)  # Check factor in each column.
ar_marketing_data1 <- ar_marketing_data1[, -1] # Remove ID column as each row is considered one transaction.

trans1 <- as(ar_marketing_data1, "transactions")  # Convert to transactions datatype
inspect(trans1)
rules1 <- apriori(data = trans1, parameter = 
                    list(minlen = 2,supp=0.01, conf = 0.1, target = "rules"), appearance = list (rhs="Response=1"))
sum(is.redundant(rules1))
rules.clean <- rules1[!is.redundant(rules1)]
rules.clean.df <- as(rules.clean, "data.frame")

summary(rules1)  
rule.table1 <- inspect(rules1) 
rule.table1
rules.df <- as(rules.clean, "data.frame")
View(rules.df)


# ============================== CREATING TABLE OF ERRORS ==============================================
## UNBALANCED DATASET 

set.seed(2)
train <- sample.split(Y = marketing_data$Response, SplitRatio = 0.70)
trainset <- subset(marketing_data, train == T)
testset <- subset(marketing_data, train == F)

Model = c("Logistic Reg", "CART", "Random Forest","Neural Network", "XGBoost")
FPR = rep(0,5)
FNR = rep(0,5)
Err = rep(0,5)

table_testset_unbal = data.frame(Model,FPR,FNR,Err)
table_testset_unbal

## BALANCED DATASET 
majority <- trainset[trainset$Response == 0, ] 
minority <- trainset[trainset$Response == 1, ]

set.seed(2)
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority)) 
majority.chosen <- majority[chosen,] 
trainset.bal <- rbind(majority.chosen, minority) 
summary(trainset.bal)
table_testset_bal = data.frame(Model,FPR,FNR,Err)
table_testset_bal

# =======================================================================================================
# ====================================== LOGISTIC REGRESSION ============================================


## UNBALANCED TRAINSET 
train_log = data.frame(trainset)
m1<-glm(Response~.,family=binomial,data=train_log)
summary(m1)

# Backwards Elimination 
backwards<-step(m1)
formula(backwards)

m2<-glm(Response~Age + Education + MaritalStatus + Income + DeviceType + 
          TimeSpentOnStraitsTimes + NumOfTimesAppOpened + NumAdsViewed + 
          NumArticlesFashion + NumArticlesTechnology + NumArticlesHealth + 
          NumArticlesWorldEvents,family=binomial,data=train_log)
summary(m2)

m3<-glm(Response~Age + Income + DeviceType + 
          TimeSpentOnStraitsTimes + NumOfTimesAppOpened + NumAdsViewed + 
          NumArticlesFashion + NumArticlesTechnology + NumArticlesHealth + 
          NumArticlesWorldEvents,family=binomial,data=train_log)
summary(m3)

# Confusion matrix
prediction <-predict(m3, newdata = testset ,type = "response")
threshold <- 0.5
m.LR.3.predict <- ifelse(prediction > threshold, "1", "0")

log_test_conf_unbal = table(Actual = testset$Response, Predicted = m.LR.3.predict)
log_test_conf_unbal

LR_test_FPR <- log_test_conf_unbal[3] / (log_test_conf_unbal[3] + log_test_conf_unbal[1])
LR_test_FNR <- log_test_conf_unbal[2] / (log_test_conf_unbal[2] + log_test_conf_unbal[4])
LR_test_error <-(log_test_conf_unbal[2] + log_test_conf_unbal[3])/(log_test_conf_unbal[1] + log_test_conf_unbal[2] + log_test_conf_unbal[3] + log_test_conf_unbal[4])

table_testset_unbal[1, "FPR"] = LR_test_FPR
table_testset_unbal[1, "FNR"] = LR_test_FNR
table_testset_unbal[1, "Err"] = LR_test_error
table_testset_unbal

# BALANCED TRAINSET 
train_log_bal = data.frame(trainset.bal)
summary(trainset.bal)

set.seed(2)
model1<-glm(Response~.,family=binomial, data=train_log_bal)
summary(model1)

# Backwards Elimination 
backwards<-step(model1)
formula(backwards)

model2<-glm(Response~Age + Income + DeviceType + TimeSpentOnStraitsTimes + 
              NumOfTimesAppOpened + NumAdsViewed + NumArticlesFashion + 
              NumArticlesTechnology + NumArticlesHealth,family=binomial,data=train_log_bal)
summary(model2)

prediction.bal<-predict(model2, newdata = testset ,type = "response")
threshold <- 0.5
m.LR.2.predict <- ifelse(prediction.bal > threshold, "1", "0")

# Confusion Matrix 
log_test_conf_bal = table(Actual = testset$Response, Predicted = m.LR.2.predict)
log_test_conf_bal

LR_test_FPR <- log_test_conf_bal[3] / (log_test_conf_bal[3] + log_test_conf_bal[1])
LR_test_FNR <- log_test_conf_bal[2] / (log_test_conf_bal[2] + log_test_conf_bal[4])
LR_test_error <-(log_test_conf_bal[2]+ log_test_conf_bal[3])/(log_test_conf_bal[1] + log_test_conf_bal[2] + log_test_conf_bal[3] + log_test_conf_bal[4])

table_testset_bal[1, "FPR"] = LR_test_FPR
table_testset_bal[1, "FNR"] = LR_test_FNR
table_testset_bal[1, "Err"] = LR_test_error
table_testset_bal


# =======================================================================================================
# ============================================== CART ===================================================

# UNBALANCED TRAINSET 
train_cart = data.frame(trainset)

set.seed(2)
c1<-rpart(Response~.,data=train_cart, method='class', control=rpart.control(minsplit=2,cp=0))
rpart.plot(c1,nn=T, main="maximal tree")
print(c1)
printcp(c1)
plotcp(c1,main="subtree")

# Finding CP Optimal 
CVerror.cap <- c1$cptable[which.min(c1$cptable[,"xerror"]), "xerror"] + c1$cptable[which.min(c1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (c1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(c1$cptable[i,1] * c1$cptable[i-1,1]), 1)
cp.opt

c2<-prune(c1,cp=cp.opt)
printcp(c2)
rpart.plot(c2,nn=T,main="pruned tree")

# Confusion Matrix  
cart.predict<-predict(c2, newdata=testset, type="class") 
cart_test_conf_unbal = table(Actual = testset$Response, Predicted = cart.predict)
cart_test_conf_unbal

cart_test_FPR <- cart_test_conf_unbal[3] / (cart_test_conf_unbal[3] + cart_test_conf_unbal[1])
cart_test_FNR <- cart_test_conf_unbal[2] / (cart_test_conf_unbal[2] + cart_test_conf_unbal[4])
cart_test_error <-(cart_test_conf_unbal[2]+ cart_test_conf_unbal[3])/(cart_test_conf_unbal[1] + cart_test_conf_unbal[2] + cart_test_conf_unbal[3] + cart_test_conf_unbal[4])

table_testset_unbal[2, "FPR"] = cart_test_FPR
table_testset_unbal[2, "FNR"] = cart_test_FNR
table_testset_unbal[2, "Err"] = cart_test_error
table_testset_unbal


# BALANCED TRAINSET 
train_cart_bal = data.frame(trainset.bal)

set.seed(2)
c3 <- rpart(Response~.,data=train_cart_bal, method='class',control=rpart.control(minsplit=2,cp=0))
rpart.plot(c3,nn=T, main="maximal tree")
print(c3)
printcp(c3)
plotcp(c3,main="subtree")

# Finding CP Optimal 
CVerror.cap <- c3$cptable[which.min(c3$cptable[,"xerror"]), "xerror"] + c3$cptable[which.min(c3$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (c3$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(c3$cptable[i,1] * c3$cptable[i-1,1]), 1)
cp.opt

c4 <- prune(c3,cp=cp.opt)
printcp(c4)
rpart.plot(c4,nn=T,main="pruned tree")

# Confusion Matrix  
cart.predict <- predict(c4, newdata=testset, type="class") 
cart_test_conf_bal = table(Actual = testset$Response, Predicted = cart.predict)
cart_test_conf_bal

cart_test_FPR <- cart_test_conf_bal[3] / (cart_test_conf_bal[3] + cart_test_conf_bal[1])
cart_test_FNR <- cart_test_conf_bal[2] / (cart_test_conf_bal[2] + cart_test_conf_bal[4])
cart_test_error <-(cart_test_conf_bal[2]+ cart_test_conf_bal[3])/(cart_test_conf_bal[1] + cart_test_conf_bal[2] + cart_test_conf_bal[3] + cart_test_conf_bal[4])

table_testset_bal[2, "FPR"] = cart_test_FPR
table_testset_bal[2, "FNR"] = cart_test_FNR
table_testset_bal[2, "Err"] = cart_test_error
table_testset_bal


# =======================================================================================================
# ========================================= RANDOM FOREST ===============================================

# UNBALANCED TRAINSET 
train_rf = data.frame(trainset)

set.seed(2)
RF.m1<-randomForest(Response~.,data=train_rf,importance=T)
RF.m1

var.impt<-importance(RF.m1)
varImpPlot(RF.m1,type=1)

# Confusion matrix
RF.predict <- predict(RF.m1, newdata = testset, type = 'class')
RF_test_conf_unbal <- table(Actual = testset$Response, Predicted = RF.predict, deparse.level = 2)
RF_test_conf_unbal

RF_test_FPR <- RF_test_conf_unbal[3] / (RF_test_conf_unbal[3] + RF_test_conf_unbal[1])
RF_test_FNR <- RF_test_conf_unbal[2] / (RF_test_conf_unbal[2] + RF_test_conf_unbal[4])
RF_test_error <-(RF_test_conf_unbal[2]+ RF_test_conf_unbal[3])/(RF_test_conf_unbal[1] + RF_test_conf_unbal[2] + RF_test_conf_unbal[3] + RF_test_conf_unbal[4])

table_testset_unbal[3, "FPR"] = RF_test_FPR
table_testset_unbal[3, "FNR"] = RF_test_FNR
table_testset_unbal[3, "Err"] = RF_test_error
table_testset_unbal

# BALANCED TRAINSET 
train_rf_bal = data.frame(trainset.bal)

set.seed(2)
RF.m2 <- randomForest(Response~., data=train_rf_bal,importance=T)
RF.m2
var.impt<-importance(RF.m2)
varImpPlot(RF.m2,type=1)

# Confusion matrix
RF.predict <- predict(RF.m2, newdata = testset, type = 'class')
RF_test_conf_bal <- table(Actual = testset$Response, Predicted = RF.predict, deparse.level = 2)
RF_test_conf_bal

RF_test_FPR <- RF_test_conf_bal[3] / (RF_test_conf_bal[3] + RF_test_conf_bal[1])
RF_test_FNR <- RF_test_conf_bal[2] / (RF_test_conf_bal[2] + RF_test_conf_bal[4])
RF_test_error <-(RF_test_conf_bal[2]+ RF_test_conf_bal[3])/(RF_test_conf_bal[1] + RF_test_conf_bal[2] + RF_test_conf_bal[3] + RF_test_conf_bal[4])


table_testset_bal[3, "FPR"] = RF_test_FPR
table_testset_bal[3, "FNR"] = RF_test_FNR
table_testset_bal[3, "Err"] = RF_test_error
table_testset_bal


# =======================================================================================================
# ======================================== NEURAL NETWORK ===============================================

train_nn = data.frame(trainset)
test_nn = data.frame(testset)
str(train_nn)

## NEURAL NETWORK WITHOUT NORMALIZATION (UNBALANCED DATASET) 

# Creating tables for different iteration of NN using unbalanced trainset
Model = c("NN without Normalization", "Norm 1 Layer 10 Nodes", 
          "Norm 1 Layer 8 Nodes", "Norm 1 Layer 4 Nodes", "Norm 1 Layer 3 Nodes",
          "Norm 1 Layer 2 Nodes")
FPR = rep(0,6)
FNR = rep(0,6)
Err = rep(0,6)
nn_table1_testset_unbal = data.frame(Model,FPR,FNR,Err)
nn_table1_testset_unbal

# Creating dummy variables for categorical variables  
marketing_matrix_train <- model.matrix(~Response + Age + Gender + ChildrenHome + Education
                                       + MaritalStatus + Income + DeviceType + SubscriptionPaymentMethod
                                       + TimeSpentOnStraitsTimes + NumOfTimesAppOpened + NumAdsViewed 
                                       + NumArticlesFashion + NumArticlesTechnology + NumArticlesHealth 
                                       + NumArticlesSports + NumArticlesWorldEvents, data=train_nn)
colnames(marketing_matrix_train)[2] <- "Response"
colnames(marketing_matrix_train)[4] <- "Gender"

marketing_matrix_test <- model.matrix(~Response + Age + Gender + ChildrenHome + Education
                                       + MaritalStatus + Income + DeviceType + SubscriptionPaymentMethod
                                       + TimeSpentOnStraitsTimes + NumOfTimesAppOpened + NumAdsViewed 
                                       + NumArticlesFashion + NumArticlesTechnology + NumArticlesHealth 
                                      + NumArticlesSports + NumArticlesWorldEvents, data=test_nn)
colnames(marketing_matrix_test)[2] <- "Response"
colnames(marketing_matrix_test)[4] <- "Gender"

# Getting formula list for neural network
col_list <- paste(c(colnames(marketing_matrix_train[,-c(1,2)])),collapse="+")
col_list <- paste(c("Response~",col_list),collapse="")
f = formula(col_list)
f

set.seed(2)
m1_nn <- neuralnet(f, 
                   data = marketing_matrix_train, 
                   hidden=2,
                   err.fct="ce", 
                   linear.output=FALSE)
par(mfrow=c(1,1))
plot(m1_nn)

# Confusion Matrix 
output <- neuralnet::compute(m1_nn, marketing_matrix_test[,-c(1,2)]) ## Predicting on testset 
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
nn_test_conf = table(Actual = factor(testset$Response, levels=c(0,1)), Predicted = factor(pred1, levels=c(0,1)))
nn_test_conf

nn_test_FPR <- nn_test_conf[3] / (nn_test_conf[3] + nn_test_conf[1])
nn_test_FNR <- nn_test_conf[2] / (nn_test_conf[2] + nn_test_conf[4])
nn_test_error <- (nn_test_conf[2] + nn_test_conf[3])/(nn_test_conf[1] + nn_test_conf[2] + nn_test_conf[3] + nn_test_conf[4])

nn_table1_testset_unbal[1, "FPR"] = nn_test_FPR
nn_table1_testset_unbal[1, "FNR"] = nn_test_FNR
nn_table1_testset_unbal[1, "Err"] = nn_test_error
nn_table1_testset_unbal


# Neural Net GW Results
par(mfrow=c(2,3))
gwplot(m1_nn,selected.covariate="Age", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="Income", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="DeviceTypeLaptop", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="DeviceTypeTablet", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="DeviceTypePhone", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="TimeSpentOnStraitsTimes", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="NumArticlesHealth", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="ChildrenHome", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="NumOfTimesAppOpened", min=-2.5, max=5)
gwplot(m1_nn,selected.covariate="NumAdsViewed", min=-2.5, max=5)
par(mfrow=c(1,1))

## NEURAL NETWORK WITH NORMALIZATION (UNBALANCED DATASET) 

# MAX_MIN NORMALIZATION
train_nn$Age = (train_nn$Age - min(train_nn$Age))/(max(train_nn$Age) - min(train_nn$Age))
train_nn$ChildrenHome = (train_nn$ChildrenHome - min(train_nn$ChildrenHome))/(max(train_nn$ChildrenHome) - min(train_nn$ChildrenHome))
train_nn$Income = (train_nn$Income - min(train_nn$Income))/(max(train_nn$Income) - min(train_nn$Income))
train_nn$TimeSpentOnStraitsTimes = (train_nn$TimeSpentOnStraitsTimes - min(train_nn$TimeSpentOnStraitsTimes))/(max(train_nn$TimeSpentOnStraitsTimes) - min(train_nn$TimeSpentOnStraitsTimes))
train_nn$NumOfTimesAppOpened = (train_nn$NumOfTimesAppOpened - min(train_nn$NumOfTimesAppOpened))/(max(train_nn$NumOfTimesAppOpened) - min(train_nn$NumOfTimesAppOpened))
train_nn$NumAdsViewed = (train_nn$NumAdsViewed - min(train_nn$NumAdsViewed))/(max(train_nn$NumAdsViewed) - min(train_nn$NumAdsViewed))
train_nn$NumArticlesFashion = (train_nn$NumArticlesFashion - min(train_nn$NumArticlesFashion))/(max(train_nn$NumArticlesFashion) - min(train_nn$NumArticlesFashion))
train_nn$NumArticlesTechnology = (train_nn$NumArticlesTechnology - min(train_nn$NumArticlesTechnology))/(max(train_nn$NumArticlesTechnology) - min(train_nn$NumArticlesTechnology))
train_nn$NumArticlesHealth = (train_nn$NumArticlesHealth - min(train_nn$NumArticlesHealth))/(max(train_nn$NumArticlesHealth) - min(train_nn$NumArticlesHealth))
train_nn$NumArticlesSports = (train_nn$NumArticlesSports - min(train_nn$NumArticlesSports))/(max(train_nn$NumArticlesSports) - min(train_nn$NumArticlesSports))
train_nn$NumArticlesWorldEvents = (train_nn$NumArticlesWorldEvents - min(train_nn$NumArticlesWorldEvents))/(max(train_nn$NumArticlesWorldEvents) - min(train_nn$NumArticlesWorldEvents))

# Creating dummy variables for categorical variables 
marketing_matrix_train <- model.matrix(~Response + Age + Gender + ChildrenHome + Education
                                 + MaritalStatus + Income + DeviceType + SubscriptionPaymentMethod
                                 + TimeSpentOnStraitsTimes + NumOfTimesAppOpened + NumAdsViewed + NumArticlesFashion
                                 + NumArticlesTechnology + NumArticlesHealth + NumArticlesSports + NumArticlesWorldEvents, data=train_nn)
colnames(marketing_matrix_train)[2] <- "Response"
colnames(marketing_matrix_train)[4] <- "Gender"
head(marketing_matrix_train)

# Getting formula list for neural network (With normalization)
col_list <- paste(c(colnames(marketing_matrix_train[,-c(1,2)])),collapse="+")
col_list <- paste(c("Response~",col_list),collapse="")
f = formula(col_list)
f

# Setting up testset (Unbalanced)
test_nn$Age = (test_nn$Age - min(test_nn$Age))/(max(test_nn$Age) - min(test_nn$Age))
test_nn$ChildrenHome = (test_nn$ChildrenHome - min(test_nn$ChildrenHome))/(max(test_nn$ChildrenHome) - min(test_nn$ChildrenHome))
test_nn$Income = (test_nn$Income - min(test_nn$Income))/(max(test_nn$Income) - min(test_nn$Income))
test_nn$TimeSpentOnStraitsTimes = (test_nn$TimeSpentOnStraitsTimes - min(test_nn$TimeSpentOnStraitsTimes))/(max(test_nn$TimeSpentOnStraitsTimes) - min(test_nn$TimeSpentOnStraitsTimes))
test_nn$NumOfTimesAppOpened = (test_nn$NumOfTimesAppOpened - min(test_nn$NumOfTimesAppOpened))/(max(test_nn$NumOfTimesAppOpened) - min(test_nn$NumOfTimesAppOpened))
test_nn$NumAdsViewed = (test_nn$NumAdsViewed - min(test_nn$NumAdsViewed))/(max(test_nn$NumAdsViewed) - min(test_nn$NumAdsViewed))
test_nn$NumArticlesFashion = (test_nn$NumArticlesFashion - min(test_nn$NumArticlesFashion))/(max(test_nn$NumArticlesFashion) - min(test_nn$NumArticlesFashion))
test_nn$NumArticlesTechnology = (test_nn$NumArticlesTechnology - min(test_nn$NumArticlesTechnology))/(max(test_nn$NumArticlesTechnology) - min(test_nn$NumArticlesTechnology))
test_nn$NumArticlesHealth = (test_nn$NumArticlesHealth - min(test_nn$NumArticlesHealth))/(max(test_nn$NumArticlesHealth) - min(test_nn$NumArticlesHealth))
test_nn$NumArticlesSports = (test_nn$NumArticlesSports - min(test_nn$NumArticlesSports))/(max(test_nn$NumArticlesSports) - min(test_nn$NumArticlesSports))
test_nn$NumArticlesWorldEvents = (test_nn$NumArticlesWorldEvents - min(test_nn$NumArticlesWorldEvents))/(max(test_nn$NumArticlesWorldEvents) - min(test_nn$NumArticlesWorldEvents))

marketing_matrix_test <- model.matrix(~Response + Age + Gender + ChildrenHome + Education
                                 + MaritalStatus + Income + DeviceType + SubscriptionPaymentMethod
                                 + TimeSpentOnStraitsTimes + NumOfTimesAppOpened + NumAdsViewed + NumArticlesFashion
                                 + NumArticlesTechnology + NumArticlesHealth + NumArticlesSports + NumArticlesWorldEvents, data=test_nn)
colnames(marketing_matrix_test)[2] <- "Response"
colnames(marketing_matrix_test)[4] <- "Gender"

# For loop to run NN Model trained on unbalanced trainset where network only has 1 layer 
x <- c(10,8,4,3,2)
index <- 2
for (val in x) {
  set.seed(22)
  m1_nn <- neuralnet(f, 
                     data = marketing_matrix_train, 
                     hidden=val,
                     err.fct="ce", 
                     linear.output=FALSE)
  
  output <- neuralnet::compute(m1_nn, marketing_matrix_test) ## Predicting on testset 
  p1 <- output$net.result
  pred1 <- ifelse(p1>0.5, 1, 0)
  nn_test_conf = table(Actual = testset$Response, Predicted = pred1)
  
  nn_test_FPR <- nn_test_conf[3] / (nn_test_conf[3] + nn_test_conf[1])
  nn_test_FNR <- nn_test_conf[2] / (nn_test_conf[2] + nn_test_conf[4])
  nn_test_error <- (nn_test_conf[2] + nn_test_conf[3])/(nn_test_conf[1] + nn_test_conf[2] + nn_test_conf[3] + nn_test_conf[4])
  
  nn_table1_testset_unbal[index, "FPR"] = nn_test_FPR
  nn_table1_testset_unbal[index, "FNR"] = nn_test_FNR
  nn_table1_testset_unbal[index, "Err"] = nn_test_error
  
  index <- index + 1
}
nn_table1_testset_unbal

table_testset_unbal[4, "FPR"] = nn_table1_testset_unbal[4,"FPR"]
table_testset_unbal[4, "FNR"] = nn_table1_testset_unbal[4,"FNR"]
table_testset_unbal[4, "Err"] = nn_table1_testset_unbal[4,"Err"]
table_testset_unbal

# =======================================================================================================


## NEURAL NETWORK WITH NORMALIZATION (BALANCED DATASET) 

train_nn_bal = data.frame(trainset.bal)
summary(train_nn_bal)

# Creating tables for different iteration of NN using balanced trainset
Model = c("NN Balanced", "NN Balanced + VarImpt from RF")
FPR = rep(0,2)
FNR = rep(0,2)
Err = rep(0,2)
nn_comparison_bal = data.frame(Model, FPR, FNR, Err)
nn_comparison_bal

# MAX_MIN NORMALIZATION
train_nn_bal$Age = (train_nn_bal$Age - min(train_nn_bal$Age))/(max(train_nn_bal$Age) - min(train_nn_bal$Age))
train_nn_bal$ChildrenHome = (train_nn_bal$ChildrenHome - min(train_nn_bal$ChildrenHome))/(max(train_nn_bal$ChildrenHome) - min(train_nn_bal$ChildrenHome))
train_nn_bal$Income = (train_nn_bal$Income - min(train_nn_bal$Income))/(max(train_nn_bal$Income) - min(train_nn_bal$Income))
train_nn_bal$TimeSpentOnStraitsTimes = (train_nn_bal$TimeSpentOnStraitsTimes - min(train_nn_bal$TimeSpentOnStraitsTimes))/(max(train_nn_bal$TimeSpentOnStraitsTimes) - min(train_nn_bal$TimeSpentOnStraitsTimes))
train_nn_bal$NumOfTimesAppOpened = (train_nn_bal$NumOfTimesAppOpened - min(train_nn_bal$NumOfTimesAppOpened))/(max(train_nn_bal$NumOfTimesAppOpened) - min(train_nn_bal$NumOfTimesAppOpened))
train_nn_bal$NumAdsViewed = (train_nn_bal$NumAdsViewed - min(train_nn_bal$NumAdsViewed))/(max(train_nn_bal$NumAdsViewed) - min(train_nn_bal$NumAdsViewed))
train_nn_bal$NumArticlesFashion = (train_nn_bal$NumArticlesFashion - min(train_nn_bal$NumArticlesFashion))/(max(train_nn_bal$NumArticlesFashion) - min(train_nn_bal$NumArticlesFashion))
train_nn_bal$NumArticlesTechnology = (train_nn_bal$NumArticlesTechnology - min(train_nn_bal$NumArticlesTechnology))/(max(train_nn_bal$NumArticlesTechnology) - min(train_nn_bal$NumArticlesTechnology))
train_nn_bal$NumArticlesHealth = (train_nn_bal$NumArticlesHealth - min(train_nn_bal$NumArticlesHealth))/(max(train_nn_bal$NumArticlesHealth) - min(train_nn_bal$NumArticlesHealth))
train_nn_bal$NumArticlesSports = (train_nn_bal$NumArticlesSports - min(train_nn_bal$NumArticlesSports))/(max(train_nn_bal$NumArticlesSports) - min(train_nn_bal$NumArticlesSports))
train_nn_bal$NumArticlesWorldEvents = (train_nn_bal$NumArticlesWorldEvents - min(train_nn_bal$NumArticlesWorldEvents))/(max(train_nn_bal$NumArticlesWorldEvents) - min(train_nn_bal$NumArticlesWorldEvents))

# Creating dummy variables for categorical variables  
marketing_matrix_train_bal <- model.matrix(~Response + Age + Gender + ChildrenHome + Education
                                       + MaritalStatus + Income + DeviceType + SubscriptionPaymentMethod
                                       + TimeSpentOnStraitsTimes + NumOfTimesAppOpened + NumAdsViewed + NumArticlesFashion
                                       + NumArticlesTechnology + NumArticlesHealth + NumArticlesSports + NumArticlesWorldEvents, data=train_nn_bal)

# Getting formula list for neural network
colnames(marketing_matrix_train_bal)[2] <- "Response"
colnames(marketing_matrix_train_bal)[4] <- "Gender"
col_list <- paste(c(colnames(marketing_matrix_train_bal[,-c(1,2)])),collapse="+")
col_list <- paste(c("Response~",col_list),collapse="")
f = formula(col_list)
f

# Shuffling the dataset
set.seed(2)
marketing_matrix_train_bal <- marketing_matrix_train_bal[sample(1:nrow(marketing_matrix_train_bal)), ]

# Running the model 
set.seed(2)
m1_nn_bal <- neuralnet(f, 
                   data = marketing_matrix_train_bal, 
                   hidden=3,
                   err.fct="ce", 
                   linear.output=FALSE)

# Confusion Matrix 
output <- neuralnet::compute(m1_nn_bal, marketing_matrix_test) ## Predicting on testset 
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
nn_test_conf_bal = table(Actual = factor(testset$Response, levels=c(0,1)), Predicted = factor(pred1, levels=c(0,1)))
nn_test_conf_bal

nn_test_FPR <- nn_test_conf_bal[3] / (nn_test_conf_bal[3] + nn_test_conf_bal[1])
nn_test_FNR <- nn_test_conf_bal[2] / (nn_test_conf_bal[2] + nn_test_conf_bal[4])
nn_test_error <- (nn_test_conf_bal[2] + nn_test_conf_bal[3])/(nn_test_conf_bal[1] + nn_test_conf_bal[2] + nn_test_conf_bal[3] + nn_test_conf_bal[4])

nn_comparison_bal[1, "FPR"] = nn_test_FPR
nn_comparison_bal[1, "FNR"] = nn_test_FNR
nn_comparison_bal[1, "Err"] = nn_test_error
nn_comparison_bal


## NEURAL NETWORK WITH NORMALIZATION (BALANCED DATASET) + VarImpt from RF

train_nn_vi = data.frame(trainset.bal)
summary(train_nn_vi)

# MAX_MIN NORMALIZATION
train_nn_vi$Age = (train_nn_vi$Age - min(train_nn_vi$Age))/(max(train_nn_vi$Age) - min(train_nn_vi$Age))
train_nn_vi$ChildrenHome = (train_nn_vi$ChildrenHome - min(train_nn_vi$ChildrenHome))/(max(train_nn_vi$ChildrenHome) - min(train_nn_vi$ChildrenHome))
train_nn_vi$Income = (train_nn_vi$Income - min(train_nn_vi$Income))/(max(train_nn_vi$Income) - min(train_nn_vi$Income))
train_nn_vi$TimeSpentOnStraitsTimes = (train_nn_vi$TimeSpentOnStraitsTimes - min(train_nn_vi$TimeSpentOnStraitsTimes))/(max(train_nn_vi$TimeSpentOnStraitsTimes) - min(train_nn_vi$TimeSpentOnStraitsTimes))
train_nn_vi$NumOfTimesAppOpened = (train_nn_vi$NumOfTimesAppOpened - min(train_nn_vi$NumOfTimesAppOpened))/(max(train_nn_vi$NumOfTimesAppOpened) - min(train_nn_vi$NumOfTimesAppOpened))
train_nn_vi$NumAdsViewed = (train_nn_vi$NumAdsViewed - min(train_nn_vi$NumAdsViewed))/(max(train_nn_vi$NumAdsViewed) - min(train_nn_vi$NumAdsViewed))
train_nn_vi$NumArticlesHealth = (train_nn_vi$NumArticlesHealth - min(train_nn_vi$NumArticlesHealth))/(max(train_nn_vi$NumArticlesHealth) - min(train_nn_vi$NumArticlesHealth))

# Creating dummy variables for categorical variables  
marketing_matrix_train_vi_bal <- model.matrix(~ Response + Age + ChildrenHome + Income + 
                                                DeviceType + TimeSpentOnStraitsTimes + 
                                                NumOfTimesAppOpened + NumAdsViewed + NumArticlesHealth, data=train_nn_vi)
colnames(marketing_matrix_train_vi_bal)[2] <- "Response"
summary(marketing_matrix_train_vi_bal)

# Getting formula list for neural network
col_list <- paste(c(colnames(marketing_matrix_train_vi_bal[,-c(1,2)])),collapse="+")
col_list <- paste(c("Response~",col_list),collapse="")
f = formula(col_list)
f

# Setting up testset (Balanced + VarImpt from RF)
test_nn_vi = data.frame(testset)

test_nn_vi$Age = (test_nn_vi$Age - min(test_nn_vi$Age))/(max(test_nn_vi$Age) - min(test_nn_vi$Age))
test_nn_vi$ChildrenHome = (test_nn_vi$ChildrenHome - min(test_nn_vi$ChildrenHome))/(max(test_nn_vi$ChildrenHome) - min(test_nn_vi$ChildrenHome))
test_nn_vi$Income = (test_nn_vi$Income - min(test_nn_vi$Income))/(max(test_nn_vi$Income) - min(test_nn_vi$Income))
test_nn_vi$TimeSpentOnStraitsTimes = (test_nn_vi$TimeSpentOnStraitsTimes - min(test_nn_vi$TimeSpentOnStraitsTimes))/(max(test_nn_vi$TimeSpentOnStraitsTimes) - min(test_nn_vi$TimeSpentOnStraitsTimes))
test_nn_vi$NumOfTimesAppOpened = (test_nn_vi$NumOfTimesAppOpened - min(test_nn_vi$NumOfTimesAppOpened))/(max(test_nn_vi$NumOfTimesAppOpened) - min(test_nn_vi$NumOfTimesAppOpened))
test_nn_vi$NumAdsViewed = (test_nn_vi$NumAdsViewed - min(test_nn_vi$NumAdsViewed))/(max(test_nn_vi$NumAdsViewed) - min(test_nn_vi$NumAdsViewed))
test_nn_vi$NumArticlesHealth = (test_nn_vi$NumArticlesHealth - min(test_nn_vi$NumArticlesHealth))/(max(test_nn_vi$NumArticlesHealth) - min(test_nn_vi$NumArticlesHealth))

marketing_matrix_test_vi_bal <- model.matrix(~ Response + Age + ChildrenHome + Income + 
                                                DeviceType + TimeSpentOnStraitsTimes + 
                                                NumOfTimesAppOpened + NumAdsViewed + NumArticlesHealth, data=test_nn_vi)
colnames(marketing_matrix_test_vi_bal)[2] <- "Response"
summary(marketing_matrix_train_vi_bal)

# Running the model 
set.seed(2)
m1_nn_vi_bal <- neuralnet(f, 
                       data = marketing_matrix_train_vi_bal, 
                       hidden=3,
                       err.fct="ce", 
                       linear.output=FALSE)
plot(m1_nn_vi_bal)

# Confusion Matrix 
output <- neuralnet::compute(m1_nn_vi_bal, marketing_matrix_test_vi_bal) ## Predicting on testset 
p1 <- output$net.result
pred1 <- ifelse(p1>0.5, 1, 0)
nn_test_conf_vi_bal = table(Actual = factor(testset$Response, levels=c(0,1)), Predicted = factor(pred1, levels=c(0,1)))
nn_test_conf_vi_bal

nn_test_FPR <- nn_test_conf_vi_bal[3] / (nn_test_conf_vi_bal[3] + nn_test_conf_vi_bal[1])
nn_test_FNR <- nn_test_conf_vi_bal[2] / (nn_test_conf_vi_bal[2] + nn_test_conf_vi_bal[4])
nn_test_error <- (nn_test_conf_vi_bal[2] + nn_test_conf_vi_bal[3])/(nn_test_conf_vi_bal[1] + nn_test_conf_vi_bal[2] + nn_test_conf_vi_bal[3] + nn_test_conf_vi_bal[4])

nn_comparison_bal[2, "FPR"] = nn_test_FPR
nn_comparison_bal[2, "FNR"] = nn_test_FNR
nn_comparison_bal[2, "Err"] = nn_test_error
nn_comparison_bal 
#RUNNING WITH VARIABLE IMPORTANCE SEEMS TO BE BETTER 

# Creating tables for different iteration of NN using balanced trainset (with VarImpt)
Model = c("Norm 1 Layer 10 Nodes", 
          "Norm 1 Layer 8 Nodes", "Norm 1 Layer 4 Nodes", "Norm 1 Layer 3 Nodes",
          "Norm 1 Layer 2 Nodes")
FPR = rep(0,5)
FNR = rep(0,5)
Err = rep(0,5)
nn_table1_testset_bal = data.frame(Model,FPR,FNR,Err)
nn_table1_testset_bal

# For loop to run NN Model trained on unbalanced trainset where network only has 1 layer 
x <- c(10,8,4,3,2)
index <- 1
for (val in x) {
  set.seed(222)
  m1_nn_vi_bal <- neuralnet(f, 
                     data = marketing_matrix_train_vi_bal, 
                     hidden=val,
                     err.fct="ce", 
                     linear.output=FALSE)
  
  output <- neuralnet::compute(m1_nn_vi_bal, marketing_matrix_test_vi_bal) ## Predicting on testset 
  p1 <- output$net.result
  pred1 <- ifelse(p1>0.5, 1, 0)
  nn_test_conf = table(Actual = testset$Response, Predicted = pred1)
  
  nn_test_FPR <- nn_test_conf[3] / (nn_test_conf[3] + nn_test_conf[1])
  nn_test_FNR <- nn_test_conf[2] / (nn_test_conf[2] + nn_test_conf[4])
  nn_test_error <- (nn_test_conf[2] + nn_test_conf[3])/(nn_test_conf[1] + nn_test_conf[2] + nn_test_conf[3] + nn_test_conf[4])
  
  nn_table1_testset_bal[index, "FPR"] = nn_test_FPR
  nn_table1_testset_bal[index, "FNR"] = nn_test_FNR
  nn_table1_testset_bal[index, "Err"] = nn_test_error
  
  index <- index + 1
}
nn_table1_testset_bal

table_testset_bal[4, "FPR"] = nn_table1_testset_bal[5,"FPR"]
table_testset_bal[4, "FNR"] = nn_table1_testset_bal[5,"FNR"]
table_testset_bal[4, "Err"] = nn_table1_testset_bal[5,"Err"]
table_testset_bal

table_testset_unbal

# Neural Net GW Results
par(mfrow=c(2,3))
gwplot(m1_nn_vi_bal,selected.covariate="Age", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="Income", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="DeviceTypeLaptop", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="DeviceTypeTablet", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="DeviceTypePhone", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="TimeSpentOnStraitsTimes", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="NumArticlesHealth", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="ChildrenHome", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="NumOfTimesAppOpened", min=-2.5, max=5)
gwplot(m1_nn_vi_bal,selected.covariate="NumAdsViewed", min=-2.5, max=5)
par(mfrow=c(1,1))

table_testset_bal


# =======================================================================================================
# =============================================== XGBoost ===============================================

# UNBALANCED TRAINSET 
train_xg = data.frame(trainset)
test_xg = data.frame(testset)

train_xg$Response <- as.numeric(as.character(train_xg$Response))
test_xg$Response <- as.numeric(as.character(test_xg$Response))

# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(Response ~ .-1, data = train_xg)
train_label <- train_xg[,"Response"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(Response~.-1, data = test_xg)
test_label <- test_xg[,"Response"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

# Parameters
nc <- length(unique(train_label))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)

# eXtreme Gradient Boosting Model
set.seed(2)
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 124,
                       watchlist = watchlist,
                       eta = 0.05)

# Training & test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')

min(e$test_mlogloss) #0.263894
e[e$test_mlogloss == 0.263894,] #124 iteration

# Feature importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
xgb.plot.importance(imp)

# Prediction & confusion matrix - test data
p <- predict(bst_model, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)
xg_test_conf <- table(Actual = pred$label, Predicted = pred$max_prob)
xg_test_conf

xg_test_FPR <- xg_test_conf[3] / (xg_test_conf[3] + xg_test_conf[1])
xg_test_FNR <- xg_test_conf[2] / (xg_test_conf[2] + xg_test_conf[4])
xg_test_error <- (xg_test_conf[2] + xg_test_conf[3])/(xg_test_conf[1] + xg_test_conf[2] + xg_test_conf[3] + xg_test_conf[4])

table_testset_unbal[5, "FPR"] = xg_test_FPR
table_testset_unbal[5, "FNR"] = xg_test_FNR
table_testset_unbal[5, "Err"] = xg_test_error
table_testset_unbal

# BALANCED TRAINSET 
train_xg_bal = data.frame(trainset.bal)

train_xg_bal$Response <- as.numeric(as.character(train_xg_bal$Response))

# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(Response ~ .-1, data = train_xg_bal)
train_label <- train_xg_bal[,"Response"]
train_matrix_bal <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(Response~.-1, data = test_xg)
test_label <- test_xg[,"Response"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

# Parameters
nc <- length(unique(train_label))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix_bal, test = test_matrix)

# eXtreme Gradient Boosting Model
set.seed(2)
bst_model_bal <- xgb.train(params = xgb_params,
                       data = train_matrix_bal,
                       nrounds = 96,
                       watchlist = watchlist,
                       eta = 0.05)

# Training & test error plot
e <- data.frame(bst_model_bal$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')

min(e$test_mlogloss) #0.344978
e[e$test_mlogloss == 0.344978,] #96 iteration

# Feature importance
imp <- xgb.importance(colnames(train_matrix_bal), model = bst_model_bal)
xgb.plot.importance(imp)

# Prediction & confusion matrix - test data
p <- predict(bst_model_bal, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)
xg_test_conf_bal <- table(Actual = pred$label, Predicted = pred$max_prob)
xg_test_conf_bal

xg_test_FPR <- xg_test_conf_bal[3] / (xg_test_conf_bal[3] + xg_test_conf_bal[1])
xg_test_FNR <- xg_test_conf_bal[2] / (xg_test_conf_bal[2] + xg_test_conf_bal[4])
xg_test_error <- (xg_test_conf_bal[2] + xg_test_conf_bal[3])/(xg_test_conf_bal[1] + xg_test_conf_bal[2] + xg_test_conf_bal[3] + xg_test_conf_bal[4])

table_testset_bal[5, "FPR"] = xg_test_FPR
table_testset_bal[5, "FNR"] = xg_test_FNR
table_testset_bal[5, "Err"] = xg_test_error


# =======================================================================================================
# ==================================== Overall Comparison ===============================================
table_testset_unbal
table_testset_bal
