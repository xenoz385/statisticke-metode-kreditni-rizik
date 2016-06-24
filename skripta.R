## Paketi

install.packages('class', dependencies = TRUE)
library(class)
install.packages('caret', dependencies = TRUE)
library(caret)
install.packages('pROC', dependencies = TRUE)
library(pROC)
install.packages('e1071', dependencies = TRUE)
library(e1071) 
install.packages('ggplot2', dependencies = TRUE)
library(ggplot2)
install.packages('ROCR', dependencies = TRUE)
library(ROCR)
install.packages('randomForest', dependencies = TRUE)
library(randomForest)
install.packages('gmodels', dependencies = TRUE)
library(gmodels)
install.packages('nnet', dependencies = TRUE)
library(nnet)


#uèitavanje podataka

raw_data=read.csv("credit_card.csv", sep = ";")
head(raw_data)
class(raw_data) #data frame
raw_data=raw_data[,-1] # mièem ID
dim(raw_data)
summary(raw_data)
str(raw_data)

colnames(raw_data) <- c("LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE", "PAY1", "PAY2", "PAY3", "PAY4", "PAY5", "PAY6", "BILL_AMT1", "BILL_AMT2", 
"BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", "PAY_AMT1", "PAY_AMT2", "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6", "DEFAULT")

#prilagodba tipova podataka
raw_data=transform(raw_data, LIMIT_BAL = as.numeric(LIMIT_BAL), SEX = as.factor(SEX), EDUCATION =  as.factor(EDUCATION), MARRIAGE = as.factor(MARRIAGE), 
PAY1 = as.factor(PAY1), PAY2 = as.factor(PAY2), PAY3 = as.factor(PAY3), PAY4 = as.factor(PAY4), PAY5 = as.factor(PAY5), PAY6 = as.factor(PAY6), 
BILL_AMT1 = as.numeric(BILL_AMT1), BILL_AMT2 = as.numeric(BILL_AMT2), BILL_AMT3 = as.numeric(BILL_AMT3), BILL_AMT4 = as.numeric(BILL_AMT4), 
BILL_AMT5 = as.numeric(BILL_AMT5), BILL_AMT6 = as.numeric(BILL_AMT6), PAY_AMT1 = as.numeric(PAY_AMT1), PAY_AMT2 = as.numeric(PAY_AMT2), 
PAY_AMT3 = as.numeric(PAY_AMT3), PAY_AMT4 = as.numeric(PAY_AMT4), PAY_AMT5 = as.numeric(PAY_AMT5), PAY_AMT6 = as.numeric(PAY_AMT6),
DEFAULT = factor(DEFAULT, levels = c("N", "Y"))) 

str(raw_data)
dim(raw_data)

table(raw_data$DEFAULT)

# Izbacivanje nemoguæih vrijednosti
index_edu0 = which(raw_data$EDUCATION == 0)
new_data = raw_data[-index_edu0,]
index_edu5 = which(new_data$EDUCATION == 5)
new_data = new_data[-index_edu5,]
index_edu6 = which(new_data$EDUCATION == 6)
new_data = new_data[-index_edu6,]
index_mar0 = which(new_data$MARRIAGE == 0)
new_data = new_data[-index_mar0,]
index_pay1 = which(new_data$PAY1 == -2)
new_data = new_data[-index_pay1,]
index_pay2 = which(new_data$PAY2 == -2)
new_data = new_data[-index_pay2,]
index_pay3 = which(new_data$PAY3 == -2)
new_data = new_data[-index_pay3,]
index_pay4 = which(new_data$PAY4 == -2)
new_data = new_data[-index_pay4,]
index_pay5 = which(new_data$PAY5 == -2)
new_data = new_data[-index_pay5,]
index_pay6 = which(new_data$PAY6 == -2)
new_data = new_data[-index_pay6,]

dim(new_data)
summary(new_data)




###otkrivanje outliera


#png(file = "boxplot_LimitBal.png", width = 1600, height = 1600, units = "px", res = 300)

boxplot(new_data$LIMIT_BAL, scipen=5, las = 2)

#dev.off()


## LIMIT_BAL

min_out_limit = min(boxplot.stats(new_data$LIMIT_BAL)$out)
index_limit = which(new_data$LIMIT_BAL >= min_out_limit)
new_data_out = new_data[-index_limit,]
dim(new_data_out)
summary(new_data_out)

## PAY_AMT and BILL_AMT

#payamount je uvijek pozitivan
min_out_pay_1 = min(boxplot.stats(new_data$PAY_AMT1)$out)
index_pay_1 = which(new_data$PAY_AMT1 >= min_out_pay_1)
min_out_pay_2 = min(boxplot.stats(new_data$PAY_AMT2)$out)
index_pay_2 = which(new_data$PAY_AMT2 >= min_out_pay_2)
min_out_pay_3 = min(boxplot.stats(new_data$PAY_AMT3)$out)
index_pay_3 = which(new_data$PAY_AMT3 >= min_out_pay_3)
min_out_pay_4 = min(boxplot.stats(new_data$PAY_AMT4)$out)
index_pay_4 = which(new_data$PAY_AMT4 >= min_out_pay_4)
min_out_pay_5 = min(boxplot.stats(new_data$PAY_AMT5)$out)
index_pay_5 = which(new_data$PAY_AMT5 >= min_out_pay_5)
min_out_pay_6 = min(boxplot.stats(new_data$PAY_AMT6)$out)
index_pay_6 = which(new_data$PAY_AMT6 >= min_out_pay_6)

#pozitivni bill amount
min_out_bill_1p = min(boxplot.stats(new_data$BILL_AMT1)$out[which(boxplot.stats(new_data$BILL_AMT1)$out >= 0)])
index_bill_1p = which(new_data$BILL_AMT1 >= min_out_bill_1p)
min_out_bill_2p = min(boxplot.stats(new_data$BILL_AMT2)$out[which(boxplot.stats(new_data$BILL_AMT2)$out >= 0)])
index_bill_2p = which(new_data$BILL_AMT2 >= min_out_bill_2p)
min_out_bill_3p = min(boxplot.stats(new_data$BILL_AMT3)$out[which(boxplot.stats(new_data$BILL_AMT3)$out >= 0)])
index_bill_3p = which(new_data$BILL_AMT3 >= min_out_bill_3p)
min_out_bill_4p = min(boxplot.stats(new_data$BILL_AMT4)$out[which(boxplot.stats(new_data$BILL_AMT4)$out >= 0)])
index_bill_4p = which(new_data$BILL_AMT4 >= min_out_bill_4p)
min_out_bill_5p = min(boxplot.stats(new_data$BILL_AMT5)$out[which(boxplot.stats(new_data$BILL_AMT5)$out >= 0)])
index_bill_5p = which(new_data$BILL_AMT5 >= min_out_bill_5p)
min_out_bill_6p = min(boxplot.stats(new_data$BILL_AMT6)$out[which(boxplot.stats(new_data$BILL_AMT6)$out >= 0)])
index_bill_6p = which(new_data$BILL_AMT6 >= min_out_bill_6p)

#negativni bill amount
min_out_bill_1n = max(boxplot.stats(new_data$BILL_AMT1)$out[which(boxplot.stats(new_data$BILL_AMT1)$out < 0)])
index_bill_1n = which(new_data$BILL_AMT1 <= min_out_bill_1n)
min_out_bill_2n = max(boxplot.stats(new_data$BILL_AMT2)$out[which(boxplot.stats(new_data$BILL_AMT2)$out < 0)])
index_bill_2n = which(new_data$BILL_AMT2 <= min_out_bill_2n)
min_out_bill_3n = max(boxplot.stats(new_data$BILL_AMT3)$out[which(boxplot.stats(new_data$BILL_AMT3)$out < 0)])
index_bill_3n = which(new_data$BILL_AMT3 <= min_out_bill_3n)
min_out_bill_4n = max(boxplot.stats(new_data$BILL_AMT4)$out[which(boxplot.stats(new_data$BILL_AMT4)$out < 0)])
index_bill_4n = which(new_data$BILL_AMT4 <= min_out_bill_4n)
min_out_bill_5n = max(boxplot.stats(new_data$BILL_AMT5)$out[which(boxplot.stats(new_data$BILL_AMT5)$out < 0)])
index_bill_5n = which(new_data$BILL_AMT5 <= min_out_bill_5n)
min_out_bill_6n = max(boxplot.stats(new_data$BILL_AMT6)$out[which(boxplot.stats(new_data$BILL_AMT6)$out < 0)])
index_bill_6n = which(new_data$BILL_AMT6 <= min_out_bill_6n)

#unija svih indeksa outliera
index_out = union(index_pay_1, index_pay_2)
index_out = union(index_out, index_pay_3)
index_out = union(index_out, index_pay_4)
index_out = union(index_out, index_pay_5)
index_out = union(index_out, index_pay_6)
index_out = union(index_out, index_bill_1p)
index_out = union(index_out, index_bill_2p)
index_out = union(index_out, index_bill_3p)
index_out = union(index_out, index_bill_4p)
index_out = union(index_out, index_bill_5p)
index_out = union(index_out, index_bill_6p)
index_out = union(index_out, index_bill_1n)
index_out = union(index_out, index_bill_2n)
index_out = union(index_out, index_bill_3n)
index_out = union(index_out, index_bill_4n)
index_out = union(index_out, index_bill_5n)
index_out = union(index_out, index_bill_6n)


new_data_out = new_data[-index_out,]

index_pay2 = which(new_data_out$PAY2 == 8)
new_data_out = new_data_out[-index_pay2,]
index_pay3 = which(new_data_out$PAY3 == 1)
new_data_out = new_data_out[-index_pay3,]
index_pay4 = which(new_data_out$PAY4 == 6)
new_data_out = new_data_out[-index_pay4,]


dim(new_data_out)
summary(new_data_out)
table(new_data_out$DEFAULT)



### Normalizacija

normalize <- function(x) {
    y <- (x - min(x))/(max(x) - min(x))
    y
}

data_numeric = new_data_out[, -(2:11)]
head(data_numeric)
dim(data_numeric)
data_normal2 = lapply(data_numeric[, 1:13], normalize)
data_normal = data.frame(data_normal2)
data_normal[,14:23] = new_data_out[,2:11]
data_normal$DEFAULT<- data_numeric$DEFAULT

head(data_normal)
str(data_normal)
# Sada set podataka ima prvo normalizirane numerièke vrijednosti [, 1:13] zatim kategorièke [,14:23] (osim AGE) i zadnji je DEFAULT

dim(data_normal)
summary(data_normal)
table(data_normal$DEFAULT)

set.seed(128)

# Store row numbers for training set: index_train
index_train = sample(1:nrow(data_normal), 2/3 * nrow(data_normal))

# Create training set: training_set
training_set <- data_normal[index_train, ]

# Create test set: test_set
testing_set <- data_normal[-index_train, ]
test_default = testing_set$DEFAULT

dim(training_set)
table(training_set$DEFAULT)
prop.table(table(training_set$DEFAULT)) * 100

dim(testing_set)
table(testing_set$DEFAULT)
prop.table(table(testing_set$DEFAULT)) * 100


################# MODELIRANJE - KLASIFIKACIJA #################################

#1. Logistièka regresija

head(training_set)
cor(training_set[,-c(14:24)]) #zakljucak: BILL_AMT varijable su jako korelirane, mozda ima smisla promatrati samo jednu od njih u modelu

log_model = glm(DEFAULT ~ ., data = training_set, family = "binomial")

summary(log_model)

logreg_probs = predict(log_model, testing_set, type = "response")
logreg_class = rep("N", 5273)
logreg_class[logreg_probs > 0.5] = "Y"
#logreg_class[logreg_probs <= 0.5] = "N"

table(logreg_class, testing_set$DEFAULT)
mean(logreg_class != testing_set$DEFAULT)

# confint(log_model)



confusionMatrix(table(testing_set$DEFAULT, logreg_class), positive = "Y")

names(testing_set)
out_data <- testing_set
out_data$logreg_class <- factor(logreg_class, levels = c("N", "Y"))
out_data$logreg_y_probs <- logreg_probs


#	png(file = "logreg_roc.png", width = 1600, height = 1600, units = "px", res = 300)

plot.roc(out_data$DEFAULT, logreg_probs ,  measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)

#	dev.off()

#	png(file = "logreg_CI.png", width = 1600, height = 1600, units = "px", res = 300)

rocobj <- plot.roc(out_data$DEFAULT, logreg_probs, percent=FALSE,  
measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE,
ci=TRUE, # compute AUC (of AUC by default)  
print.auc=FALSE) # print the AUC (will contain the CI)  
ciobj <- ci.se(rocobj, # CI of sensitivity  
specificities=seq(0, 1, 0.05)) # over a select set of specificities  
plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape  
#plot(ci(rocobj, of="thresholds", thresholds="best")) # add one threshold

#	dev.off()



#2. NAIVE BAYES


naive_class=naiveBayes(training_set[,-24], training_set$DEFAULT, probability = TRUE)
bayes_class = predict(naive_class, testing_set[,-24])
bayes_probs = predict(naive_class, testing_set[,-24],type = "raw")
table(bayes_class , test_default)
confusionMatrix(bayes_class , test_default, positive = "Y")


#out_data <- testing_set
out_data$bayes_class <- factor(bayes_class, levels = c("N", "Y"))
out_data$bayes_y_probs <- bayes_probs[,2]


summary(out_data)
str(out_data)


#	png(file = "bayes_roc2.png", width = 1600, height = 1600, units = "px", res = 300)

plot.roc(out_data$DEFAULT, out_data$bayes_y_probs,  measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)

#	dev.off()

#	png(file = "bayes_CI.png", width = 1600, height = 1600, units = "px", res = 300)

rocobj <- plot.roc(out_data$DEFAULT, out_data$bayes_y_probs, percent=FALSE,  
measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE,
ci=TRUE, # compute AUC (of AUC by default)  
print.auc=FALSE) # print the AUC (will contain the CI)  
ciobj <- ci.se(rocobj, # CI of sensitivity  
specificities=seq(0, 1, 0.05)) # over a select set of specificities  
plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape  
#plot(ci(rocobj, of="thresholds", thresholds="best")) # add one threshold

#	dev.off()

#naivni bayes uzima i kategorijske varijable, a ostali ne



#3. kNN


head(training_set)
prop.table(table(training_set$DEFAULT)) * 100
prop.table(table(testing_set$DEFAULT)) * 100


trainX <- training_set[,names(training_set) != "DEFAULT"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

set.seed(128)

knnFit <- train(training_set[,-24], training_set$DEFAULT,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 25,
                 trControl = trainControl(method="repeatedcv",repeats = 3),
		     metric = "Kappa",
		     prob=TRUE)

#Output of kNN fit
knnFit


#	png(file = "knn_fit.png", width = 1600, height = 1200, units = "px", res = 300)

plot(knnFit)

#	dev.off()


knn_model = knn(train = training_set[,-24], test = testing_set[,-24], cl = training_set$DEFAULT, k=39, prob=TRUE)

table(testing_set$DEFAULT, knn_model)
confusionMatrix(table(testing_set$DEFAULT, knn_model), positive = "Y")

knn_y_probs <- attr(knn_model, "prob")
knn_indeksi = which(knn_model == "N")
knn_y_probs[knn_indeksi] = 1 - knn_y_probs[knn_indeksi]

#out_data <- testing_set
out_data$knn_class  <- factor(knn_model, levels = c("N", "Y"))
out_data$knn_y_probs <- knn_y_probs


summary(out_data)
str(out_data)

#	png(file = "knn_roc.png", width = 1600, height = 1600, units = "px", res = 300)

plot.roc(out_data$DEFAULT, out_data$knn_y_probs, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)

#	dev.off()


#	png(file = "knn_CI.png", width = 1600, height = 1600, units = "px", res = 300)

rocobj <- plot.roc(out_data$DEFAULT, out_data$knn_y_probs, percent=FALSE,  
measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE,
ci=TRUE, # compute AUC (of AUC by default)  
print.auc=FALSE) # print the AUC (will contain the CI)  
ciobj <- ci.se(rocobj, # CI of sensitivity  
specificities=seq(0, 1, 0.05)) # over a select set of specificities  
plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape  
#plot(ci(rocobj, of="thresholds", thresholds="best")) # add one threshold

#	dev.off()




#4. Random forest

set.seed(128)

rf_model<-train(DEFAULT ~ ., data = training_set,
            preProcess = c("center", "scale"),
            tuneLength = 5,
		method="rf",
            trControl=trainControl(
            		method='cv',
				number=10,
                    	classProbs = TRUE),
		metric = "Kappa")
print(rf_model)


rf_class = predict(rf_model, testing_set)
rf_y_probs = predict(rf_model, testing_set,"prob")[,2]

confusionMatrix(table(testing_set$DEFAULT, rf_class), positive = "Y" )


#out_data <- testing_set
out_data$forest_class  <- factor(rf_class, levels = c("N", "Y"))
out_data$forest_y_probs <- rf_y_probs

summary(out_data)
str(out_data)


#	png(file = "rf_roc.png", width = 1600, height = 1600, units = "px", res = 300)

plot.roc(out_data$DEFAULT, out_data$forest_y_probs, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)

#	dev.off()


#	png(file = "rf_CI.png", width = 1600, height = 1600, units = "px", res = 300)

rocobj <- plot.roc(out_data$DEFAULT, out_data$forest_y_probs, percent=FALSE,  
measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE,
ci=TRUE, # compute AUC (of AUC by default)  
print.auc=FALSE) # print the AUC (will contain the CI)  
ciobj <- ci.se(rocobj, # CI of sensitivity  
specificities=seq(0, 1, 0.05)) # over a select set of specificities  
plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape  
#plot(ci(rocobj, of="thresholds", thresholds="best")) # add one threshold

#	dev.off()


# View the forest results.
plot(rf_model) 

#5. Umjetna neuronska mreža

set.seed(128)

nnet_model<-train(DEFAULT ~ ., data = training_set,
            preProcess = c("center", "scale"),
            tuneLength = 5,
		method="nnet",
            trControl=trainControl(
            		method='cv',
				number=3,
                    	classProbs = TRUE),
		metric = "Kappa")
print(nnet_model)

nnet_class = predict(nnet_model, testing_set, type="raw")
nnet_y_probs = predict(nnet_model, testing_set, type="prob")[,2]

confusionMatrix(table(testing_set$DEFAULT, nnet_class), positive = "Y" )

#out_data <- testing_set
out_data$nnet_class <- factor(nnet_class, levels = c("N", "Y"))
out_data$nnet_y_probs <- nnet_y_probs

summary(out_data)
str(out_data)


#	png(file = "nnet_roc.png", width = 1600, height = 1600, units = "px", res = 300)

plot.roc(out_data$DEFAULT, out_data$nnet_y_probs, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)

#	dev.off()


#	png(file = "nnet_CI.png", width = 1600, height = 1600, units = "px", res = 300)

rocobj <- plot.roc(out_data$DEFAULT, out_data$nnet_y_probs, percent=FALSE,  
measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE,
ci=TRUE, # compute AUC (of AUC by default)  
print.auc=FALSE) # print the AUC (will contain the CI)  
ciobj <- ci.se(rocobj, # CI of sensitivity  
specificities=seq(0, 1, 0.05)) # over a select set of specificities  
plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape  
#plot(ci(rocobj, of="thresholds", thresholds="best")) # add one threshold

#	dev.off()

    
plot(nnet_model)
print(nnet_model)

#### Usporedba modela

auc(out_data$DEFAULT, out_data$logreg_y_probs)
auc(out_data$DEFAULT, out_data$bayes_y_probs)
auc(out_data$DEFAULT, out_data$knn_y_probs)
auc(out_data$DEFAULT, out_data$forest_y_probs)
auc(out_data$DEFAULT, out_data$nnet_y_probs)

confusionMatrix(table(testing_set$DEFAULT, out_data$logreg_class), positive = "Y" )
confusionMatrix(table(testing_set$DEFAULT, out_data$bayes_class), positive = "Y" )
confusionMatrix(table(testing_set$DEFAULT, out_data$knn_class), positive = "Y" )
confusionMatrix(table(testing_set$DEFAULT, out_data$forest_class), positive = "Y" )
confusionMatrix(table(testing_set$DEFAULT, out_data$nnet_class), positive = "Y" )


#	png(file = "roc_comp.png", width = 1600, height = 1600, units = "px", res = 300)

rocobj_logreg <- plot.roc(out_data$DEFAULT, out_data$logreg_y_probs, col=1, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
rocobj_bayes <- lines.roc(out_data$DEFAULT, out_data$bayes_y_probs, col=2, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
rocobj_knn<- lines.roc(out_data$DEFAULT, out_data$knn_y_probs, col=3, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
rocobj_forest<- lines.roc(out_data$DEFAULT, out_data$forest_y_probs, col=4, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
rocobj_nnet<- lines.roc(out_data$DEFAULT, out_data$nnet_y_probs, col=5, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
legend("bottomright", legend=c("Logistièka regresija", "Naivni Bayes", "K najbližih susjeda", "Sluèajna šuma", "Umjetna neuronska mreža"), col=c(1,2,3,4,5), lwd=2)

#	dev.off()


rocobj_logreg <- plot.roc(out_data$DEFAULT, out_data$logreg_y_probs, col=1, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
rocobj_nnet<- lines.roc(out_data$DEFAULT, out_data$nnet_y_probs, col=5, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
testobj <- roc.test(rocobj_nnet, rocobj_logreg, alternative = "greater")
text(0.5, 0.5, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c( "Umjetna neuronska mreža", "Logistièka regresija",), col=c(5,1), lwd=2)

#> testobj 
#
#        DeLong's test for two correlated ROC curves
#
#data:  rocobj_nnet and rocobj_logreg
#Z = 0.24129, p-value = 0.4047
#alternative hypothesis: true difference in AUC is greater than 0
#sample estimates:
#AUC of roc1 AUC of roc2 
#  0.7845326   0.7840780 


rocobj_nnet<- plot.roc(out_data$DEFAULT, out_data$nnet_y_probs, col=5, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
rocobj_forest<- lines.roc(out_data$DEFAULT, out_data$forest_y_probs, col=4, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
legend("bottomright", legend=c("Umjetna neuronska mreža","Sluèajna šuma"), col=c(5,4), lwd=2)
testobj <- roc.test(rocobj_nnet, rocobj_forest, alternative = "greater")
text(0.5, 0.5, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))


#> testobj 
#
#        DeLong's test for two correlated ROC curves
#
#data:  rocobj_nnet and rocobj_forest
#Z = 1.7737, p-value = 0.03806
#alternative hypothesis: true difference in AUC is greater than 0
#sample estimates:
#AUC of roc1 AUC of roc2 
#  0.7845326   0.7764835


rocobj_logreg <- plot.roc(out_data$DEFAULT, out_data$logreg_y_probs, col=1, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
rocobj_forest<- lines.roc(out_data$DEFAULT, out_data$forest_y_probs, col=4, measure = 'tpr', x.measure = 'fpr', legacy.axes = TRUE)
legend("bottomright", legend=c("Logistièka regresija", "Sluèajna šuma"), col=c(1,4), lwd=2)
testobj <- roc.test(rocobj_logreg , rocobj_forest, alternative = "greater")
text(0.5, 0.5, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))

#> testobj 
#
#        DeLong's test for two correlated ROC curves
#
#data:  rocobj_logreg and rocobj_forest
#Z = 1.6677, p-value = 0.04768
#alternative hypothesis: true difference in AUC is greater than 0
#sample estimates:
#AUC of roc1 AUC of roc2 
#  0.7840780   0.7764835 
