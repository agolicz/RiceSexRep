library(e1071)
library(caret)
library(ROCR)
library(DescTools)
library(mltools)

set.seed(2)

t<-read.csv("RI.scores.cod.both.bayes.na.yn",sep="\t", header=F, row.names=1)
names(t)<-c("ET","EV","P","H","CP","CT","D","CLASS")
id<-sample(2,nrow(t),prob=c(0.7,0.3),replace=T)
emptrain<-t[id==1,]
emptest<-t[id==2,]
emp_nb<-naiveBayes(CLASS ~ ET + EV + P + H + CP + D, data=emptrain)
save(emp_nb,file="bayes.c.classifier.model")
predictions = predict(emp_nb, emptest)
predictions2 = predict(emp_nb, emptest,type='raw')
cm<-confusionMatrix(table(predictions,emptest$CLASS), positive="rep")
nb.sen = cm$byClass['Sensitivity']
nb.spe = cm$byClass['Specificity']
nb.acc = cm$overall['Accuracy']
pa<-predictions
pa<-gsub("rep","TRUE", pa)
pa<-gsub("non","FALSE", pa)
ta<-emptest$CLASS
ta<-gsub("rep","TRUE", ta)
ta<-gsub("non","FALSE", ta)
nb.mcc<-mcc(as.logical(pa),as.logical(ta))
score <- predictions2[, c("rep")]
actual_class <- emptest$CLASS
pred <- prediction(score, actual_class)
perf <- performance(pred, "tpr", "fpr")
roc <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))
nb.auc = AUC(roc$fpr, roc$tpr)
roc$method <- "Naive Bayes"
save(roc,file="bayes.c.roc")
print(nb.sen)
print(nb.spe)
print(nb.acc)
print(nb.auc)
print(nb.mcc)
#print(cm)

t2<-read.csv("RI.scores.forBayes.na.yn.cod",sep="\t",header=F, row.names=1)
names(t2)<-c("ET","EV","P","H","CP","CT","D")
res<-predict(emp_nb,t2)
t2$RES<-res
write.table(t2,file="bayes.roc.cod.tsv",sep="\t",row.names=T,quote=F)


t<-read.csv("RI.scores.nc.both.bayes.na.yn",sep="\t", header=F, row.names=1)
names(t)<-c("ET","EV","P","H","CP","CT","D","CLASS")
id<-sample(2,nrow(t),prob=c(0.7,0.3),replace=T)
emptrain<-t[id==1,]
emptest<-t[id==2,]
emp_nb<-naiveBayes(CLASS ~ ET + EV + P + CP + CT + D, data=emptrain)
save(emp_nb,file="bayes.nc.classifier.model")
predictions = predict(emp_nb, emptest)
predictions2 = predict(emp_nb, emptest,type='raw')
cm<-confusionMatrix(table(predictions,emptest$CLASS), positive="rep")
nb.sen = cm$byClass['Sensitivity']
nb.spe = cm$byClass['Specificity']
nb.acc = cm$overall['Accuracy']
pa<-predictions
pa<-gsub("rep","TRUE", pa)
pa<-gsub("non","FALSE", pa)
ta<-emptest$CLASS
ta<-gsub("rep","TRUE", ta)
ta<-gsub("non","FALSE", ta)
nb.mcc<-mcc(as.logical(pa),as.logical(ta))
score <- predictions2[, c("rep")]
actual_class <- emptest$CLASS
pred <- prediction(score, actual_class)
perf <- performance(pred, "tpr", "fpr")
roc <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))
nb.auc = AUC(roc$fpr, roc$tpr)
roc$method <- "Naive Bayes"

save(roc,file="bayes.nc.roc")
print(nb.sen)
print(nb.spe)
print(nb.acc)
print(nb.auc)
print(nb.mcc)
#print(cm)

t2<-read.csv("RI.scores.forBayes.na.yn.nc",sep="\t",header=F, row.names=1)
names(t2)<-c("ET","EV","P","H","CP","CT","D")
res<-predict(emp_nb,t2)
t2$RES<-res
write.table(t2,file="bayes.roc.nc.tsv",sep="\t",row.names=T,quote=F)
