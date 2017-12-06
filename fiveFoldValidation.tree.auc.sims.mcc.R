#library(e1071)
library(rpart)
library(caret)
library(ROCR)
library(DescTools)

set.seed(100)
sim.res<-data.frame()
for (n in 1:500){
        f<-paste("sim.cod.",n,".split",sep="")
        load(f)
	tr.sen = c()
	tr.spe = c()
	tr.acc = c()
	tr.auc = c()
	tr.mcc = c()
	pdfc<-data.frame()
	for (i in 1:3) {
        	d2.1=d2[d2$fold != i,]
        	d2.1$fold<-NULL
        	d2.2=d2[d2$fold == i,]
        	d2.2$fold<-NULL
        	m.tree<-rpart(CLASS ~ ET + EV + P + H + CP + CT + D, data=d2.1,method="class")
        	predictions = predict(m.tree, d2.2, type="class")
        	predictions2 = predict(m.tree, d2.2)
        	cm<-confusionMatrix(table(predictions,d2.2$CLASS))
        	tr.sen = append(cm$byClass['Sensitivity'], tr.sen)
        	tr.spe = append(cm$byClass['Specificity'], tr.spe)
        	tr.acc = append(cm$overall['Accuracy'], tr.acc)
		pa<-predictions
                pa<-gsub("rep","TRUE", pa)
                pa<-gsub("non","FALSE", pa)
                ta<-d2.2$CLASS
                ta<-gsub("rep","TRUE", ta)
                ta<-gsub("non","FALSE", ta)
                mccv<-mcc(as.logical(pa),as.logical(ta))
                tr.mcc = append(mccv, tr.mcc)
        	score <- predictions2[, c("rep")]
        	actual_class <- d2.2$CLASS
        	pred <- prediction(score, actual_class)
        	perf <- performance(pred, "tpr", "fpr")
        	roc <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))
		tr.auc = append(AUC(roc$fpr, roc$tpr),tr.auc)
        	roc$fold <- paste("fold",as.character(i),sep="")
        	roc$method <- "decision tree"
        	print(dim(roc))
        	pdfc <- rbind(pdfc,roc)
	}
	tdf<-data.frame(value=tr.sen,measure=rep("Sensitivity", length(tr.sen)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=tr.spe,measure=rep("Specificity", length(tr.spe)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=tr.acc,measure=rep("Accuracy", length(tr.acc)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=tr.auc,measure=rep("AUC", length(tr.auc)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=tr.mcc,measure=rep("MCC", length(tr.mcc)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
}
save(sim.res, file="sims.coding.tree")

set.seed(100)
sim.res<-data.frame()
for (n in 1:500){
        f<-paste("sim.nc.",n,".split",sep="")
        load(f)
	tr.sen = c()
	tr.spe = c()
	tr.acc = c()
	tr.auc = c()
	tr.mcc = c()
	pdfnc<-data.frame()
	for (i in 1:3) {
        	d2.1=d2[d2$fold != i,]
        	d2.1$fold<-NULL
        	d2.2=d2[d2$fold == i,]
        	d2.2$fold<-NULL
        	m.tree<-rpart(CLASS ~ ET + EV + P + CP + CT + D, data=d2.1,method="class")
        	predictions = predict(m.tree, d2.2, type="class")
        	predictions2 = predict(m.tree, d2.2)
        	cm<-confusionMatrix(table(predictions,d2.2$CLASS))
        	tr.sen = append(cm$byClass['Sensitivity'], tr.sen)
        	tr.spe = append(cm$byClass['Specificity'], tr.spe)
        	tr.acc = append(cm$overall['Accuracy'], tr.acc)
		pa<-predictions
                pa<-gsub("rep","TRUE", pa)
                pa<-gsub("non","FALSE", pa)
                ta<-d2.2$CLASS
                ta<-gsub("rep","TRUE", ta)
                ta<-gsub("non","FALSE", ta)
                mccv<-mcc(as.logical(pa),as.logical(ta))
                tr.mcc = append(mccv, tr.mcc)
        	score <- predictions2[, c("rep")]
        	actual_class <- d2.2$CLASS
        	pred <- prediction(score, actual_class)
        	perf <- performance(pred, "tpr", "fpr")
 		roc <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))
		tr.auc = append(AUC(roc$fpr, roc$tpr),tr.auc)
        	roc$fold <- paste("fold",as.character(i),sep="")
        	roc$method <- "decision tree"
        	print(dim(roc))
        	pdfnc <- rbind(pdfnc,roc)	
	}
	tdf<-data.frame(value=tr.sen,measure=rep("Sensitivity", length(tr.sen)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=tr.spe,measure=rep("Specificity", length(tr.spe)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=tr.acc,measure=rep("Accuracy", length(tr.acc)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=tr.auc,measure=rep("AUC", length(tr.auc)), method="Classification Tree")
	sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=tr.mcc,measure=rep("MCC", length(tr.mcc)), method="Classification Tree")
        sim.res<-rbind(tdf,sim.res)
}
save(sim.res, file="sims.noncoding.tree")
