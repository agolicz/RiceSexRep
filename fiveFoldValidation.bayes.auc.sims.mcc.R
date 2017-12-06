#https://rpubs.com/jhofman/nb_vs_lr
#http://joshwalters.com/2012/11/27/naive-bayes-classification-in-r.html
library(e1071)
library(caret)
library(ROCR)
library(DescTools)
library(mltools)
set.seed(100)
sim.res<-data.frame()
for (n in 1:500){
        f<-paste("sim.cod.",n,".split",sep="")
	load(f)
	nb.sen = c()
	nb.spe = c()
	nb.acc = c()
	nb.auc = c()
        nb.mcc = c()
	pdfc<-data.frame()
	for (i in 1:3) {
        	d2.1=d2[d2$fold != i,]
        	d2.1$fold<-NULL
        	d2.2=d2[d2$fold == i,]
        	d2.2$fold<-NULL
		m.nbi = naiveBayes(CLASS ~ ET + EV + H + P + CP + D, data=d2.1)
        	predictions = predict(m.nbi, d2.2)
		predictions2 = predict(m.nbi, d2.2,type='raw')
		cm<-confusionMatrix(table(predictions,d2.2$CLASS))
        	nb.sen = append(cm$byClass['Sensitivity'], nb.sen)
        	nb.spe = append(cm$byClass['Specificity'], nb.spe)
		nb.acc = append(cm$overall['Accuracy'], nb.acc)
		pa<-predictions
		pa<-gsub("rep","TRUE", pa)
		pa<-gsub("non","FALSE", pa)
                ta<-d2.2$CLASS
		ta<-gsub("rep","TRUE", ta)
		ta<-gsub("non","FALSE", ta)
		mccv<-mcc(as.logical(pa),as.logical(ta))
                nb.mcc = append(mccv, nb.mcc)
		score <- predictions2[, c("rep")]
		actual_class <- d2.2$CLASS
        	pred <- prediction(score, actual_class)
		perf <- performance(pred, "tpr", "fpr")
		roc <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))
		nb.auc = append(AUC(roc$fpr, roc$tpr),nb.auc)
        	roc$fold <- paste("fold",as.character(i),sep="")
        	roc$method <- "naive bayes"
        	print(dim(roc))
        	pdfc <- rbind(pdfc,roc)
	}
	tdf<-data.frame(value=nb.sen,measure=rep("Sensitivity", length(nb.sen)), method="Naive Bayes")
	sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=nb.spe,measure=rep("Specificity", length(nb.spe)), method="Naive Bayes")
        sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=nb.acc,measure=rep("Accuracy", length(nb.acc)), method="Naive Bayes")
	sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=nb.auc,measure=rep("AUC", length(nb.auc)), method="Naive Bayes")
	sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=nb.mcc,measure=rep("MCC", length(nb.mcc)), method="Naive Bayes")
	sim.res<-rbind(tdf,sim.res)
}

save(sim.res, file="sims.coding.bayes")

set.seed(100)
sim.res<-data.frame()
for (n in 1:500){
	f<-paste("sim.nc.",n,".split",sep="")
	load(f)
	nb.sen = c()
	nb.spe = c()
	nb.acc = c()
	nb.auc = c()
	nb.mcc = c()
	pdfnc<-data.frame()
	for (i in 1:3) {
        	d2.1=d2[d2$fold != i,]
        	d2.1$fold<-NULL
        	d2.2=d2[d2$fold == i,]
        	d2.2$fold<-NULL
       		m.nbi = naiveBayes(CLASS ~ ET + EV + P + CP + CT + D, data=d2.1)
        	predictions = predict(m.nbi, d2.2)
        	predictions2 = predict(m.nbi, d2.2,type='raw')
        	cm<-confusionMatrix(table(predictions,d2.2$CLASS))
        	nb.sen = append(cm$byClass['Sensitivity'], nb.sen)
        	nb.spe = append(cm$byClass['Specificity'], nb.spe)
        	nb.acc = append(cm$overall['Accuracy'], nb.acc)
		pa<-predictions
                pa<-gsub("rep","TRUE", pa)
                pa<-gsub("non","FALSE", pa)
                ta<-d2.2$CLASS
                ta<-gsub("rep","TRUE", ta)
                ta<-gsub("non","FALSE", ta)
                mccv<-mcc(as.logical(pa),as.logical(ta))
                nb.mcc = append(mccv, nb.mcc)
        	score <- predictions2[, c("rep")]
        	actual_class <- d2.2$CLASS
        	pred <- prediction(score, actual_class)
        	perf <- performance(pred, "tpr", "fpr")
		roc <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))
		nb.auc = append(AUC(roc$fpr, roc$tpr),nb.auc)
        	roc$fold <- paste("fold",as.character(i),sep="")
        	roc$method <- "naive bayes"
        	print(dim(roc))
        	pdfnc <- rbind(pdfnc,roc)
	}
	tdf<-data.frame(value=nb.sen,measure=rep("Sensitivity", length(nb.sen)), method="Naive Bayes")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=nb.spe,measure=rep("Specificity", length(nb.spe)), method="Naive Bayes")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=nb.acc,measure=rep("Accuracy", length(nb.acc)), method="Naive Bayes")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=nb.auc,measure=rep("AUC", length(nb.auc)), method="Naive Bayes")
        sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=nb.mcc,measure=rep("MCC", length(nb.mcc)), method="Naive Bayes")
        sim.res<-rbind(tdf,sim.res)
}
save(sim.res, file="sims.noncoding.bayes")

