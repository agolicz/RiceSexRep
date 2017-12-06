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
	lg.sen = c()
	lg.spe = c()
	lg.acc = c()
	lg.auc = c()
	lg.mcc = c()
	pdfc<-data.frame()
	for (i in 1:3) {
        	d2.1=d2[d2$fold != i,]
        	d2.1$fold<-NULL
        	d2.2=d2[d2$fold == i,]
        	d2.2$fold<-NULL
		m.log<-glm(CLASS ~ ET + EV + H + CP + CT + D, data=d2.1, family="binomial", control = list(maxit = 100))
        	predictions = predict(m.log, d2.2)
		predictions2 = predict(m.log, d2.2, type="response")
		pdata <- predict(m.log, d2.2, type = "response")
		pdata2<-as.character(pdata > 0.5)
		names(pdata2)<-names(pdata)
		pdata2<-gsub("TRUE", "rep", pdata2)
		pdata2<-gsub("FALSE", "non", pdata2)
		pdata2[is.na(pdata2)]<-"non"
		cm<-confusionMatrix(table(pdata2,d2.2$CLASS))
        	lg.sen = append(cm$byClass['Sensitivity'], lg.sen)
        	lg.spe = append(cm$byClass['Specificity'], lg.spe)
		lg.acc = append(cm$overall['Accuracy'], lg.acc)
		pa<-pdata2
                pa<-gsub("rep","TRUE", pa)
                pa<-gsub("non","FALSE", pa)
                ta<-d2.2$CLASS
                ta<-gsub("rep","TRUE", ta)
                ta<-gsub("non","FALSE", ta)
                mccv<-mcc(as.logical(pa),as.logical(ta))
                lg.mcc = append(mccv, lg.mcc)
		actual_class <- d2.2$CLASS
        	pred <- prediction(predictions2, actual_class)
		perf <- performance(pred, "tpr", "fpr")
		roc <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))
		lg.auc = append(AUC(roc$fpr, roc$tpr),lg.auc)
        	roc$fold <- paste("fold",as.character(i),sep="")
        	roc$method <- "logistic regression"
        	print(dim(roc))
        	pdfc <- rbind(pdfc,roc)
	}
	tdf<-data.frame(value=lg.sen,measure=rep("Sensitivity", length(lg.sen)), method="Logistic Regression")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=lg.spe,measure=rep("Specificity", length(lg.spe)), method="Logistic Regression")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=lg.acc,measure=rep("Accuracy", length(lg.acc)), method="Logistic Regression")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=lg.auc,measure=rep("AUC", length(lg.auc)), method="Logistic Regression")
	sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=lg.mcc,measure=rep("MCC", length(lg.mcc)), method="Logistic Regression")
        sim.res<-rbind(tdf,sim.res)
}

save(sim.res, file="sims.coding.logistic")

set.seed(100)
sim.res<-data.frame()

for (n in 1:500){
        f<-paste("sim.nc.",n,".split",sep="")
        load(f)
	lg.sen = c()
	lg.spe = c()
	lg.acc = c()
	lg.auc = c()
	lg.mcc = c()
	pdfnc<-data.frame()
	for (i in 1:3) {
        	d2.1=d2[d2$fold != i,]
        	d2.1$fold<-NULL
        	d2.2=d2[d2$fold == i,]
        	d2.2$fold<-NULL
        	m.log = glm(CLASS ~ ET + EV + CP + CT + D, data=d2.1, family="binomial")
       		predictions = predict(m.log, d2.2)
        	predictions2 = predict(m.log, d2.2, type="response")
		pdata <- predict(m.log, d2.2, type = "response")
        	pdata2<-as.character(pdata > 0.5)
        	names(pdata2)<-names(pdata)
        	pdata2<-gsub("TRUE", "rep", pdata2)
        	pdata2<-gsub("FALSE", "non", pdata2)
		pdata2[is.na(pdata2)]<-"non"
        	cm<-confusionMatrix(table(pdata2,d2.2$CLASS))
        	lg.sen = append(cm$byClass['Sensitivity'], lg.sen)
        	lg.spe = append(cm$byClass['Specificity'], lg.spe)
        	lg.acc = append(cm$overall['Accuracy'], lg.acc)
		pa<-pdata2
                pa<-gsub("rep","TRUE", pa)
                pa<-gsub("non","FALSE", pa)
                ta<-d2.2$CLASS
                ta<-gsub("rep","TRUE", ta)
                ta<-gsub("non","FALSE", ta)
                mccv<-mcc(as.logical(pa),as.logical(ta))
                lg.mcc = append(mccv, lg.mcc)
        	actual_class <- d2.2$CLASS
       		pred <- prediction(predictions2, actual_class)
        	perf <- performance(pred, "tpr", "fpr")
		roc <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))
		lg.auc = append(AUC(roc$fpr, roc$tpr),lg.auc)
        	roc$fold <- paste("fold",as.character(i),sep="")
        	roc$method <- "logistic regression"
        	print(dim(roc))
       		pdfnc <- rbind(pdfnc,roc)
	}
	tdf<-data.frame(value=lg.sen,measure=rep("Sensitivity", length(lg.sen)), method="Logistic Regression")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=lg.spe,measure=rep("Specificity", length(lg.spe)), method="Logistic Regression")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=lg.acc,measure=rep("Accuracy", length(lg.acc)), method="Logistic Regression")
        sim.res<-rbind(tdf,sim.res)
        tdf<-data.frame(value=lg.auc,measure=rep("AUC", length(lg.auc)), method="Logistic Regression")
	sim.res<-rbind(tdf,sim.res)
	tdf<-data.frame(value=lg.mcc,measure=rep("MCC", length(lg.mcc)), method="Logistic Regression")
        sim.res<-rbind(tdf,sim.res)
}

save(sim.res, file="sims.noncoding.logistic")
