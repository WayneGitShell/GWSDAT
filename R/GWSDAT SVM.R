

GWSDAT.svmfit<-function(All.Data,Cont.Name,GWSDAT_Options){


#rearranging names to cope with actual and aggregate dates
Time.Eval <- sort(All.Data$All.Agg.Dates)

temp.Cont.Data <- All.Data$Cont.Data[as.character(All.Data$Cont.Data$Constituent)==Cont.Name,]
temp.Cont.Data <- na.omit(temp.Cont.Data)
names(temp.Cont.Data)[names(temp.Cont.Data)=="AggDate"]<-"AggDatekeep"
names(temp.Cont.Data)[names(temp.Cont.Data)=="SampleDate"]<-"AggDate"



gamma=GWSDAT_Options[["gamma"]]
cost = GWSDAT_Options[["cost"]]

gamma<-try(GWSDAT.Auto.Select.gamma(temp.Cont.Data,gamma))
if(inherits(gamma, "try-error")){gamma=NA}#else{gamma=1/gamma}#Wayne


svm.temp<-try(tune.svm(log(Result.Corr.ND)~AggDate+XCoord+YCoord,data=temp.Cont.Data, 
gamma = gamma, cost = cost,scale=T,
tunecontrol=tune.control(cross=min(GWSDAT_Options[["cross"]],nrow(temp.Cont.Data)))))


if(!inherits(svm.temp, "try-error")){

	temp.Cont.Data$ModelPred<-exp(predict(svm.temp$best.model,newdata=temp.Cont.Data))

}else{

	temp.Cont.Data$ModelPred<-rep(NA,nrow(temp.Cont.Data))

}

names(temp.Cont.Data)[names(temp.Cont.Data)=="AggDate"]<-"SampleDate"
names(temp.Cont.Data)[names(temp.Cont.Data)=="AggDatekeep"]<-"AggDate"

temp.Cont.Data$Result.Corr.ND[!is.finite(temp.Cont.Data$Result.Corr.ND)]<-NA #Wayne V3 coerce -inf to NA for NAPL only data sets. 

list(Cont.Data=temp.Cont.Data,Model.tune=svm.temp,Time.Eval=Time.Eval)

}
