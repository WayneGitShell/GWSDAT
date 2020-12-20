RefitModel<-function(csite,substance,WellsToOmit){

All.Data<-csite$All.Data
All.Data$Cont.Data<-subset(All.Data$Cont.Data, !WellName %in% WellsToOmit)

csite[["Reduced.Fitted.Data"]]<-fitData(All.Data=All.Data,param=csite$GWSDAT_Options,showProgress = TRUE,calcTrend=FALSE)$Fitted.Data

return(csite)
}