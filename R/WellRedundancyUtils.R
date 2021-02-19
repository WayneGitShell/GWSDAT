RefitModel<-function(csite,substance,WellsToOmit){

All.Data<-csite$All.Data
All.Data$Cont.Data<-subset(All.Data$Cont.Data, !WellName %in% WellsToOmit)

csite[["Reduced.Fitted.Data"]]<-fitData(All.Data=All.Data,params=csite$GWSDAT_Options,showProgress = TRUE,calcTrend=FALSE)$Fitted.Data

### Refit GW flows.
temp.GW.Flows <- csite$GW.Flows
temp.GW.Flows<-temp.GW.Flows[!temp.GW.Flows$WellName %in% WellsToOmit,]
temp.GW.Flows<-evalGWFlow(temp.GW.Flows,showErrorMessage=FALSE)
csite[["Reduced.Fitted.Data.GW.Flows"]]<-temp.GW.Flows 
return(csite)
}