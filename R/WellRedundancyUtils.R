RefitModel<-function(csite){

  
#csite[["Fitted.Data"]]
csite[["Reduced.Fitted.Data"]]<-SiteModel$csite$Fitted.Data



tempContData<-csite$Fitted.Data[[1]]$Cont.Data[csite$Fitted.Data[[1]]$Cont.Data$WellName!="MW-02",]
#tempContData$Scenario.Include[tempContData$WellName=="MW02"]<-F


#print(system.time(mod<-fitPSplines(tempContData, csite$GWSDAT_Options$PSplineVars)))
csite$Fitted.Data[[1]]<-fitPSplines(tempContData, csite$GWSDAT_Options$PSplineVars)

mycsite<<-csite
return(csite)
}