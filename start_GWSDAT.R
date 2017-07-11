#
# This is the stand-alone test script.
#


source("R/GWSDAT_Setup.R")

options(warn=1) #, error=1)

GWSDAT_Options = GWSDAT_Setup()




ret = GWSDAT_Init(GWSDAT_Options)

## Get return status and display on page.
if(class(ret$status) == "GWSDAT_Error")
  stop(ret$status$msg)
if(class(ret$status) == "GWSDAT_Warning")
  stop(ret$status$msg)



#GWSDAT.Make.Panel(ret$Curr_Site_Data)

pnl = Create_PanelAttr(ret$Curr_Site_Data, RUNNING_SHINY = TRUE)
Plot_ImagePlot(pnl)


#Plot_SmoothTimeSeries(pnl)