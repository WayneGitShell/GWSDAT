#
# This is the stand-alone test script.
#


#source("R/GWSDAT_Setup.R")
source("R/create_GWSDAT_Instance.R")
source("R/GWSDAT MakePanel.R")
source("R/GWSDAT_Setup.R")


GWSDAT_Setup()

options(warn = 1) #, error=1)

GWSDAT_Options = create_GWSDAT_Instance()

#
# Change some Option for testing.
# 
GWSDAT_Options[['WellDataFilename']] <- 'data/ComprehensiveExample_WellData.csv'
GWSDAT_Options[['WellCoordsFilename']] <- 'data/ComprehensiveExample_WellCoords.csv'


ret = GWSDAT_Init(GWSDAT_Options)

## Get return status and display on page.
if (class(ret$status) == "GWSDAT_Error")
  stop(ret$status$msg)
if (class(ret$status) == "GWSDAT_Warning")
  stop(ret$status$msg)



#GWSDAT.Make.Panel(ret$Curr_Site_Data)

pnl = Create_PanelAttr(ret$Curr_Site_Data)
Plot_ImagePlot(pnl)


#Plot_SmoothTimeSeries(pnl)




#
# Prepare the input data with the selected Aquifer.
#
pnl$All.Data <- prepare_data(pnl$All.Data$solute_data, 
                                  pnl$All.Data$well_data, 
                                  pnl$GWSDAT_Options, 
                                  Aq_sel = "A")


# Fit the data.
pnl$Fitted.Data <- GWSDAT_Fit_Data(pnl$All.Data, pnl$GWSDAT_Options)

browser()
# Create a complete GWSDAT instance with data, model, and options. 
new_pnl <- Create_PanelAttr(pnl)
Plot_ImagePlot(new_pnl)




#
# Prepare the input data with the selected Aquifer.
#
#pnl$All.Data <- prepare_data(pnl$All.Data$solute_data, 
#                             pnl$All.Data$well_data, 
#                             pnl$GWSDAT_Options 
#                             )


# Fit the data.
#pnl$Fitted.Data <- GWSDAT_Fit_Data(pnl$All.Data, pnl$GWSDAT_Options)


# Create a complete GWSDAT instance with data, model, and options. 
#pnl <- Create_PanelAttr(pnl)
#Plot_ImagePlot(pnl)

