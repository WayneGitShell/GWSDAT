#
# This is the stand-alone test script.
#


source("R/GWSDAT MakePanel.R")
source("R/GWSDAT_Setup.R")


COMPREHENSIVE_EXAMPLE = FALSE

GWSDAT_Setup()

options(warn = 1) #, error=1)

GWSDAT_Options = createOptions()

#
# Change some Option for testing.
#
if (COMPREHENSIVE_EXAMPLE) {
  GWSDAT_Options[['SiteName']] <- 'Comprehensive Example'
  GWSDAT_Options[['WellDataFilename']] <- 'data/ComprehensiveExample_WellData.csv'
  GWSDAT_Options[['WellCoordsFilename']] <- 'data/ComprehensiveExample_WellCoords.csv'
  GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'data/GIS_Files/GWSDATex2.shp')
}

pnl = GWSDAT_Init(GWSDAT_Options)

## Get return status and display on page.
if (class(pnl) == "GWSDAT_Error")
  stop(pnl$msg)
if (class(pnl) == "GWSDAT_Warning")
  stop(pnl$msg)



GWSDAT.Make.Panel(pnl)

#pnl = Create_PanelAttr(ret$Curr_Site_Data)
#Plot_ImagePlot(pnl)


#Plot_SmoothTimeSeries(pnl)




#
# Prepare the input data with the selected Aquifer.
#
#pnl$All.Data <- prepare_data(pnl$All.Data$solute_data, 
#                                  pnl$All.Data$well_data, 
#                                  pnl$GWSDAT_Options, 
#                                  Aq_sel = "A")


# Fit the data.
#pnl$Fitted.Data <- GWSDAT_Fit_Data(pnl$All.Data, pnl$GWSDAT_Options)

# Create a complete GWSDAT instance with data, model, and options. 
#new_pnl <- Create_PanelAttr(pnl)
#Plot_ImagePlot(new_pnl)




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

