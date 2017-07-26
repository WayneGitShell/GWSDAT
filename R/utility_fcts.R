

########################################################################################################################

GWSDAT.excelDate2Date <- function(excelDate) 
{
  Date <- excelDate + as.Date("1900-01-01") - 2
  return(Date)
}
#----------------------------------------------------------------------------------------------------------------------#


########################################################################################################################

.my.tkdev <- function(hscale = 1, vscale = 1){
  win.metafile(width = 4 * hscale, height = 4 * vscale, restoreConsole = FALSE)
}

#----------------------------------------------------------------------------------------------------------------------#


########################################################################################################################

rm_spaces <- function(x){
  
  #Function to remove trailing and leading spaces!
  if (!is.character(x)) { stop("not of class character") }

  x <- sub('[[:blank:]]+?','',x)
  x <- sub(" *$","",x)
  
  return(x)
}

#------------------------------------------------------------------------------------------------------------#

napl_exists <- function(All.Data, well, solute) {

  Well.Data <- All.Data$Cont.Data[as.character(All.Data$Cont.Data$WellName) == 
                                    well & All.Data$Cont.Data$Constituent == solute,]
  
  NAPL.Present <- any("napl" %in% tolower(as.character(Well.Data$Result))) || 
    nrow(All.Data$NAPL.Thickness.Data[as.character(All.Data$NAPL.Thickness.Data$WellName) == well,]) > 0
  
  if (is.na( NAPL.Present)) { NAPL.Present <- FALSE }
  
  return(NAPL.Present)
  
}
