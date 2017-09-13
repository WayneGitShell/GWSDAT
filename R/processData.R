

formatData <- function(solute_data, sample_loc) {
  
  ## Format solute concentration data ##########################################
  
  # Replace NA flags and delete rows that have the flag "omit".
  solute_data$Flags[is.na(solute_data$Flags)] = ""
  solute_data <- solute_data[tolower(as.character(solute_data$Flags)) != "omit",]
  
  if (class(solute_data$SampleDate) == "integer") {
    
    solute_data$SampleDate <- excelDate2Date(solute_data$SampleDate) 
    
  #} else if (class(solute_data$SampleDate)[1] %in% c("POSIXct", "POSIXt")) {
  } else if (class(solute_data$SampleDate) %in% c("POSIXct", "POSIXt")) {
      # Watch for time zone differences here!
      solute_data$SampleDate <- format(solute_data$SampleDate + 12*60*60,"%Y-%m-%d") 
      solute_data$SampleDate <- as.Date(solute_data$SampleDate,"%Y-%m-%d")
    
  } 
  # else {
  #   msg = "Trouble reading Date Format, Please convert Input Excel Data to Date format."
  #   showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
  #   return(NULL)
  # }
  
  solute_data$WellName <- factor(rm_spaces(as.character(solute_data$WellName)))
  solute_data$Result <- factor(rm_spaces(as.character(solute_data$Result)))
  solute_data$Units <- factor(rm_spaces(as.character(solute_data$Units)))
  solute_data$Constituent <- factor(rm_spaces(as.character(solute_data$Constituent)))
  
  if (length(unique(as.character(solute_data$Constituent))) != 
      length(unique(toupper(as.character(solute_data$Constituent))))) {
    
    msg = "Warning: Constituent types have different letter cases (e.g. 'MTBE v mtbe'). All names are transformed to upper case."
    showNotification(msg, type = "warning", duration = 10)
    
    solute_data$Constituent <- factor(toupper(as.character(solute_data$Constituent)))
  }
  
  
  ##  Tranform Aquifer #########################################################
  
  sample_loc$data$Aquifer <- as.character(sample_loc$data$Aquifer)  
  sample_loc$data$Aquifer[sample_loc$data$Aquifer == ""] <- "Blank"
  sample_loc$data$Aquifer[is.na(sample_loc$data$Aquifer)] <- "Blank"

  sample_loc$data$WellName <- factor(rm_spaces(as.character(sample_loc$data$WellName)))
    
  sample_loc$data$XCoord <- as.numeric(rm_spaces(as.character(sample_loc$data$XCoord)))
  sample_loc$data$YCoord <- as.numeric(rm_spaces(as.character(sample_loc$data$YCoord)))
  sample_loc$data <- na.omit(sample_loc$data)
  sample_loc$data <- unique(sample_loc$data)
  
  
  return(list(solute_data = solute_data, sample_loc = sample_loc))
}





#' @importFrom splancs areapl
processData <- function(solute_data, sample_loc, GWSDAT_Options, Aq_sel = "Blank",
                        shape_file_data) {


  #Pick up Electron Acceptors before deleting non-aquifer wells. 
  ElecAccepts <- unique(as.character(solute_data[ tolower(as.character(solute_data$Flags)) %in% c("e-acc","notinnapl","redox"),"Constituent"]))
 
  well_tmp_data <- sample_loc$data[sample_loc$data$Aquifer == Aq_sel,]
 
  if (nrow(well_tmp_data) == 0) {
    showNotification(paste0("No wells selected with Aquifer ", Aq_sel), type = "error")
    return(NULL)
  }
  
  # Keep only the following columns in the well_tmp_data table.
  well_tmp_data <- well_tmp_data[,c("WellName","XCoord","YCoord")]
  
  
  if (any(table(well_tmp_data$WellName) > 1)) {
    msg = "Found non-unique well names in well coordinate table."
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
  }
  
  
  if (nrow(unique(well_tmp_data[,c("XCoord","YCoord")])) < nrow(well_tmp_data)) {
    msg <- paste0("Aquifer \'", Aq_sel, "\': Non-Unique Well Coordinates found. Corresponding Groundwater elevations will be substituted by their mean value.")
    showNotification(msg, type = "warning", duration = 10)
  }
  
 
  
  # Keep concentration data that also exists in the well coordinate table.
  solute_data <- solute_data[solute_data$WellName %in% well_tmp_data$WellName,]
  
  # Extract the unique well names from concentration data table.
  sample_loc_names <- sort(unique(as.character(solute_data$WellName)))
  
  # Lookup well coordinates for the extracted concentrations.
  well_tmp_data <- well_tmp_data[as.character(well_tmp_data$WellName) %in% sample_loc_names,]
  
  
  Cont.Data <- solute_data[tolower(as.character(solute_data$Constituent)) != "gw",]
  
  
  ############### Contaminant Data Type Processing #############################
  ContTypeData = "Default"
  
  # GW only type data. 
  if (nrow(Cont.Data) == 0) { 
  
  	ContTypeData <- "NoConcData"
  	Cont.Data    <- rbind(Cont.Data,
  	                      data.frame(WellName    = as.character(sample_loc_names[1]),
  	                                 Constituent = " ",
  	                                 SampleDate  = max(solute_data$SampleDate), 
  	                                 Result      = NA, 
  	                                 Units       = "ug/l", 
  	                                 Flags       = ""))
  }
  
  # NAPL Only type data
  if (nrow(Cont.Data[tolower(as.character(Cont.Data$Constituent)) != "napl",]) == 0) { 
  
  	ContTypeData <- "NoConcData"
  	Cont.Data    <- rbind(Cont.Data, 
  	                      data.frame(WellName    = as.character(sample_loc_names[1]),
  	                                 Constituent = " ",
  	                                 SampleDate  = max(solute_data$SampleDate), 
  	                                 Result      = NA, 
  	                                 Units       = "ug/l", 
  	                                 Flags       = "")
  	                      )
  }
  
  
  Cont.Data$Constituent <- factor(as.character(Cont.Data$Constituent))
  cont_names <- unique(as.character(Cont.Data$Constituent))
  
  
  ########################## Units Checking ####################################
  if (any(!tolower(as.character(Cont.Data$Units[tolower(as.character(Cont.Data$Constituent)) != "napl"])) %in% c("ug/l","mg/l","ng/l"))) {

    msg = "Solute data must be one of 'ng/l', 'ug/l' or 'mg/l'. Please correct and re-run GWSDAT analysis."
    showModal(modalDialog(title = "Units Error", msg, easyClose = FALSE))
    return(NULL)
  }
  
  
  
  
  
  Cont.Data$ND <- rep(FALSE,nrow(Cont.Data))
  Cont.Data$ND[grep("<", as.character(Cont.Data$Result))] <- TRUE
  Cont.Data$Result.Corr.ND <- rep(NA,nrow(Cont.Data))
  Cont.Data$Result.Corr.ND[!Cont.Data$ND] <- as.numeric(as.character(Cont.Data$Result[!Cont.Data$ND]))
  
  
  ############# Checking for 0 conc concentration data #########################
  zero_conc <- which(Cont.Data$Result.Corr.ND[tolower(Cont.Data$Constituent) != "napl"] == 0)
  non_zero  <- which(Cont.Data$Result.Corr.ND[tolower(Cont.Data$Constituent) != "napl"] != 0)
  
  if (length(zero_conc) > 0) {
    Cont.Data <- Cont.Data[-zero_conc,] 
    showNotification(paste0("Ignoring ", length(zero_conc), "/", length(non_zero), " zero concentration entries for Aquifer \'", Aq_sel, "\'."),
                     duration = 10)
  }
  
  if (nrow(Cont.Data) == 0)  {
    showNotification(paste0("No concentration data (valid and ND) present for Aquifer ", Aq_sel, ", skipping."),
                     type = "warning", duration = 10)
  }
  
  # if (any(Cont.Data$Result.Corr.ND[tolower(Cont.Data$Constituent) != "napl"] == 0,na.rm = TRUE)) {
  # 
  #   msg = "Zero solute concentration data detected in input data - this is not permissible. Please correct and re-run GWSDAT analysis."
  #   showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
  #   return(NULL)
  #   
  # }
  
  
  
  
  temp.hold <- sub(".*<", "", as.character(Cont.Data$Result[Cont.Data$ND]))
  
  if (any(grep("nd",temp.hold,ignore.case = T))) {
    msg <- "Warning: '<ND' detected. Non-Detect limits must be specified. Omitting unspecified Non Detect Data."
    showNotification(msg, type = "warning", duration = 10)    
  }
  
  Cont.Data$Result.Corr.ND[Cont.Data$ND] <- as.numeric(temp.hold)
  
  
  
  ############# Solute Unit Handling converts all ng/l and mg/l to ug/l ########
  Cont.Data$Units <- tolower(as.character(Cont.Data$Units))
  
  if (any(grep("mg",Cont.Data$Units))) {
  
    Cont.Data$Units[grep("mg",Cont.Data$Units)] <- "mg/l"
    Cont.Data$Result.Corr.ND[Cont.Data$Units == "mg/l"] <- 1000*Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l"]
    Cont.Data$Result <- as.character(Cont.Data$Result)
    Cont.Data$Result[Cont.Data$Units == "mg/l" & !Cont.Data$ND]<-as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l" & !Cont.Data$ND])
    Cont.Data$Result[Cont.Data$Units == "mg/l" & Cont.Data$ND]<-paste("ND<",as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l" & Cont.Data$ND]),sep="")
    Cont.Data$Result <- factor(as.character(Cont.Data$Result))
    Cont.Data$Units[Cont.Data$Units == "mg/l"] <- "ug/l"
    
  }
  
  if (any(grep("ng",Cont.Data$Units))) {
  
    Cont.Data$Units[grep("ng",Cont.Data$Units)]<-"ng/l"
    Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l"]<-0.001*Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l"]
    Cont.Data$Result<-as.character(Cont.Data$Result)
    Cont.Data$Result[Cont.Data$Units=="ng/l" & !Cont.Data$ND]<-as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l" & !Cont.Data$ND])
    Cont.Data$Result[Cont.Data$Units=="ng/l" & Cont.Data$ND]<-paste("ND<",as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l" & Cont.Data$ND]),sep="")
    Cont.Data$Result<-factor(as.character(Cont.Data$Result))
    Cont.Data$Units[Cont.Data$Units == "ng/l"] <- "ug/l"
    
  }
  
  
  Cont.Data$Units <- factor(as.character(Cont.Data$Units))
  
  
  
  ############## NAPL Handling #################################################
  # Substituted with max observed value on a cont by cont basis 
  
  if ("napl" %in% tolower(as.character(Cont.Data$Constituent))) {
  
    NAPL.Thickness.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent)) == "napl",]
  
  
    NAPL.Units <- unique(tolower(as.character(NAPL.Thickness.Data$Units)))
    
    if (length(NAPL.Units) > 1) {
      
      msg = "Multiple units detected for NAPL thickness in input dataset. Please ensure same thickness units are used throughout."
      showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
      return(NULL)
    }
    
    if (length(NAPL.Units) > 0) {
      
      if (!NAPL.Units %in% c("level","mm","cm","metres","inches","feet")) {
        
        msg = "NAPL thickness units must be one of 'level', 'mm', 'cm', 'metres', 'inches' or 'feet'.\n\nPlease correct and re-run GWSDAT analysis."
        showModal(modalDialog(title = "Error", msg))
        return(NULL)
      }
    }
    
   
    
    NAPL.Thickness.Data <- try(NAPL.Thickness.Data[order(NAPL.Thickness.Data$SampleDate),])
    NAPL.Thickness.Data[,c("XCoord","YCoord")] <- well_tmp_data[match(as.character(NAPL.Thickness.Data$WellName),as.character(well_tmp_data$WellName)),c("XCoord","YCoord")]

    
    
    
    
    #msg <- "Do you wish to substitute NAPL values with maximum observed solute concentrations? \nNote: NAPL measurements for electron acceptor, Redox or 'NotInNapl' flagged constituents will be ignored."
    #msg <- paste(msg, "\n(Currently only Yes choice is possible, changing to Yes/No soon.", sep = "")
    
    subst_napl_vals <- "yes"
    msg <- "NAPL values are substituted with maximum observed solute concentrations (Yes/No choice will soon be supported). \nNote: NAPL measurements for electron acceptor, Redox or 'NotInNapl' flagged constituents will be ignored."
    showNotification(msg, type = "warning", duration = 10)  
    
    
    if (ContTypeData == "NoConcData" || subst_napl_vals == "yes") {
      
      
      
      cont_names.No.NAPL <- cont_names[tolower(cont_names)!="napl"]
      cont_names.No.NAPL <- setdiff(cont_names.No.NAPL,ElecAccepts) #omit e-acc constituent from NAPL set
      
      
      ############################# NAPL and dissolved conflict resolution ##########################################
      NAPLWellandDates <- unique(Cont.Data[tolower(Cont.Data$Constituent)=="napl",c("WellName","SampleDate","Constituent"),drop=FALSE])[,1:2]
      NonNAPLWellandDates <- unique(Cont.Data[tolower(Cont.Data$Constituent)!="napl" & !is.na(Cont.Data$Result.Corr.ND),c("WellName","SampleDate","Constituent"),drop=FALSE])[,1:2]
      NAPLConflictDateandWells <- NonNAPLWellandDates[which(apply(NonNAPLWellandDates,1,paste,collapse="") %in% apply(NAPLWellandDates,1,paste,collapse="")),]
      NAPLConflictDateandWells <- unique(NAPLConflictDateandWells); 
      
      if (nrow(NAPLConflictDateandWells) > 0) {
        
        if (all(Cont.Data[apply(Cont.Data[,c("WellName","SampleDate")],1,paste,collapse = "") %in% apply(NAPLConflictDateandWells,1,paste,collapse="") 
                          & tolower(Cont.Data$Constituent) == "napl",]$Result.Corr.ND == 0)) {

          myans <- "yes"

        } else {

          myans <- "yes"

          # "NAPL Data Conflict"        
          # msg <- "Concentration data reported in presence of NAPL. Do you wish to use concentration data (Yes) or substitue these NAPL values with maximum observed solute concentrations (No)?\nNote: NAPL measurements for electron acceptor, Redox or 'NotInNapl' flagged constituents will be ignored."
          msg <- "Concentration data reported in presence of NAPL. Use it? (Yes/No choice will soon be supported)"
          showNotification(msg, type = "warning", duration = 10)  
        }
        
        if (myans == "no") {
          
  	      for (i in 1:nrow(NAPLConflictDateandWells)){
            Cont.Data <- Cont.Data[-which(as.character(Cont.Data$WellName) == as.character(NAPLConflictDateandWells$WellName)[i] & 
                                            Cont.Data$SampleDate == NAPLConflictDateandWells$SampleDate[i] & tolower(as.character(Cont.Data$Constituent)) != "napl"),]
          }
          
  	    } else {
                    
          for (i in 1:nrow(NAPLConflictDateandWells)){
            Cont.Data <- Cont.Data[-which(as.character(Cont.Data$WellName) == as.character(NAPLConflictDateandWells$WellName)[i] & 
                                            Cont.Data$SampleDate == NAPLConflictDateandWells$SampleDate[i] & tolower(as.character(Cont.Data$Constituent)) == "napl"),]
          }
    	  }
      }
      
      #-------------------------------------------------------------------------------------------------------------#
      
      NAPL.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent))=="napl",]
      No.NAPL.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent))!="napl",]
      No.NAPL.Data$Constituent <- factor(as.character(No.NAPL.Data$Constituent))
      No.NAPL.Data <- No.NAPL.Data[,c("WellName","Constituent","SampleDate","Result","Units","ND","Result.Corr.ND")]
      
      
      
      New.NAPL.Data <- data.frame(WellName = 
                                  rep(NAPL.Data$WellName, length(cont_names.No.NAPL)),
                                  Constituent = rep(cont_names.No.NAPL,each=nrow(NAPL.Data)),
  	                              SampleDate = rep(NAPL.Data$SampleDate,length(cont_names.No.NAPL))
                        )
      
      New.NAPL.Data$Result=rep("NAPL",nrow(New.NAPL.Data))
      New.NAPL.Data$Units=rep(NAPL.Units,nrow(New.NAPL.Data)); 
      New.NAPL.Data$ND = rep(FALSE,nrow(New.NAPL.Data))
      New.NAPL.Data$Result.Corr.ND <- tapply(No.NAPL.Data$Result.Corr.ND,No.NAPL.Data$Constituent,max,na.rm=T)[as.character(New.NAPL.Data$Const)]
      
      
      Cont.Data <- rbind(No.NAPL.Data,New.NAPL.Data)
      cont_names <- unique(as.character(Cont.Data$Constituent))
      
      
    } else {
  
      cont_names.No.NAPL <- cont_names[tolower(cont_names) != "napl"]
      NAPL.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent)) == "napl",]
      No.NAPL.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent)) != "napl",]
      No.NAPL.Data$Constituent <- factor(as.character(No.NAPL.Data$Constituent))
      
      Cont.Data <- No.NAPL.Data
      cont_names <- unique(as.character(Cont.Data$Constituent))
      
    }
  }
  
  
  ##################################### ND correction Handling ##############################################
  
  if (GWSDAT_Options$NDMethod == "Half of ND Value"){Cont.Data$Result.Corr.ND[Cont.Data$ND]<-0.5*Cont.Data$Result.Corr.ND[Cont.Data$ND]}
  Cont.Data[,c("XCoord","YCoord")] <- well_tmp_data[match(as.character(Cont.Data$WellName),as.character(well_tmp_data$WellName)),c("XCoord","YCoord")]
  
  
  
  ####################### Groundwater Data ###############################################################
  
  GW.Data <- solute_data[tolower(as.character(solute_data$Constituent)) == "gw",]
  GW.Units <- unique(tolower(as.character(GW.Data$Units)))

  
  if (length(GW.Units) > 1) {

    # "Units Error"
    msg <- "Multiple units detected for GroundWater elevation in input dataset. \nPlease ensure same elevation units are used throughout."
    showModal(modalDialog(title = "Units Error", msg))
    return(NULL)
  }
  
  
  if (length(GW.Units) > 0) {
    if (!GW.Units %in% c("level","mm","cm","metres","inches","feet")) {
      
      msg <- "GroundWater elevation units must be one of 'level', 'mm', 'cm', 'metres', 'inches' or 'feet'.\n\nPlease correct and re-run GWSDAT analysis."
      showModal(modalDialog(title = "Units Error", msg))
      return(NULL)
    }
  }
  
  GW.Data$Result <- as.numeric(as.character(GW.Data$Result))
  GW.Data <- GW.Data[!is.na(GW.Data$Result),]
  GW.Data[,c("XCoord","YCoord")] <- well_tmp_data[match(as.character(GW.Data$WellName),as.character(well_tmp_data$WellName)),c("XCoord","YCoord")]

  tryCatch(
    agg_data <- aggregateData(Cont.Data, GW.Data, 
                              NAPL.Thickness.Data = if (exists("NAPL.Thickness.Data")) { NAPL.Thickness.Data } else {NULL},
                              well_tmp_data,
                              GWSDAT_Options$Aggby, 
                              GWSDAT_Options$AggMethod 
    ), error = function(e) {
      showModal(modalDialog(title = "Error", paste0("Failed to aggregate data: ", e$message), easyClose = FALSE))
      return(NULL)                      
  })
  
  sample_loc$data  <- well_tmp_data
  sample_loc$names <- sample_loc_names
  sample_loc$area  <- splancs::areapl(as.matrix(well_tmp_data[chull(well_tmp_data[,c("XCoord","YCoord")]),c("XCoord","YCoord")]))


  #
  # This list is way to big!! Make it slimmer and more structured.
  #
  
  All.Data <- list(GW.Data = GW.Data,
                 Agg_GW_Data = agg_data$Agg_GW_Data,
                 NAPL.Thickness.Data = agg_data$NAPL.Thickness.Data,
                 Cont.Data  = agg_data$Cont.Data,
                 All_Agg_Dates = agg_data$All_Agg_Dates,
                 cont_names = cont_names,
                 GW.Units = GW.Units,
                 NAPL.Units = if (exists("NAPL.Units")) { NAPL.Units } else {NULL}, 
                 ElecAccepts = ElecAccepts,
                 shape_data  = shape_file_data,
                 sample_loc = sample_loc
                 )
  
  return(All.Data)
}
